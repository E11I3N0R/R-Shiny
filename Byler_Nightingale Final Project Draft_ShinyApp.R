# LIBRARIES
library(shiny) # for Shiny app.
library(readxl) # for loading dataset from Excel file.
library(magrittr) # adds operators that make function sequencing easier.
library(tidyverse) # for data manipulation and visualization.
library(stringr) # for text processing.
library(lubridate) # for date-time processing.

# LOAD DATA
Nightingale <- read_excel("DATASET_Nightingale.xlsx", sheet = "Nightingale")

# PREP DATA
Nightingale %>%
  select(-c("ID")) %>%
  pivot_longer(cols = c("Disease", "Wounds", "Other"), names_to = c("Cause"), values_to = c("Deaths")) %>%
  mutate(Rate = if_else(.$Cause == "Disease", .$Disease.rate, if_else(.$Cause == "Wounds", .$Wounds.rate, .$Other.rate))) %>%
  select(-c("Disease.rate", "Wounds.rate", "Other.rate")) %>%
  unite("Month_Year", Month:Year, sep=" ", remove=FALSE) %>%
  mutate(Regime = if_else(str_detect(.$Month_Year, "[:alpha:]\\s1854"), "Before", if_else(str_detect(.$Month_Year, "(Jan|Feb|Mar)\\s1855"), "Before", "After"))) %>%
  mutate(Regime = factor(Regime, levels = c("Before", "After"))) %>%
  mutate(mo = month(Date, label = TRUE, abbr = TRUE)) -> long_data


# APP UI
ui <- fluidPage(
  # colors include 78c2ad (mint), f3969a (pale brick), 6cc3d5 (pale blue)
  # 6cc3d5 (bright mint), ffce67 (yellow), ff7851 (orange), 7C8083 (gray)
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&family=Playball&family=Spectral:ital@1&display=swap');
      body {
        font-family: 'Lato'
      }
      h1 {
        font-family: 'Playball'
      }
      h3 {
        font-family: 'Spectral', serif
      }
      .shiny-input-container {
      }"))
  ),
  
  titlePanel(h1("Florence Nightingale: Mother of Data Viz")),
  
  navlistPanel(
    
    # INTRO SECTION
    "Introduction",
    
    # ABOUT FLORENCE SECTION
    tabPanel("About Florence Nightingale",
      h3("About Florence Nightingale"),
      p("This is some text explaining Florence Nightingale's work.")
    ),
    
    # PLACEHOLDER SECTION
    "Example Chart",
    tabPanel("Slider Example",
      h3("Slider Example"),
      sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1, max = 100),
      plotOutput("hist")
    ),
    
    # ROSE DIAGRAM SECTION
    "Rose Diagram",
    
    # ORIGINAL ROSE DIAGRAM
    tabPanel("Original",
       h3("Original Rose Diagram"),
       img(src = "Rose_Diagram.jpg", height = 420, width = 768),
       br(),
       br(),
       div(
         p(tags$b("DIAGRAM of the CAUSES of MORTALITY in the ARMY in the EAST")),
         p("Figure 1: APRIL 1854 to MARCH 1855"),
         p("Figure 2: APRIL 1855 to MARCH 1856"),
         tags$ul(
           tags$li("The areas of the ",
            span("blue", style = "color:SteelBlue", .noWS = "after"),
            ", ",
            span("red", style = "color:PaleVioletRed", .noWS = "after"),
            ", & ",
            span("black ", style = "color:LightSlateGray"),
            "wedges are each measured from the center as the common vortex."),
           tags$li("The ",
             span("blue ", style = "color:SteelBlue"),
             "wedges measured from the center of the circle represent area for area the deaths from ",
             span("Preventable or Mitigable Zymotic diseases", style = "color:SteelBlue", .noWS = "after"),
             ", the ",
             span("red ", style = "color:PaleVioletRed"),
             "wedges measured from the center the deaths from ",
             span("wounds", style="color:PaleVioletRed", .noWS = "after"),
             ", & the ",
             span("black ", style = "color:LightSlateGray"),
             "wedges measured from the center the deaths from ",
             span("all other causes", style = "color:LightSlateGray", .noWS = "after"),
             "."),
           tags$li("The ",
             span("black ", style = "color:LightSlateGray"),
             "line across the ",
             span("red ", style = "color:PaleVioletRed"),
             "triangle in Nov. 1854 marks the boundary of the deaths from ",
             span("all other causes ", style = "color:LightSlateGray"),
             "during the month."),
           tags$li("In October 1854 & April 1855, the ",
             span("black ", style = "color:LightSlateGray"),
             "area coincides with the ",
             span("red", style = "color:PaleVioletRed", .noWS = "after"),
             ", in January & February 1856, the ",
             span("blue ", style = "color:SteelBlue"),
             "coincides with the ",
             span("black", style = "color:LightSlateGray", .noWS = "after"),
             "."),
           tags$li("The entire areas may be compared by following the ",
             span("blue", style = "color:SteelBlue", .noWS = "after"),
             ", the ",
             span("red", style = "color:PaleVioletRed", .noWS = "after"),
             ", & the ",
             span("black ", style = "color:LightSlateGray"),
             "lines enclosing them."
           )
         ),
         em("—Florence Nightingale, Contribution to the Sanitary History of the British Army (1859)")
       ),
    
    ),
    
    # RECREATION ROSE DIAGRAM
    tabPanel("Recreation",
      h3("Modern Recreation with R"),
      plotOutput("rose")
    ),
    
    "-----",
    
    # SOURCES TAB
    tabPanel("Sources",
      h3("Sources:"),
      tags$ul(
        tags$li(tags$a(href="https://github.com/vincentarelbundock/Rdatasets/blob/master/csv/HistData/Nightingale.csv", "Nightingale Dataset")),
        tags$li(tags$a(href="http://understandinguncertainty.org/node/214", "Mathematics of the Coxcombs")),
        tags$li(tags$a(href="https://rpubs.com/chidungkt/819554", "Florence Nightingale’s Rose Diagram"))
      )
    )
  ),
  

)

# APP SERVER
server <- function(input, output) {
  
  # EXAMPLE CHART
  output$hist <- renderPlot({
    title <- "Example Chart"
    hist(rnorm(input$num), main = title)
    })
  
  # ROSE DIAGRAM
  output$rose <- renderPlot({
    title <- "Rose Diagram"
    long_data %>% 
      ggplot(aes(x = mo, y = Deaths, fill = Cause)) +
      geom_col(color = "grey20") + 
      scale_fill_manual(values = c("#6cc3d5", "#7C8083", "#f3969a"), name = "") +
      scale_y_sqrt() +
      facet_wrap(~ Regime) + 
      coord_equal(ratio = 1) +
      coord_polar() +
      labs(title = "Causes of Mortality in the Army in the East", 
           subtitle = "BEFORE and AFTER March 1855",
           caption = "Data Source: Deaths from various causes in the Crimean War") +
      theme(legend.position = "top") + 
      theme(text = element_text(size = 14)) + 
      theme(axis.title.y = element_blank()) + 
      theme(axis.title.x = element_blank()) + 
      theme(axis.text.y = element_blank()) + 
      theme(axis.ticks = element_blank()) + 
      theme(plot.margin = unit(rep(0.7, 4), "cm")) + 
      theme(plot.title = element_text(color = "#78c2ad", size = 20, face="bold")) + 
      theme(plot.caption = element_text(color = "grey70", size = 11)) + 
      theme(plot.subtitle = element_text(color = "#7C8083", size = 13, face="bold")) +
      theme(legend.text = element_text(color = "black", size = 12)) + 
      theme(strip.text = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))
  },
  width = 682,
  height = 528)
}

shinyApp(ui = ui, server = server)