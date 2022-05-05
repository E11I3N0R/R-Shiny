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
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&family=Playball&family=Spectral:ital@1&display=swap');
      body {
        font-family: 'Lato';
        background-color: white;
        color: black;
      }
      h1 {
        font-family: 'Playball';
      }
      h3 {
        font-family: 'Spectral', serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
  ),
  
  titlePanel(h1("Florence Nightingale: Mother of Data Viz")),
  
  sidebarLayout(position = "right",
    sidebarPanel(tags$b("Sources:"),
      tags$ul(
        tags$li(tags$a(href="https://github.com/vincentarelbundock/Rdatasets/blob/master/csv/HistData/Nightingale.csv", "Nightingale Dataset")),
        tags$li(tags$a(href="http://understandinguncertainty.org/node/214", "Mathematics of the Coxcombs")),
        tags$li(tags$a(href="https://rpubs.com/chidungkt/819554", "Florence Nightingale’s Rose Diagram"))
      )
    ),
    
    mainPanel(h3("main panel"),
      sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1, max = 100),
      plotOutput("hist"),
      br(),
      hr(),
      br(),
      h3("Original Rose Diagram"),
      img(src = "Rose_Diagram.jpg", height = 350, width = 640),
      div(
        tags$blockquote(
          p("DIAGRAM of the CAUSES of MORTALITY in the ARMY in the EAST"),
          p("Figure 1: APRIL 1854 to MARCH 1855"),  
          p("Figure 2: APRIL 1855 to MARCH 1856"),
          tags$ul(
            tags$li("The areas of the ",
              span("blue", style = "color:blue", .noWS = "after"),
              ", ",
              span("red", style = "color:red", .noWS = "after"),
              ", & ",
              span("black ", style = "color:gray"),
              "wedges are each measured from the center as the common vortex."),
            tags$li("The ",
              span("blue ", style = "color:blue"),
              "wedges measured from the center of the circle represent area for area the deaths from ",
              span("Preventable or Mitigable Zymotic diseases", style = "color:blue", .noWS = "after"),
              ", the ",
              span("red ", style = "color:red"),
              "wedges measured from the center the deaths from ",
              span("wounds", style="color:red", .noWS = "after"),
              ", & the ",
              span("black ", style = "color:gray"),
              "wedges measured from the center the deaths from ",
              span("all other causes", style = "color:gray", .noWS = "after"),
              "."),
            tags$li("The ",
              span("black ", style = "color:gray"),
              "line across the ",
              span("red ", style = "color:red"),
              "triangle in Nov. 1854 marks the boundary of the deaths from ",
              span("all other causes ", style = "color:gray"),
              "during the month."),
            tags$li("In October 1854 & April 1855, the ",
              span("black ", style = "color:gray"),
              "area coincides with the ",
              span("red", style = "color:red", .noWS = "after"),
              ", in January & February 1856, the ",
              span("blue ", style = "color:blue"),
              "coincides with the ",
              span("black", style = "color:gray", .noWS = "after"),
              "."),
            tags$li("The entire areas may be compared by following the ",
              span("blue", style = "color:blue", .noWS = "after"),
              ", the ",
              span("red", style = "color:red", .noWS = "after"),
              ", & the ",
              span("black ", style = "color:gray"),
              "lines enclosing them."
              )
            ),
          br(),
          em("—Florence Nightingale")
        )
      ),
      br(),
      plotOutput("rose")
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
      ggplot(aes(x = mo, y = Deaths, fill = Cause))+
      geom_col(color = "grey20") + 
      scale_fill_manual(values = c("firebrick", "orange", "#365181"), name = "") +
      scale_y_sqrt() +
      facet_wrap(~ Regime) + 
      coord_equal(ratio = 1) +  
      coord_polar() +
      labs(title = "Causes of Mortality in the Army in the East", 
           subtitle = "Created by R Statistical Software", 
           caption = "Data Source: Deaths from various causes in the Crimean War") + 
      theme(legend.position = "top") + 
      theme(text = element_text(size = 14)) + 
      theme(axis.title.y = element_blank()) + 
      theme(axis.title.x = element_blank()) + 
      theme(axis.text.y = element_blank()) + 
      theme(axis.ticks = element_blank()) + 
      theme(plot.margin = unit(rep(0.7, 4), "cm")) + 
      theme(plot.title = element_text(color = "black", size = 20)) + 
      theme(plot.caption = element_text(color = "grey70", size = 11)) + 
      theme(plot.subtitle = element_text(color = "grey70", size = 13)) +
      theme(legend.text = element_text(color = "black", size = 12)) + 
      theme(strip.text = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)