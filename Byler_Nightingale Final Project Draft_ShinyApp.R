# LIBRARIES
library(shiny) # for Shiny app.
library(readxl) # for loading dataset from Excel file.
library(magrittr) # adds operators that make function sequencing easier.
library(tidyverse) # for data manipulation and visualization.
library(stringr) # for text processing.
library(lubridate) # for date-time processing.
library(slickR) # for image slideshow.
library(DT) # for data table formatting.


# FUNCTION TO COLOR DATA TABLE
rowCallback <- c(
  "function(row, data, num, index){",
  "  var $row = $(row);",
  "  if($row.hasClass('even')){",
  "    $row.css('background-color', '#D1F0E4');",
  "    $row.hover(function(){",
  "      $(this).css('background-color', '#C4E3D7');",
  "     }, function(){",
  "      $(this).css('background-color', '#D1F0E4');",
  "     }",
  "    );",
  "  }else{",
  "    $row.css('background-color', 'white');",
  "    $row.hover(function(){",
  "      $(this).css('background-color', '#FAFBFC');",
  "     }, function(){",
  "      $(this).css('background-color', 'white');",
  "     }",
  "    );",  
  "  }",
  "}"  
)

# LOAD DATA
Nightingale <- read_excel("DATASET_Nightingale.xlsx", sheet = "Nightingale")

# PREP DATA FOR ANALYSIS
Nightingale %>%
  select(-c("ID")) %>%
  pivot_longer(cols = c("Disease", "Wounds", "Other"), names_to = c("Cause"), values_to = c("Deaths")) %>%
  mutate(Rate = if_else(.$Cause == "Disease", .$Disease.rate, if_else(.$Cause == "Wounds", .$Wounds.rate, .$Other.rate))) %>%
  select(-c("Disease.rate", "Wounds.rate", "Other.rate")) %>%
  mutate(Rate = round((Rate/12), 2)) %>%
  unite("Month_Year", Month:Year, sep=" ", remove=FALSE) %>%
  mutate(Regime = if_else(str_detect(.$Month_Year, "[:alpha:]\\s1854"), "Before", if_else(str_detect(.$Month_Year, "(Jan|Feb|Mar)\\s1855"), "Before", "After"))) %>%
  mutate(Regime = factor(Regime, levels = c("Before", "After"))) %>%
  mutate(Sort_Date = as.yearmon(as.Date(Date))) %>%
  mutate(mo = month(Date, label = TRUE, abbr = TRUE)) -> long_data

# CREATE SUBSET FOR COMBINED ROSE DIAGRAM
long_data %>%
  select(-c("Date", "Month_Year", "Month", "Year", "Army", "Rate", "mo")) %>%
  subset(Cause == "Disease") -> sub_long


# REMOVE EXTRA COLUMNS FOR DISPLAY IN TABLE IN APP
Nightingale %>%
  select(-c("ID", "Date")) -> display_nightingale

long_data %>%
  select(-c("Date", "Month_Year", "mo", "Sort_Date")) -> display_long_data

# APP UI
ui <- fluidPage(
  # colors include 78c2ad (mint), f3969a (pale brick), 6cc3d5 (pale blue)
  # 4b8c99 (dark blue), 56CC9D (bright mint), ffce67 (yellow), ff7851 (orange), 7C8083 (gray), 
  # 92DEC0 (light bright mint), FAFBFC (pale gray), C4E3D7 (dark pale mint), D1F0E4 (light pale mint)
  # F8766D (red for line chart), 00BFC4 (blue for line chart)
  
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&family=Playball&family=Spectral:ital@1&display=swap');
      body {
        font-family: 'Lato';
        font-size: 16px;
      }
      h1 {
        font-family: 'Playball'
      }
      h3 {
        font-family: 'Spectral', serif
      }
      h4 {
        font-family: 'Lato';
        font-size: 18px;
        font-weight: bold;
        color: #f3969a
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
      br(),
      
      # IMAGE SLIDESHOW
      fluidRow(
        column(5,
          p(span(tags$b("Florence Nightingale"), style = "color:#f3969a"),
            "(1820-1910) was an incredible woman. Not only did",
            "she revolutionize medical care by setting the standards for modern nursing,",
            "she also was a pioneer in the field of statistics and especially data visualization.",
            "Working at a military hospital during the Crimean War, Nightingale was struck",
            "by the deplorable conditions for wounded soldiers. She suspected that many of them",
            "were dying from diseases that would be preventable with proper hygiene protocols.",
            "Determined to convince the British government that something had to change,",
            "Nightingale collected data and drew up charts to accompany her report.",
            "Data visualization was still novel at the time, and Nightingale's charts allowed her",
            "to proscribe a solution instead of merely describing the problem. She became the first",
            "female member elected to the Royal Statistical Society in 1859.")
        ),
        column(1),
        column(5,
          slickROutput("slickr", width="auto", height="auto")),
        column(1)
      ),
      
      # VOICE RECORDING
      h4("Florence Nightingale's Voice"),
      p(em("Recorded to wax cylinder on July 30, 1890, to raise money for veterans of the Charge of the Light Brigade.")),
      tags$audio(src = "Florence_Nightingale_voice.ogg", type = "audio/ogg", controls = NA),
      tags$blockquote(em('"When I am no longer even a memory, just a name, I hope my voice may perpetuate the great work of my life. God bless my dear old comrades of Balaclava and bring them safe to shore. Florence Nightingale."')),
      br(),
      
      # VIDEOS
      h4("What would Florence Nightingale make of big data? | BBC Ideas"),
      p(em("Best overview of Florence's career in stats and data viz.")),
      tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/sNppKQh0xPo", 
                  title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; 
                  clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
      
      h4("Florence Nightingale: Changing the Field of Nursing - Fast Facts | History Channel"),
      p(em("General info on her life and her nursing career.")),
      tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/B94Zf4Vye3Y", 
                  title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; 
                  clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
      
      h4("Florence Nightingale Revolutionizes Nursing (feat. Minka Kelly) - Drunk History"),
      p(em("Video won't embed, but you can follow the link below to watch on YouTube.")),
      tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/bOGSRJ1_7DI", 
                  title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; 
                  clipboard-write; encrypted-media; gyroscope; picture-in-picture")
    ),
    
    
    # DATASET SECTION
    "Dataset",
    
    # ORIGINAL DATASET
    tabPanel("Original Dataset",
      withMathJax(),
      h3("Original Dataset"),
      tags$ul(
        tags$li(tags$b("Dataset tracks deaths each month for British soldiers in the Crimean War.")),
        tags$li("ARMY represents average size of army in that month."),
        tags$li("RATES are calculated as",
          span("annual rate of mortality", style = "color:#f3969a"),
          "per 1,000 in each month, e.g., ",
          "the proportion of soliders (S) who died (D) out of every 1,000 soldiers, replicated throughout the year. ",
          span("\\(\\frac{12,000D}{S}\\)", style = "color:#78c2ad")),
        tags$li('Deaths from disease are what Florence Nightingale considered',
          span('"preventable deaths"', style = "color:#f3969a"))
      ),
      DTOutput("NightingaleDataset")
    ),
    
    # CLEANED DATASET
    tabPanel("Cleaned Dataset",
      h3("Cleaned Dataset"),
        tags$ul(
          tags$li(tags$b("Table has been pivoted and undergone data prep.")),
          tags$li('REGIME reflects "Before" and "After"',
            span('March 1855', style = "color:#f3969a", .noWS = "after"),
            ', which is when the Sanitary Commission arrived.'),
          tags$li("RATE is calculated as",
            span("monthly mortality", style = "color:#f3969a"),
            "per 1,000, e.g., the proportion of soliders (S) who died (D) each month out of every 1,000 soldiers. ",
            span("\\(\\frac{1,000D}{S}\\)", style = "color:#78c2ad")),
        ),
      DTOutput("LongDataset")
    ),
    
    
    # ROSE DIAGRAM SECTION
    "Rose Diagram",
    
    # EXPLANATION OF ROSE DIAGRAM (VIDEO)
    tabPanel("Explanation of Rose Diagram (video)",
      h3("Explaining the Rose Diagram"),
      tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/u6XqiDccroM", 
                  title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; 
                  clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
      p("Source:",
        tags$a(href="http://www.florence-nightingale-avenging-angel.co.uk/blog/?page_id=462",
                     "Florence Nightingale's Forgotten Legacy: Public Health laws"))
    ),
    
    # ORIGINAL ROSE DIAGRAM
    tabPanel("Original Rose Diagram",
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
          em("Florence Nightingale, Contribution to the Sanitary History of the British Army (1859)")
        ),
    ),
    
    # MODERN RECREATION ROSE DIAGRAM
    tabPanel("Modern Rose Diagram",
      h3("Modern Recreation with R"),
      plotOutput("rose")
    ),
    
    # COMBINED ROSE DIAGRAM
    tabPanel("Combined Rose Diagram",
      h3("Combined Rose Diagram"),
      plotOutput("combo")
    ),
    
    # MODERN CHART SECTION
    "Modern Charts",
    
    # PREVENTABLE DEATH RATE BY YEAR (LINE)
    tabPanel("Preventable Death Rate by Year",
      h3("Preventable Death Rate by Year"),
      h4("Notes:"),
      tags$ul(
        tags$li(tags$b("Includes only deaths from preventable causes, i.e., disease.")),
        tags$li("REGIME reflects",
          span(tags$b("Before"), style= "color:#6cc3d5"),
          "and",
          span(tags$b("After"), style= "color:#4b8c99"),
          "March 1855, which is when the Sanitary Commission arrived."),
        tags$li("RATE is calculated as monthly mortality per 1,000 soldiers."),
        tags$li('Deaths from disease are what Florence Nightingale considered "preventable".')
      ),
      br(),
      plotOutput("disease_by_year")
    ),
    
    # DEATH RATE BY CAUSE: BEFORE & AFTER (BAR)
    tabPanel("Death Rate by Cause: Before & After",
      h3("Death Rate by Cause: Before & After"),
      h4("Notes:"),
      tags$ul(
        tags$li(tags$b("Includes deaths from disease, wounds, and all other causes.")),
        tags$li('REGIME reflects "Before" and "After" March 1855, which is when the Sanitary Commission arrived.'),
        tags$li("RATE is calculated as monthly mortality per 1,000 soldiers.")
      ),
      br(),
      plotOutput("rates_by_cause")
    ),
    
    
    # CONCLUSION SECTION
    "Conclusion",
    
    # SOURCES & MORE INFO TAB
    tabPanel("Sources & More Info",
      fluidRow(
        column(6,
          
          # SOURCES
          h3("Sources:"),
            tags$ul(
              tags$li(tags$a(href="https://github.com/vincentarelbundock/Rdatasets/blob/master/csv/HistData/Nightingale.csv", 
                "Nightingale Dataset")),
              "Original dataset from Github.",
              
              tags$li(tags$a(href="http://understandinguncertainty.org/node/214", 
                "Mathematics of the Coxcombs")),
              'Explains how rose/coxcomb/polar area charts work, as well as how Florence calculated death rate as "annual rate of mortality per 1000 in each month".',
              
              tags$li(tags$a(href="https://rpubs.com/chidungkt/819554", 
                "Florence Nightingale's Rose Diagram")),
              "Inspiration and code base for modern recreation of rose diagram.",
              
              tags$li(tags$a(href="https://en.wikipedia.org/wiki/Florence_Nightingale", 
                "Wikipedia: Florence Nightingale")),
              "Source of Florence Nightingale portraits and sound recording.",
              
              tags$li(tags$a(href="https://bootswatch.com/minty/", 
                "Bootswatch Themes: Minty")),
              "Theme for Shiny app.",
              
              tags$li(tags$a(href="https://www.r-bloggers.com/2013/01/going-beyond-florence-nightingales-data-diagram-did-flo-blow-it-with-wedges/", 
                "Going Beyond Florence Nightingale's Data Diagram: Did Flo Blow It with Wedges?")),
              "Inspiration and code base for combined rose diagram.",
              
              tags$li(tags$a(href="http://www.florence-nightingale-avenging-angel.co.uk/blog/?page_id=462",
                "Florence Nightingale's Forgotten Legacy: Public Health laws")),
              "Historian explains the rose diagram."
            )
        ),
        
        # MORE INFO
        column(6,
          h3("More Info:"),
            tags$ul(
              tags$li(tags$a(href="https://www.atlasobscura.com/articles/florence-nightingale-infographic", 
                "Florence Nightingale Was Born 197 Years Ago, and Her Infographics Were Better Than Most of the Internet's | Atlas Obscura")),
              
              tags$li(tags$a(href="https://en.wikipedia.org/wiki/Florence_Nightingale", 
                "Wikipedia: Florence Nightingale")),
              
              tags$li(tags$a(href="https://en.wikipedia.org/wiki/Crimean_War", 
                "Wikipedia: Crimean War")),
              
              tags$li(tags$a(href="https://curiosity.lib.harvard.edu/contagion/catalog/36-990101646750203941",
                "Florence Nightingale's Full Report (1859)"))
            )
        )
      ),
      
      br(),
      img(src = "drunk_history.png", height=387, width=690),
      p(em("Florence using data to convince Queen Victoria. Source: Drunk History (Comedy Central).")),
    )
  ),
  

)

# APP SERVER
server <- function(input, output) {
  
  # IMAGE SLIDESHOW
  output$slickr <- renderSlickR({
    imgs <- c("Florence1.jpg", "Florence2.jpg", "Florence3.jpg", "Florence4.jpg", "Florence5.jpg", "Florence6.jpg", "Florence7.jpg", "Florence8.jpg", "Florence9.jpg")
    slickR(imgs)
  })
  
  
  # DATASET
  output$NightingaleDataset <- renderDT(
    datatable(display_nightingale, options = list(rowCallback = JS(rowCallback)))
  )
  
  output$LongDataset <- renderDT(
    datatable(display_long_data, options = list(rowCallback = JS(rowCallback)))
  )
  
  
  # MODERN ROSE DIAGRAM
  output$rose <- renderPlot({
    title <- "Rose Diagram"
    long_data %>% 
      ggplot(aes(x = mo, y = Deaths, fill = Cause)) +
      geom_col(color = "grey20") + 
      scale_fill_manual(values = c("#6cc3d5", "#7C8083", "#f3969a"), name = "") +
      scale_y_sqrt() +
      facet_wrap(~ Regime) + 
      # A coxcomb plot/rose diagram = bar chart + polar coordinates
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
      theme(plot.caption = element_text(color = "grey70", size = 12)) + 
      theme(plot.subtitle = element_text(color = "#7C8083", size = 13, face="bold")) +
      theme(legend.text = element_text(color = "black", size = 12)) + 
      theme(strip.text = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))
  },
  width = 682,
  height = 528)
  

  # COMBINED ROSE DIAGRAM: Before and After
  output$combo <- renderPlot({
    title <- "Combined Rose Diagram"
    sub_long %>% 
      ggplot(aes(x = factor(Sort_Date), y = Deaths, fill = Regime)) +
      geom_col(color = "grey20") + 
      scale_fill_manual(values = c("#6cc3d5", "#4b8c99"), name = "") +
      scale_y_sqrt() +
      # A coxcomb plot/rose diagram = bar chart + polar coordinates
      coord_equal(ratio = 1) +
      coord_polar() +
      labs(title = "Deaths from Preventable Causes: Apr 1854 - Mar 1856", 
           subtitle = "BEFORE and AFTER March 1855",
           caption = "Data Source: Deaths from various causes in the Crimean War") +
      theme(legend.position = "top") + 
      theme(text = element_text(size = 14)) + 
      theme(axis.title.x = element_blank()) + 
      theme(axis.title.y = element_blank()) + 
      theme(axis.text.y = element_blank()) + 
      theme(axis.ticks = element_blank()) + 
      theme(plot.margin = unit(rep(0.7, 4), "cm")) + 
      theme(plot.title = element_text(color = "#78c2ad", size = 20, face="bold")) + 
      theme(plot.caption = element_text(color = "grey70", size = 12)) + 
      theme(plot.subtitle = element_text(color = "#7C8083", size = 13, face="bold")) +
      theme(legend.text = element_text(color = "black", size = 12)) 
  },
  width = 800,
  height = 700)

  
  # MODERN CHART: Preventable Death Rate by Year
  output$disease_by_year <- renderPlot({
    title <- "disease_by_year"
    sub_long <- subset(long_data, Cause == "Disease")
    sub_long %>%
      ggplot(aes(x=mo, y=Rate, group=Year, color=Regime)) +
      geom_line(size = 2) +
      scale_color_manual(values = c("#6cc3d5", "#4b8c99"), name = "") +
      facet_wrap(Year ~ ., dir="v") +
      xlab("Month") +
      ylab("Death Rate") +
      theme_minimal() +
      theme(axis.title.y = element_text(size = 14)) + 
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.title.x = element_text(size = 14)) + 
      theme(axis.text.x = element_text(size = 12)) + 
      theme(legend.title = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      theme(strip.text = element_text(color = "black", size = 14, face = "bold"))
  })
  
  
  # MODERN CHART: Death Rate by Cause
  output$rates_by_cause <- renderPlot({
    title <- "rates_by_cause"
    long_data %>%
      ggplot(aes(x=Regime, y=Rate, fill=Cause)) +
      geom_bar(position="dodge", stat="identity") +
      scale_fill_manual(values = c("#6cc3d5", "#7C8083", "#f3969a"), name = "") +
      ylab("Death Rate") +
      theme_minimal() +
      theme(legend.position = "top") +
      theme(axis.title.y = element_text(size = 14)) + 
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.title.x = element_text(size = 14)) + 
      theme(axis.text.x = element_text(size = 12)) + 
      theme(legend.title = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      theme(strip.text = element_text(color = "black", size = 14, face = "bold"))
  })
  

}

shinyApp(ui = ui, server = server)