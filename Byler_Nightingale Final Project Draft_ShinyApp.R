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
  sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1, max = 100),
  plotOutput("hist"),
  plotOutput("rose")
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