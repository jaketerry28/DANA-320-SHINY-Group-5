### ----- Load packages ----

library(shiny)
library(tidyverse)
library(scales)
library(shinyWidgets)


### ---- Define UI ----

# create vector of variable names
# -> take out the categorical variables

df <- read_csv("Real_Estate_Sales_2001-2023_GL.csv")

ui <- fluidPage(
  titlePanel("Shiny Real Estate Explorer"),
  
  sidebarPanel(
    sliderInput(
      "yearSlider",
      "Years",
      min = min(df$`List Year`, na.rm = TRUE),
      max = max(df$`List Year`, na.rm = TRUE),
      value = range(df$`List Year`, na.rm = TRUE)
    ),
    selectInput(
      "yvar",
      "Y-Axis Variable:",
      choices = c("Sale Amount", "Assessed Value"),
      selected = "Sale Amount"
    ),
    pickerInput(
      inputId = "towns",
      label = "Choose Town(s):",
      choices = sort(unique(df$Town)),
      selected = unique(df$Town)[1:5],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    pickerInput(
      inputId = "types",
      label = "Choose Property Type(s):",
      choices = sort(unique(df$`Property Type`)),
      selected = unique(df$`Property Type`),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    )
  ),

  
  # create spot for plot
  mainPanel(
    plotOutput(outputId = "plot1"),
    plotOutput(outputId = "propertyPlot")
  )
)


### ---- Define server ----

server <- function(input, output) {
  
  # Filtered by slider
  filtered <- reactive({
    req(input$yearSlider)
    
    df[df$`List Year` >= as.numeric(input$yearSlider[1]) &
         df$`List Year` <= as.numeric(input$yearSlider[2]), ]
  })
  
  # Plot
  output$plot1 <- renderPlot({
    data <- filtered() %>%
      filter(Town %in% input$towns)
    
    town_year <- data %>%
      group_by(Town, `List Year`) %>%
      summarize(
        value = mean(.data[[input$yvar]], na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(town_year, aes(x = `List Year`, y = value, color = Town)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      labs(
        x = "List Year",
        y = input$yvar,
        title = paste("Average", input$yvar, "Over Time by Town"),
        color = "Town"
      ) +
      theme_bw()
  })


# Plot for Property Type
output$propertyPlot <- renderPlot({
  df_clean <- df %>% 
    filter(!is.na(`Property Type`)) %>%
    filter(`Property Type` %in% input$types)
  
  ggplot(df_clean, aes(x = `Property Type`, fill = `Property Type`)) +
    geom_bar() +
    labs(
      title = "Number of Sales by Property Type",
      x = "Property Type",
      y = "Number of Sales",
      fill = "Property Type"
    ) +
    theme_bw()
})
}
  

### ---- Run app ----

shinyApp(ui, server)