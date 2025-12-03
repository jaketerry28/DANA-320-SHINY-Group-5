### ----- Load packages ----

library(shiny)
library(tidyverse)
library(scales)

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
    )
  ),

  
  # create spot for plot
  mainPanel(
    plotOutput(outputId = "plot1")
  )
)


### ---- Define server ----

server <- function(input, output) {
  
  # Filtered by slider
  filtered <- reactive({
    req(input$yearSlider)  # ensure slider exists
    
    df[df$`List Year` >= as.numeric(input$yearSlider[1]) &
         df$`List Year` <= as.numeric(input$yearSlider[2]), ]
  })
  
  # Plot
  output$plot1 <- renderPlot({
    data <- filtered()
    
    # aggregate by year
    yearly <- data %>%
      group_by(`List Year`) %>%
      summarize(
        value = mean(.data[[input$yvar]], na.rm = TRUE)
      )
    
    ggplot(yearly, aes(x = `List Year`, y = value)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      labs(
        x = "List Year",
        y = input$yvar,
        title = paste("Average", input$yvar, "Over Time")
      ) +
      theme_bw()
  })
}
  

### ---- Run app ----

shinyApp(ui, server)