# Required Libraries
library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(ggplot2)

# Load the dataset
happiness_data <- read.csv("World-happiness-report-2024.csv")

# UI Code
ui <- fluidPage(
  titlePanel("World Happiness Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = unique(happiness_data$Regional.indicator), selected = "Western Europe", multiple = TRUE),
      selectInput("country", "Select Country:", choices = unique(happiness_data$Country.name), selected = "Finland", multiple = TRUE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("happinessTable")),
        tabPanel("Happiness Trends", plotlyOutput("happinessTrend")),
        tabPanel("Factor Correlations", plotlyOutput("correlationPlot")),
        tabPanel("Regional Analysis", plotlyOutput("regionAnalysis")),
        tabPanel("Regression Analysis", plotlyOutput("regressionPlot"))
      ),
      width = 9
    )
  )
)

# Server Code
server <- function(input, output, session) {
  
  # Filtered Data based on user input
  filtered_data <- reactive({
    happiness_data %>%
      filter(Regional.indicator %in% input$region, Country.name %in% input$country)
  })
  
  # Data Table
  output$happinessTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Happiness Score Trends
  output$happinessTrend <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Country.name, y = ~Ladder.score, type = 'scatter', mode = 'lines+markers', name = 'Happiness Score') %>%
      layout(title = "Happiness Score Trends by Country", xaxis = list(title = "Country"), yaxis = list(title = "Happiness Score"))
  })
  
  # Correlation Plot: Happiness Score vs GDP
  output$correlationPlot <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Log.GDP.per.capita, y = ~Ladder.score, type = 'scatter', mode = 'markers', color = ~Country.name) %>%
      layout(title = "Happiness Score vs GDP", xaxis = list(title = "Log GDP per Capita"), yaxis = list(title = "Happiness Score"))
  })
  
  # Regional Analysis: Average Happiness by Region
  output$regionAnalysis <- renderPlotly({
    df <- happiness_data %>%
      group_by(Regional.indicator) %>%
      summarise(Average_Happiness = mean(Ladder.score, na.rm = TRUE))
    
    plot_ly(df, x = ~Regional.indicator, y = ~Average_Happiness, type = 'bar', name = 'Average Happiness') %>%
      layout(title = "Average Happiness by Region", xaxis = list(title = "Region"), yaxis = list(title = "Average Happiness"))
  })
  
  # Regression Plot: Happiness Score ~ GDP + Social Support
  output$regressionPlot <- renderPlotly({
    df <- filtered_data()
    
    # Build the regression model
    lm_model <- lm(Ladder.score ~ Log.GDP.per.capita + Social.support, data = df)
    
    plot_ly(df, x = ~Log.GDP.per.capita, y = ~Ladder.score, mode = 'markers', name = 'Data') %>%
      add_lines(x = df$Log.GDP.per.capita, y = predict(lm_model), name = 'Regression Line') %>%
      layout(title = "Regression: Happiness Score ~ GDP + Social Support", xaxis = list(title = "Log GDP per Capita"), yaxis = list(title = "Happiness Score"))
  })
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
