library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

# Load the data
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- readRDS("ind_summary.rds")

# Define the user interface
ui <- fluidPage(
  titlePanel("Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("groupCol", "Select column to display group differences:", choices = names(data), selected = "reward_over_chance"),
      radioButtons("plotType", "Choose Visualization Style:", choices = c("boxplot", "violin", "dotplot"), selected = "boxplot"),
      selectInput("xcol", "Select the X-axis for correlation:", choices = names(data), selected = "reward_over_chance"),
      selectInput("ycol", "Select the Y-axis for correlation:", choices = names(data), selected = "avg_rt" ),
      checkboxInput("corr_error", "Show Error Bars on Correlation", FALSE)
    ),
    mainPanel(
      plotOutput("groupPlot"),
      plotOutput("correlationPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render correlation plot with smooth lines for each group
  output$correlationPlot <- renderPlot({
    req(input$xcol, input$ycol, input$groupCol)  # Ensure that the inputs are available
    ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]], color = group)) +
      geom_smooth(method = "lm", se = input$corr_error) +  # Using linear model; se = FALSE removes the confidence interval shading
      labs(title = paste("Trend between", input$xcol, "and", input$ycol, "by group"),
           x = input$xcol,
           y = input$ycol)
  })
  
  # Render group difference plot
  output$groupPlot <- renderPlot({
    req(input$groupCol)
    plot_data <- ggplot(data, aes(x = group, y = .data[[input$groupCol]], fill = group))
      if (input$plotType == "boxplot") {
        plot_data + geom_boxplot()
      } else if (input$plotType == "violin") {
        plot_data + geom_violin()
      } else if (input$plotType == "dotplot") {
        plot_data + geom_dotplot(binaxis = 'y', stackdir = 'center')
      }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
