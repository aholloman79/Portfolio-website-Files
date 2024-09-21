#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Warehouse Logistics Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("This app visualizes the relationship between Shipment Volume, Handling Time, and Cost using a Thin Plate Spline model.")
    ),
    mainPanel(
      plotlyOutput("tpsPlot")
    )
  )
))

library(shiny)
library(plotly)
library(fields)

shinyServer(function(input, output) {
  
  # Load your dataset (assuming 'warehouse_logistics_analysis' is stored in the specified directory)
  warehouse_logistics_analysis <- read.csv("C:/Users/jacob/OneDrive/Desktop/R Studio Projects 2024/Datasets/ShinyApp1/warehouse_logistics_analysis.csv")
  
  # Define x, y, z
  x <- warehouse_logistics_analysis$Shipment_Volume
  y <- warehouse_logistics_analysis$Handling_Time
  z <- warehouse_logistics_analysis$Cost
  
  # Fit a thin plate spline model
  tps_model <- Tps(cbind(x, y), z)
  
  # Create a grid for the TPS model to predict on
  grid_size <- 40  # Increase for finer resolution
  x_grid <- seq(min(x), max(x), length.out = grid_size)
  y_grid <- seq(min(y), max(y), length.out = grid_size)
  grid <- expand.grid(x_grid, y_grid)
  z_pred <- matrix(predict(tps_model, grid), nrow = grid_size, ncol = grid_size)
  
  # Render the 3D plot using Plotly
  output$tpsPlot <- renderPlotly({
    plot_ly() %>%
      add_surface(x = ~x_grid, y = ~y_grid, z = ~z_pred, opacity = 0.7, showscale = FALSE) %>%
      add_markers(x = ~x, y = ~y, z = ~z, marker = list(size = 5, color = 'red')) %>%
      layout(scene = list(xaxis = list(title = 'Shipment Volume'),
                          yaxis = list(title = 'Handling Time'),
                          zaxis = list(title = 'Cost')))
  })
})

