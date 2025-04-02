library(shiny)
library(plotly)   
library(rlang)
library(DT)
library(ForestElementsR)
library(sf)
library(data.table)
library(tibble)
source("TreeGeometryHelper.R")
library(SilvaR)

prediction_server <- function(input, output, test_dataset) {
  # Reactive values for storing data
  df <- reactiveVal(NULL)
  prediction_result <- reactiveVal(NULL)
  
  # File data reactive for loading RDS files
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df(readRDS(inFile$datapath))
    return(df())
  })
  
  # Event handler for load data button
  observeEvent(input$loadDataForPrediction, {
    showModal(
      modalDialog(
        title = "Choose Forest Data",
        fileInput("file", "Choose RDS File",
                 accept = c(".rds")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("loadButtonForPrediction", "Load")
        )
      )
    )
  })
  
  # Handle load button click
  observeEvent(input$loadButtonForPrediction, {
    filedata()
    showNotification("Forest data loaded successfully")
    removeModal()
  })
  
  # Run prediction when button is clicked
  observeEvent(input$runPrediction, {
    data_to_use <- if (is.null(df())) test_dataset else df()
    
    # Add site conditions as shown in test_simulation.R
    data_to_use$site_con <- if (input$siteFine) {
      tibble(
        forest_growth_region = "09.12.08",
        NOx = 304,
        CO2 = 340,
        length_veg_period = 145,
        temp_amplitude = 19,
        temp_veg = 14,
        precip_veg = 700,
        soil_water_avail = 0.4,
        soil_nutrient_supply = 0.4
      )
    } else {
      tibble(
        forest_growth_region = "09.12.08",
        latitude = 49,
        elevation = 700,
        exposition = 200,
        slope = 10,
        year = 2020,
        soil_moistness = 4,
        soil_nutrient_supply = 2
      )
    }
    
    tryCatch({
      # Run the prediction
      result <- run_prediction(
        fe_stand_spatial_object = data_to_use,
        site_con_fine = input$siteFine,
        mortality = input$mortalityCheck,
        runs = input$numRuns,
        n_per = input$nPeriods
      )
      
      prediction_result(result)
      showNotification("Prediction completed successfully")
      
    }, error = function(e) {
      showNotification(paste("Error in prediction:", e$message), type = "error")
    })
  })
  
  # Render prediction summary
  output$predictionSummary <- renderPrint({
    req(prediction_result())
    summary(prediction_result())
  })
  
  # Render prediction plot
  output$predictionPlot <- renderPlotly({
    req(prediction_result())
    
    summary_data <- stand_sums_dynamic(prediction_result())
    
    plot_ly() %>%
      add_trace(
        data = summary_data,
        x = ~period, 
        y = ~vol_m3_ha,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Volume (m³/ha)'
      ) %>%
      layout(
        title = "Forest Development Over Time",
        xaxis = list(title = "Period"),
        yaxis = list(title = "Volume (m³/ha)")
      )
  })
  
  # Render prediction table
  output$predictionTable <- renderDT({
    req(prediction_result())
    summary_data <- stand_sums_dynamic(prediction_result())
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}