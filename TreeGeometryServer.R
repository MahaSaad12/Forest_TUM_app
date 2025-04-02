# server.R
library(shiny)
library(plotly)
source("TreeGeometryHelper.R")

# Define the server function that takes the test_dataset as a parameter
server <- function(input, output, test_dataset) {
  not_removed_name <- "Not Removed"
  removed_name <- "Removed"
  #add reactive component
  click_info <- reactiveValues(point_clicked = NULL)
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df(readRDS(inFile$datapath))
    first_init_df <- df()
    return(df())
  })
  
  #convert list to dataframe for the scatter plot
  df_value <- convert_data_to_map_format(test_dataset)
  df <- reactiveVal(df_value)
  
  #Original df is saved here to be used in reset case
  first_init_df <- df_value
  
  # Create a scatter plot for trees and removed trees
  output$treePlot <- renderPlotly({
    print("treePlot is started")
    df_from_user <- filedata()
    if (!is.null(df_from_user)) {
      print("Data is loaded from file")
      df(df_from_user)
      create_scatter_plot(convert_data_to_map_format(df_from_user))
    }else{
      print("Data is loaded from")
      create_scatter_plot(df())
    }
  })
  
  # Function to update removal status
  update_removal_status <- function(x, y, removal_status) {
    tolerance <- 0.01 # adjust tolerance level as needed
    row_index <- which(abs(df()$x - x) < tolerance & abs(df()$y - y) < tolerance)
    if (length(row_index) > 0) {
      temp <- df()
      temp[row_index, "status"] <- removal_status
      df(temp)
    } else {
      print("No matching tree found.")
    }
  }
  
  output$deleteConfirmation <- renderUI({
    req(input$point_clicked)
    if(input$point_clicked$status == 1){
      showModal(
        modalDialog(
          title = "Delete Tree",
          "Are you sure you want to delete this tree?",
          footer = tagList(
            modalButton("No"),
            actionButton("delete_clicked", "Yes")
          )
        )
      )
    }else if(input$point_clicked$status == 0){
      showModal(
        modalDialog(
          title = "Take Back the Tree",
          "Are you sure you want to take back this tree?",
          footer = tagList(
            modalButton("No"),
            actionButton("tack_back_clicked", "Yes")
          )
        )
      )
    }else{
      showNotification("This tree is dead, you cannot do anything...")
    }
  })
  
  observeEvent(input$reset, {
    df(first_init_df)
    showNotification("Dataframe has been reseted")
  })
  #Event function to create loading data popup
  observeEvent(input$showLoadData, {
    showModal(
      modalDialog(
        title = "Choose Dataframe",
        fileInput("file", "Choose RDS File",
                  accept = c(".rds")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("loadButton", "Load")
        )
      )
    )
  })
  observeEvent(input$showInstallPopup, {
    showModal(
      modalDialog(
        title = "Enter Path",
        textInput("pathInput", "Enter path:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("saveButton", "Save")
        )
      )
    )
  })
  #OnClick function of save dataframe
  observeEvent(input$saveButton, {
    path <- input$pathInput
    message <- save_dataframe(path,df)
    showNotification(renderPrint(message))
    # Hide the delete confirmation dialog
    removeModal()
  })
  #OnClick function of load dataframe
  observeEvent(input$loadButton, {
    filedata()
    showNotification(renderPrint("The new data is loaded."))
    # Hide the delete confirmation dialog
    removeModal()
  })
  observeEvent(input$treePlot_click, {
    x_clicked <- input$treePlot_click$x
    y_clicked <- input$treePlot_click$y
    
    # Pass the clicked point's coordinates to JavaScript
    session$sendCustomMessage("clickedPoint", list(x = x_clicked, y = y_clicked))
  })
  # Handle deletion button click
  observeEvent(input$deleteButton, {
    showModal("deleteConfirmation")
  })
  # Handle delete confirmation
  observeEvent(input$delete_clicked, {
    clicked_point <- input$point_clicked
    # Update the removal status for the clicked point
    update_removal_status(clicked_point$x, clicked_point$y, 0)
    # Hide the delete confirmation dialog
    removeModal()
  })
  # Handle tack back confirmation
  observeEvent(input$tack_back_clicked, {
    clicked_point <- input$point_clicked
    # Update the removal status for the clicked point
    update_removal_status(clicked_point$x, clicked_point$y, 1)
    # Hide the delete confirmation dialog
    removeModal()
  })
  # Output the unique years
  output$uniqueYearsOutput <- renderText({
    unique_years <- unique(test_dataset$trees$time_yr)
    paste("Number of unique years: ", length(unique_years))
  })

  
  # Execute the function when the application stops
  onStop(function() {
    delete_forestr_files("^forestr_")
  })
}
