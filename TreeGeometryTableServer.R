# compare_server.R
library(shiny)
library(plotly)
source("TreeGeometryHelper.R")


table_server <- function(input, output, test_dataset) {
  
  df <- reactiveVal(NULL)

  
  #Event function to create loading data popup
  observeEvent(input$showTableData, {
    showModal(
      modalDialog(
        title = "Choose Dataframe",
        fileInput("file", "Choose RDS File",
                  accept = c(".rds")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("loadButtonForTable", "Load")
        )
      )
    )
  })
  
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    # Load the first two files selected
    df(readRDS(inFile$datapath))
  })
  
  
  output$dataTable <- renderDT({
    req(df())
    df_table <- convert_data_to_table_format(df(),"df")

    # Create a datatable with the delete button column
    datatable(df_table, escape = FALSE, 
              options = list(
                columnDefs = list(list(targets = 1, orderable = FALSE, className = 'dt-center'))
              ))
  })
  
  
  #OnClick function of load dataframe
  observeEvent(input$loadButtonForTable, {
    filedata()
    showNotification(renderPrint("The new data is loaded."))
    # Hide the delete confirmation dialog
    removeModal()
  })
  
}