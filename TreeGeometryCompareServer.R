# compare_server.R
library(shiny)
library(plotly)
source("TreeGeometryHelper.R")


compare_server <- function(input, output, test_dataset) {
  
  df1 <- reactiveVal(NULL)
  df2 <- reactiveVal(NULL)
  df1_table <- reactiveVal(NULL)
  df2_table <- reactiveVal(NULL)
  combined_table <- reactiveVal(NULL)
  
  #Event function to create loading data popup
  observeEvent(input$showLoadDataForCompare, {
    showModal(
      modalDialog(
        title = "Choose Dataframe",
        fileInput("file", "Choose RDS File",
                  multiple = TRUE,
                  accept = c(".rds")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("loadButtonForCompare", "Load")
        )
      )
    )
  })
  
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    # Check if more than two files are selected
    if (length(inFile$datapath) > 2) {
      showNotification("Please choose only two files.", type = "warning")
      return(NULL)
    }
    # Load the first two files selected
    df1(readRDS(inFile$datapath[1]))
    df2(readRDS(inFile$datapath[2]))
  })
  
  output$treePlot1 <- renderPlotly({
    req(df1())
    df1_table(convert_data_to_table_format(df1(),"df1"))
    create_scatter_plot(convert_data_to_map_format(df1()))
  })
  
  output$treePlot2 <- renderPlotly({
    req(df2())
    df2_table(convert_data_to_table_format(df2(),"df2"))
    create_scatter_plot(convert_data_to_map_format(df2()))
  })
  
  output$dataTable <- renderDT({
    req(df1_table(), df2_table())
    
    combined_df <- rbind(df1_table(), df2_table())
    combined_df$database_tree_id <- paste0(combined_df$database_name, "_", combined_df$tree_id)
    
    combined_table(combined_df)
    button_id <- paste(combined_df$x, combined_df$y, sep = "_")
    # Add a delete button column
    combined_df$delete <- sprintf('<button id="%s" class="btn btn-danger btn-sm" onclick="deleteTree(this)">Delete</button>',button_id )
    
    # Reorder columns
    combined_df <- combined_df[, c(setdiff(names(combined_df), "delete"), "delete")]
    
    # Create a datatable with the delete button column
    datatable(combined_df[, c("delete", setdiff(names(combined_df), "delete"))], escape = FALSE, 
              options = list(
                columnDefs = list(list(targets = 1, orderable = FALSE, className = 'dt-center'))
              ))
  })
  
  # Observe the click event on delete buttons
  observeEvent(input$tree_clicked, {
    clicked_point <- input$tree_clicked
    print(clicked_point)
    x_clicked <- clicked_point$x
    y_clicked <- clicked_point$y
    print(x_clicked,y_clicked)
    # Find the tree closest to the clicked point
    update_removal_status(x_clicked, y_clicked,TRUE)
  })
  
  #OnClick function of load dataframe
  observeEvent(input$loadButtonForCompare, {
    filedata()
    showNotification(renderPrint("The new data is loaded."))
    # Hide the delete confirmation dialog
    removeModal()
  })
  
}