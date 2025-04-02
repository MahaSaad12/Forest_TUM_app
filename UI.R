library(shiny)
library(plotly)

ui <- navbarPage("Forest UI",
  tabPanel("Map",
           titlePanel("Tree Positions Shiny App"),
           mainPanel(
             plotlyOutput("treePlot"),
             uiOutput("deleteConfirmation"),
             fluidRow(
               column(width = 3,actionButton("showInstallPopup", "Save"),),
               column(width = 3,actionButton("showLoadData", "Load New Data"),),
               column(width = 3,actionButton("reset", "Reset"),),
             ),
             verbatimTextOutput("installButtonMessage")
           ) 
  ),
  tabPanel("Compare",
      verbatimTextOutput("Compare"),
      mainPanel(
        actionButton("showLoadDataForCompare", "Load Data"),
        fluidRow(
          column(width = 8,plotlyOutput("treePlot1"),),
          column(width = 8,plotlyOutput("treePlot2"),),
        ),
      )
  ),
  tabPanel("Table",
    mainPanel(
      actionButton("showTableData", "Load Data"),
      DTOutput("dataTable")
    )
  ),
  tabPanel("Prediction",
    titlePanel("Forest Growth Prediction"),
    sidebarLayout(
      sidebarPanel(
        actionButton("loadDataForPrediction", "Load Forest Data"),
        hr(),
        checkboxInput("siteFine", "Use Fine Site Conditions", FALSE),
        checkboxInput("mortalityCheck", "Include Mortality", TRUE),
        numericInput("numRuns", "Number of Runs", 1, min = 1, max = 10),
        numericInput("nPeriods", "Number of Periods", 10, min = 1, max = 50),
        actionButton("runPrediction", "Run Prediction")
      ),
      mainPanel(
        verbatimTextOutput("predictionSummary"),
        plotlyOutput("predictionPlot"),
        DTOutput("predictionTable")
      )
    )
  ),
  tags$head(
    includeCSS("styles.css"),
    includeCSS("custom.css"),
    tags$script(
      '
                     function deleteTree(button) {
                       var id = button.id; // Get the id of the button (which contains the x and y coordinates)
                       var coordinates = id.split("_"); // Split the id to extract the x and y coordinates
                       var x = coordinates[0];
                       var y = coordinates[1];
                       // Send the coordinates to Shiny to handle the deletion
                       Shiny.setInputValue("tree_clicked", {x: x, y: y});
                     }
                     '
    )
  )
)

server <- function(input, output) {
  # Your server logic here
}

shinyApp(ui = ui, server = server)