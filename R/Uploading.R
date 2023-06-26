UploadingUI <- function(id){
  tabItem(
    tabName = "upload",
    fluidRow(
      sidebarPanel(

        # File input component (CSV) ----
        fileInput(shiny::NS(id,"file1"),
                  "Upload your CSV file",
                  multiple = FALSE,
                  accept = c("text/csv","text/comma-separated-values,
                             text/plain",".csv")
                  ),
        tags$hr(),

        # Input: Checkbox if file has header ----
        checkboxInput(shiny::NS(id,"header"), "Has header", TRUE),

        # Input: Select separator ----
        radioButtons(shiny::NS(id,"sep"), "Separator",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        tags$hr(),

        # Input: Select number of rows to display ----
        radioButtons(shiny::NS(id,"disp"), "Display",
                     choices = c(Head = "head", All = "all"),
                     selected = "head")
        ),

      mainPanel(width = 12,
                # Output: Data file ----
                tabsetPanel(id=shiny::NS(id,'tables'),
                            tabPanel("Data",
                                     DT::dataTableOutput(
                                       shiny::NS(id,"contents")
                                       )
                                     ),
                            #tabPanel("Data Types",
                            #         DT::dataTableOutput("dataTypes")
                            #         ),
                            tabPanel("Missing Values",
                                     DT::dataTableOutput("missing")
                                    )
                            )
                )
      )
    )
}

UploadingServer <- function(id) {
  moduleServer(id, function(input, output, session)
    {
    values <- reactiveValues(df_data = NULL)
    observeEvent(ignoreInit = TRUE,
                  list(input$file1, input$sep ,input$header),
                  {
                    req(input$file1)
                    values$df = utils::read.csv(input$file1$datapath,
                                                header = input$header,
                                                sep= input$sep)
                  })
    output$contents <- DT::renderDataTable({
      req(input$file1)
      if(input$disp == "head") {
        return(head(values$df))
        }
      else {
        return(values$df)
        }
      })



                   # output$missing <- DT::renderDataTable({
                   #   req(input$file1)
                   #   tryCatch(
                   #     {
                   #       na_count = na_counts(values$df)
                   #
                   #       return(na_count)
                   #
                   #     },
                   #     error = function(e) {
                   #       # return a safeError if a parsing error occurs
                   #       stop(safeError(e))
                   #     }
                   #   )
                   #
                   #
                   # })
    }
  )
}
