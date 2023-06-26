DataTypesUI <- function(id){
  tabItem(
    tabName = "dtypes",
    fluidRow(
      sidebarPanel(

        # File input component (CSV) ----
        sidebarPanel(
          selectInput("select_col1","Seleccionar una columna",choices = ""),
          actionButton("do", "Turn to factor"))

      )
      ),


      mainPanel(width = 12,
                # Output: Data file ----
                tabPanel("Data Types",
                         DT::dataTableOutput("dataTypes")
                         ),
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
