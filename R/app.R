library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggpubr)
library(bslib)
#statsApp <- function (...){

library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)
ui<- function() {
  dashboardPage(

    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Uploading dataset", tabName = "upload",
                 icon = icon("upload")
                 ),
        menuItem("Data types", tabName = "dtypes",
                 icon = icon("upload")
        ),
        menuItem("Preprocesamiento", tabName = "preprocessing",
                 icon = icon("search")
                 ),
        menuItem("Normalidad", tabName = "normal",
                 icon = icon("poll")
                 ),
        menuItem("Homocedasticidad",tabName="homoced",
                 icon=icon('bar-chart')
                 ),
        menuItem("Tests",tabName="tests",
                 icon=icon('table')
                 )
        )
      ),
    dashboardBody(
      use_theme(mytheme),
      UploadingUI("file1")
      )
  )
  }

server <- function(input, output, session) {
    UploadingServer("file1")
  }

shinyApp(ui, server)
#}
