library(shiny)
library(shinydashboard)
library(magrittr)
library(shinyWidgets)
library(geosphere)
library(leaflet)
library(data.table)

ui <- function() {
  dashboardPage(
    title = "Ships Dubel",
    dashboardHeader(
      title = "Marine Data Dubel"
    ),
    dashboardSidebar(
      tags$h5(
        "Maps shows the longest distance sailed by selected vessel. Please select the type and the name to show the movement.",
        style = "padding: 15px;"
      ),
      vesselDropdownUI("vesselDropdownType"),
      vesselDropdownUI("vesselDropdownName")
    ),
    dashboardBody(
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            leafletOutput("distanceMap", width = "100%", height = "500px")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            htmlOutput("vesselInfo")
          )
        )
      )
    )
  )
}
