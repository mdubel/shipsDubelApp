#' Single dropdown with vessel data
#'
#' This module creates single dropdown selector for some selected vessel data and returns the selected value
#'
#' @param id string with id of the module.
#' @param input parameter used by shiny.
#' Should not be used by the user \code{\link{callModule}}.
#' @param output parameter used by shiny.
#' Should not be used by the user \code{\link{callModule}}.
#' @param session parameter used by shiny.
#' Should not be used by the user \code{\link{callModule}}.
#'
#' @return This module does not return anything.
#' @export
#' @rdname modulevesselDropdown
vesselDropdownUI <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    uiOutput(ns("vesselDropdownUI"))
  )
}



#' @param label string; how dropdown should be labeled
#' @param choices (named) vector; what are the possible selection options; if vector is named then names not values are displayed
#'
#' @import shiny
#' @importFrom shinyWidgets pickerInput
#' @export
#' @rdname modulevesselDropdown
vesselDropdown <- function(input, output, session,
                           label,
                           choices,
                           is.searchable = FALSE) {

  # DROPDOWN ----
  output$vesselDropdownUI <- renderUI({
    shinyWidgets::pickerInput(
      session$ns('vesselDropdown'),
      label = label,
      choices = choices,
      options = list(`live-search` = is.searchable,
                     `live-search-placeholder` = "Type to search"
      ),
      multiple = FALSE
    )
  })

  # RETURN ----
  return(reactive({input$vesselDropdown}))
}
