function(input, output, session) {
  # get the list to store the modules' values
  rtnVessel <- reactiveValues()

  # VESSEL TYPE ----
  # save the type to object to use it later (note that module returns reactive)
  rtnVessel$vessel.type <-
    callModule(
      module = vesselDropdown,
      id = "vesselDropdownType",
      label = "Type",
      is.searchable = FALSE,
      choices = glb.ships.data$ship_type %>% getDropdownChoices()
    )

  # VESSEL NAME ----
  # get the available names for the ships
  rctAvailableNames <- reactive({
    req(rtnVessel$vessel.type())
    req(rtnVessel$vessel.type() != "")
    glb.ships.data[ship_type == rtnVessel$vessel.type(), SHIPNAME] %>% getDropdownChoices()
  })

  # save the name to object according to the type selected
  observeEvent(rtnVessel$vessel.type(), {
    rtnVessel$vessel.name <-
      callModule(
        module = vesselDropdown,
        id = "vesselDropdownName",
        label = "Name",
        is.searchable = TRUE,
        choices = rctAvailableNames()
      )
  })

  # DATA FOR SELECTED VESSEL ----
  rctVesselData <- reactive({
    req(rtnVessel$vessel.type())
    req(rtnVessel$vessel.type() != "")
    req(rtnVessel$vessel.name())
    # to not react too quickly
    req(rtnVessel$vessel.name() %in% rctAvailableNames())

    returned.data <- glb.ships.data[ship_type == rtnVessel$vessel.type() & SHIPNAME == rtnVessel$vessel.name() & is_parked == 0, ]

    # for cases when ships does not sail show his parking location
    if(nrow(returned.data) > 0) {
      return(returned.data)
    } else {
      return(tail(glb.ships.data[ship_type == rtnVessel$vessel.type() & SHIPNAME == rtnVessel$vessel.name(), ], 1))
    }
  })

  # list of longest distance sailed (in meters) and index of observation
  rctLongestDistance <- reactive({
    req(rctVesselData)
    if(nrow(rctVesselData()) > 1) {
      getLongestDistance(rctVesselData())
    } else {
      list()
    }
  })

  rctLongestDistanceData <- reactive({
    req(rctLongestDistance)
    if(nrow(rctVesselData()) > 1) {
      rctVesselData()[rctLongestDistance()$which:(rctLongestDistance()$which + 1), ]
    } else {
      rctVesselData()
    }
  })

  # VESSEL INFO ----
  # note about distance covered
  output$vesselInfo <- renderText({
    req(rctLongestDistance)
    validate(need(nrow(rctVesselData()) > 1, "Selected vessel does not sail in observed data."))

    sprintf("Vessel <b>%s</b> of type <b>%s</b> longest distance sailed was <b>%.2f</b> meters between <b>%s</b> and <b>%s</b>",
            rtnVessel$vessel.name(),
            rtnVessel$vessel.type(),
            rctLongestDistance()$max,
            rctLongestDistanceData()[1, DATETIME],
            rctLongestDistanceData()[2, DATETIME])
  })

  # LEAFLET MAP ----
  # map shows either longest distance sailed between two points or vessel location in case it was parked all the time
  output$distanceMap <- renderLeaflet({
    req(rctLongestDistanceData)

    leaflet(rctLongestDistanceData()) %>%
      addTiles() %>%
      addMarkers(lng = ~LON, lat = ~LAT) %>%
      addPolylines(lng = ~LON, lat = ~LAT)
  })

}
