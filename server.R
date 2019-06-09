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
    req(rtnVessel$vessel.name())
    # to not react too quickly
    req(rtnVessel$vessel.name() %in% rctAvailableNames())

    ship.data <- glb.ships.data[ship_type == rtnVessel$vessel.type() & SHIPNAME == rtnVessel$vessel.name(), ]
    sailing.data <- ship.data[is_parked == 0, ]

    # for cases when ships does not sail show its last 'parking' location
    if(nrow(sailing.data) > 0) {
      return(sailing.data)
    } else {
      return(ship.data %>% tail(., 1))
    }
  })

  # list of longest distance sailed (in meters) and index of observation
  rctLongestDistance <- reactive({
    req(rctVesselData)
    # calculate the distance and index only if there are at least two points when ship was actually sailing
    if(nrow(rctVesselData()) > 1) {
      getLongestDistance(rctVesselData())
    # otherwise there is no point in calculating distance thus return empty list  
    } else {
      list()
    }
  })

  rctLongestDistanceData <- reactive({
    req(rctLongestDistance)
    # distance is calculated between two points, we can call them 'from' and 'to'. $which is the index of 'from' observation thus we need to select this and the next one
    if(nrow(rctVesselData()) > 1) {
      rctVesselData()[rctLongestDistance()$which:(rctLongestDistance()$which + 1), ]
    } else {
    # for 'parked' vessels just pass the data which is the last observed parking location  
      rctVesselData()
    }
  })

  # VESSEL INFO ----
  # note about distance covered
  output$vesselInfo <- renderText({
    req(rctLongestDistance)
    # for 'parked' vessels show the following info
    validate(need(nrow(rctVesselData()) > 1, "Selected vessel does not sail in observed data."))

    # here message can be edited
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
