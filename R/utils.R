#' Function that prepares input to dropdowns by removing duplicates and sorting character vectors
#'
#' @param input.vector character; data that fills dropdown
#'
#' @return character vector of unique and sorted values
#' @export
#'
#' @examples getDropdownChoices(c("kota", "kota", "ma", "ala"))
getDropdownChoices <- function(input.vector) {
  stopifnot(is.character(input.vector))
  stopifnot(length(input.vector) > 0)

  return(input.vector %>% unique() %>% sort())
}


#' Function that finds the longest distance sailed by selected vessel
#'
#' @param vessel.data data.table; marine data for selected vessel
#'
#' @return list of maximum disatance and index of observation from which max was observed
#' @export
#'
#' @import data.table
getLongestDistance <- function(vessel.data) {
  # TODO data looks to not be cleaned fully: check KAROLI Cargo ship

  # calculates the distance vector
  distance.vector <- calcDistance(vessel.data)
  stopifnot(length(distance.vector) > 0)
  
  # get the max value
  distance.max <- max(distance.vector)
  # get the index for which the max was found (if more then one selects the last observation)
  distance.which.max <- which(distance.vector == distance.max) %>% tail(., 1)

  return(list(max = distance.max, which = distance.which.max))
}

#' Function that calculates the distance vector for given set of localisation data
#'
#' @param vessel.data data.table; marine data for selected vessel
#'
#' @return vector of distances between each pair of points (length is nrow of vessel.data - 1)
#' @export
#'
#' @import data.table
#' @importFrom geosphere distGeo
calcDistance <- function(vessel.data) {
  stopifnot(all(c("LON", "LAT") %in% names(vessel.data)))
  stopifnot(nrow(vessel.data) > 0)
  
  # select only LON and LAT columns (in that order!); calculate the distance vector and remove the last NA value (there is no distance that starts in last observation) 
  vessel.data[, .(LON, LAT)] %>% geosphere::distGeo() %>% na.omit()
}
