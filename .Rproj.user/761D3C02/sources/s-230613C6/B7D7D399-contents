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
#' @importFrom geosphere distGeo
getLongestDistance <- function(vessel.data) {
  # TODO data looks to not be cleaned fully: check KAROLI Cargo shi[]
  stopifnot(all(c("LON", "LAT") %in% names(vessel.data)))
  stopifnot(nrow(vessel.data) > 0)

  distance.vector <- vessel.data[, .(LON, LAT)] %>% geosphere::distGeo() %>% na.omit()
  distance.max <- max(distance.vector)
  distance.which.max <- which(distance.vector == distance.max) %>% tail(., 1)

  return(list(max = distance.max, which = distance.which.max))
}
