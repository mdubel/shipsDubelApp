library(data.table)
source("R/utils.R")
source("R/module-vesselDropdown.R")
# load marine data as data.table
# NOTE: file relocation in package should be reflected here
glb.ships.data <- data.table::fread(
  "data/ships_organised.csv",
  sep = ",",
  encoding = "UTF-8"
)
