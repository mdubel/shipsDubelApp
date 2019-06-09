# load pre-downloaded data
glb.ships.data <- data.table::fread(
  "data/ships.csv",
  sep = ",",
  encoding = "UTF-8"
)

# format date column into actual date in order to sort by it 
glb.ships.data[, DATETIME_TIME := as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%S")]
# make sure that SHIPNAME is ok column to be used as identifier
length(unique(glb.ships.data$SHIPNAME)) == length(unique(glb.ships.data$SHIP_ID))
# not quite: seems that 23 ships have doubled names -> but we only must do sth with it if they also share the same ship_type
# we need to have unique but user friendly names to be displayed on UI

# usually we want to keep it the same
glb.ships.data[, SHIPNAME_ID := SHIPNAME]
# for loop is efficient enough for this problem
for(name in unique(glb.ships.data$SHIPNAME)) {
  # get unique ids for given name (we hoped that it was only one)
  unique.ids <- unique(glb.ships.data[SHIPNAME == name, SHIP_ID])
  
  if(length(unique.ids) > 1) {
    unique.types <- unique(glb.ships.data[SHIPNAME == name, ship_type])
    # if the number of unique types is the same as number of unique ids we assume that names are unique inside the type
    # what is enough for our app, as we select data based on those two fields
    if(length(unique.types) < length(unique.ids)) {
      # keep the first occurance the same
      for(i in 2:length(unique.ids)) {
        glb.ships.data[SHIPNAME == name & SHIP_ID == unique.ids[i], SHIPNAME_ID := paste0(SHIPNAME, "_", i)]
      }
    } else if(length(unique.types) > length(unique.ids)) {
      warning(sprintf("There is sth wrong with number of types for %s", name))
    }
  }
}
# FYI: no warnings were printed out
# names were modified for 7 vessels. Looks like 'AMANDA' is popular as a ship name both for Danish and Swedish sailors.

# get the sorted data by name and time
data.table::setorderv(glb.ships.data, cols = c("SHIP_ID", 'DATETIME_TIME'))

# the time between observations seems to be about 2 minutes, but I've noticed also some exceptions. Let's examine it:
glb.ships.data[, TIME_DIFF := c(0, diff(DATETIME_TIME)), by = SHIP_ID]

# yes, seems that there are peculiar observations which are very close (2-3 seconds) to one another
# min equal to 0 is obvious as it was set for the first entry for each ship
# but the small time differences do not bother us - they won't be classified as max distance as ship do not move much in 2 seconds
# the problem might be with huge time differences that also happens: those are probably some missings in observations between which might result in 
# presenting distance that might seems to be teleportation-fast (check e.g. ship "KONYA". And the record is hold by "HANNA")
# or they've just were parked during that time and it won't matter

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0      2.0    118.0    110.2    120.0 567120.0 

# Ships would probably sail the longest distance for those suprisingly big time gaps. It might not be an error in data but be connected to how the data is collected:
# I can imagine that vessels are checked often when being close to shipyard, but rarely on the open sea. I'd need more information about the data in order to remove some outliers
# nevertheless the times are presented in the app (in the note below map) thus if user notice some strangly long distance it might be checked that it has taken longer that usual

# Finally let's remove the redundant colums to make dataset smaller:
columns.to.keep <- c("LAT", "LON", "SHIPNAME_ID", "SHIP_ID", "ship_type", "is_parked", "DATETIME_TIME")
columns.to.remove <- setdiff(colnames(glb.ships.data), columns.to.keep)
glb.ships.data[, (columns.to.remove) := NULL]
# and lets rename columns a little bit to make it consistent:
data.table::setcolorder(glb.ships.data, columns.to.keep)
names(glb.ships.data) <- c("LAT", "LON", "SHIP_NAME", "SHIP_ID", "SHIP_TYPE", "IS_PARKED", "DATETIME")
# order again just to be sure:
data.table::setorderv(glb.ships.data, cols = c("SHIP_ID", 'DATETIME'))

# and finally we save the new dataset to the csv again. To recapture we've:
# 1) make sure that the data is ordered by the increasing time for each SHIP_ID
# 2) make sure that the names can work as id inside selected ship type
# 3) checked that there are some outliers in the data in time differences between observations (but nothing was done with it, it might be expected result)
# 4) data was trunckated to save space and load app quicker
write.csv(glb.ships.data, "data/ships_organised.csv")
