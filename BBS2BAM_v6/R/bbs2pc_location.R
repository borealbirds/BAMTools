#' Create XY table
#'
#' The function used SScombo received from Diana Stralberg and create XY table according to coordinate we have 
#'
#' @param xytbl A path to access current XY table.
#' 
#' @param lookupTables A list of path to access lookupTables. 
#' 
#' @param dataset_tbl A dataframe with dataset parameters.
#' 
#' @return XY table
#'
#' @importFrom utils read.table
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname bbs2pc_location
#' @examples
#' bbs2pc_location(xytbl, lookupTables, dataset_tbl)
bbs2pc_location <- function(xytbl, lookupTables, dataset_tbl) {
  
  if (!file.exists(xytbl)) {
    stop("You must provide existing path to access XY table")
  }
  
  if (missing(lookupTables)) {
    stop("You must provide a list of path to access lookupTables")
  }
  
  if (!exists(deparse(substitute(dataset_tbl)))) {
    stop("You must provide a valid dataset_tbl object. Please run the bbs2pc_dataset function")
  }
  # Start
  BBSstop <- read.csv(file = xytbl, header = TRUE, sep=",")
  BBSstop_dataset <- merge(BBSstop, dataset_tbl[, c("dataset_id", "dataset_code")], by.x="PCODE", by.y="dataset_code")
  
  # SET vector column
  location_id <-seq.int(nrow(BBSstop_dataset))
  dataset_fk <- BBSstop_dataset$dataset_id
  location_name <- BBSstop_dataset$SS
  location_longitude <- BBSstop_dataset$X_Long
  location_latitude <- BBSstop_dataset$Y_Lat
  OnRoad <- 'Yes'

  # Create XY table
  bbs_location <- data.frame(location_id, dataset_fk, location_name, location_longitude, location_latitude, OnRoad)
  return(bbs_location)
}
