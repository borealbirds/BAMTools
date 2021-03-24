##################################################################################
#' create BBS point Count table based on BBS Fifty
#'
#' The function produce a character vector of file available to download. It uses a preset table
#' in which a list of relevant dataset name, their associate url and password are stored. To
#' retrive available file, the function derive URL, and username/password using the \code{datasetName}
#'
#' @param xytbl A path to access current XY table.
#'
#' @param lookupTables A list of path to access lookupTables.
#'
#' @return Vector of available url to download
#'
#' @importFrom utils read.csv
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname bbs2pc_dataset
#' @examples
#' url <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide/"
#' bbs2pc_dataset(xytbl, lookupTables)
bbs2pc_dataset <- function(xytbl, lookupTables) {

    # Check params
  if (!file.exists(xytbl)) {
    stop("You must provide existing path to access XY table")
  }
  if (missing(lookupTables)) {
    stop("You must provide a list of path to access lookupTables")
  }
  BBSstop <- read.csv(file = xytbl, header = TRUE, sep=",")
  
  # SET vector column
  dataset_code <- unique(BBSstop$PCODE)
  dataset_id <- seq.int(length(dataset_code))
  Acronym <- sub(".*BBS","", dataset_code)
  facronym <- read.csv(file = unlist(lookupTables[names(lookupTables)=='regionCodes']), header = TRUE, sep=",")
  dataset_name <- paste("BBS", facronym$State.Prov.TerrName[match(Acronym, facronym$Acronym)])
  dataset_description <- " "
  primary_contact <- "Canada Wildlife Service and USGS"
  Organization <- "Canada Wildlife Service and USGS"
  Affiliation <- "Canada Wildlife Service and USGS"
  Privacy_level <- "Public"

  # Create bbs_PCODE table
  bbs_dataset <- data.frame(dataset_id, dataset_code, dataset_name, dataset_description, primary_contact, Organization, Affiliation, Privacy_level)  
  return(bbs_dataset)
}

