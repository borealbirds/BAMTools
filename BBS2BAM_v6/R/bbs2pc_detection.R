##################################################################################
#' create BBS point Count table based on BBS Fifty
#'
#' The function produce a character vector of file available to download. It uses a preset table
#' in which a list of relevant dataset name, their associate url and password are stored. To
#' retrive available file, the function derive URL, and username/password using the \code{datasetName}
#'
#' @param fiftyDir A character string giving the path where the downloaded files are saved.
#'
#' @param datasetName A character string. Name of the dataset from which BBS PointCount table will be created. Default is all fifty files. 
#' 
#' @param lookupTables A list of path to access lookupTables.
#'
#' @param minYear Numerical giving the lower bound of year to be included. Default is \code{1965}.
#' 
#' @param maxYear Numerical giving the upper bound of year to be included. Default is \code{2019}.
#'
#' @param survey_tbl An dataframe from which geographical range is subset and visit foreign_key is derived.
#'
#' @param dropZeros Logical Drop all species with abundance being zero. Default is TRUE.
#'
#' @return Vector of available url to download
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' @importFrom utils read.csv
#' @importFrom data.table setnames
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname bbs2pc_detection
#' @examples
#' fiftyDir = outDir
#' lookupTables = list(regionCodes = file.path(lookupDir,fregionCodes), 
#'                     speciesList = file.path(lookupDir,fSpecies)) 
#' example <-bbs2pc_detection(fiftyDir, c("fifty1.csv"), lookupTables, minYear=2018, maxYear=2019, bbs_visit)
bbs2pc_detection <- function(fiftyDir, datasetName, lookupTables, minYear=1965, maxYear=2019, survey_tbl, dropZeros = TRUE) {

  # Check params
  if (missing(fiftyDir)) {
    stop("You must provide directory where Fifty were downloaded")
  }else{
    if (!file.exists(fiftyDir)) {
      stop("You must provide a valid directory where Fifty are downloaded")
    }
  }
  if (missing(datasetName)) datasetName <- c("fifty1.csv","fifty2.csv","fifty3.csv","fifty4.csv","fifty5.csv",
                                             "fifty6.csv","fifty7.csv","fifty8.csv","fifty9.csv","fifty10.csv")

  needDwd <-subset(datasetName, !datasetName %in% list.files(fiftyDir))
  if (length(needDwd)> 0){
    stop(paste(needDwd, " is/are missing from", fiftyDir, " You need to run dwdBBS function with param extract = TRUE"))
  }
  
  if (missing(lookupTables)) {
    stop("You must provide a list of path to access lookupTables")
  }
  
  if (missing(survey_tbl)) {
    stop("You must provide the output from bbs2pc_visit to extract foreign key ")
  }
  #Create temporal range
  rangeYear = c()
  rangeYear <- c(rangeYear, minYear:maxYear)
  
  # Read lookupTables to extract acronym
  regionCodes <- read.csv(file = unlist(lookupTables[names(lookupTables)=='regionCodes']), header = TRUE, sep=",")

  # Read and process fifty according to geographical and temporal range
  readFifty <- lapply(datasetName, function(x){
                  
                  path2fifty = file.path(normalizePath(fiftyDir),x)
                  fifty <- read.csv(file = path2fifty, header = TRUE)
                  
                  # Subset temporal range
                  fiftyincluded <-subset(fifty, fifty$Year %in% rangeYear)
                  
                  if(!nrow(fiftyincluded)==0){
                    fiftygather <- gather(fiftyincluded, key=Stop_no, value = count, Stop1:Stop50)
                    #---------
                    ## Delete 0's and RPID >200
                    #---------
                    if (dropZeros) {
                      fiftygather<- fiftygather[which(fiftygather$count!=0), ] 
                    }  
                    # RPID < 200 Does meet BBS weather, date, time, or route completion criteria
                    fiftygather <-subset(fiftygather, fiftygather$RPID < 200)
                     
                    #---------
                    ## Create a join field to merge pc_visit and recover survey_fk
                    #---------
                    #dataset_name
                    #recover acronym from regionCodes
                    fifty_BAM <-merge(fiftygather, regionCodes[, c("StateNum", "CountryNum","Acronym")], by=c("StateNum","CountryNum"))
                    fifty_BAM$dataset_name <- paste0("BBS",fifty_BAM$Acronym)
                    #location_name  
                    fifty_BAM$location_name <- paste0(fifty_BAM$dataset_name,":",fifty_BAM$Route,":",fifty_BAM$Stop_no)
                    # Assign visit
                    fifty_BAM$round <- sub(".*(\\d+$)", "\\1", fifty_BAM$RPID)
                    #PKEY  SS:YY:1 (2-digit year and each visit was round 1 for that year)
                    fifty_BAM$PKEY <- paste0(fifty_BAM$location_name,":",substr(fifty_BAM$Year,3,4),":",fifty_BAM$round)
                    #---------
                    #---------------------
                    #----OUTPUT
                    #---------------------
                    ## Subset geographical range with the assignation of location_fk
                    fifty_BAM <- merge(fifty_BAM, survey_tbl[, c("survey_id", "PKEY")], by.x="PKEY", by.y="PKEY")
                    fifty_BAM$survey_fk <- fifty_BAM$survey_id
                    #---------
                    ##pc_auto_id
                    fifty_BAM$detection_id <- seq.int(nrow(fifty_BAM))
                    #---------
                    #Species
                    spList <- read_excel(unlist(lookupTables[names(lookupTables)=='speciesList']), sheet = "BAM_Avian_Species")
                    fifty_BAM$detection_species_code <- spList$BAM_Species_Code[match(fifty_BAM$AOU, spList$BBS_Number)]
                    fifty_BAM$detection_original_species <-""
                    #---------
                    #Distance (0-400m)
                    fifty_BAM$detection_distanceband <- 40
                    #---------
                    #Duration (3mins)
                    fifty_BAM$detection_durationinterval <- 5
                    #---------
                    #ABUND
                    fifty_BAM$detection_abundance <- fifty_BAM$count
                    #---------
                    #DETECTION
                    fifty_BAM$detection_detected <- "Yes"
                    #---------
                    #HEARD
                    fifty_BAM$detection_heard <- "DNC"
                    #---------
                    #SEEN
                    fifty_BAM$detection_seen <- "DNC"
                    #---------
                    #comments
                    fifty_BAM$pc_comments <- ""
                    #---------------------
                   
                    # Order output columns 
                    col_order <- c("detection_id", "survey_fk", "detection_species_code", "detection_original_species", "detection_distanceband", "detection_durationinterval", 
                                   "detection_abundance", "detection_detected", "detection_heard", "detection_seen", "pc_comments", "PKEY")
                    fifty2_detection <- fifty_BAM[, col_order]
                    #---------

                    return(fifty2_detection)
                  }
  })
}
  
