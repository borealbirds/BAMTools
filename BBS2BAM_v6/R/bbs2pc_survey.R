##################################################################################
#' Convert BBS weather file into BAM PKEY table
#'
#' The function uses weather.csv and to create BBS PKEY table according to the BAM standard
#'
#' @param fweather A character string giving the path where the downloaded file is saved.
#'
#' @param lookupTables A list of path to access lookupTables. 
#'
#' @param minYear Numerical giving the lower bound of year to be included. Default is \code{1965}.
#' 
#' @param maxYear Numerical giving the upper bound of year to be included. Default is \code{2019}.
#'
#' @param pc_location A location table from which foreign_key is derived.
#' 
#' @return Data frame representing PKEY table
#'
#' @importFrom utils read.csv
#' @importFrom readr parse_number
#' @importFrom chron times hours minutes
#' @importFrom tidyr gather 
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname bbs2pc_survey
#' @examples
#' outDir <- "C:/data"
#' bbs2pc_survey(outDir, lookupTables,minYear=1965, maxYear=2019, pc_location)
bbs2pc_survey <- function(fweather, lookupTables, minYear=1965, maxYear=2019, pc_location) {
  
  # Check params
  if (missing(fweather)) {
    stop("You must provide path where weather file is located")
  }else{
    if (!file.exists(fweather)) {
      stop("File weather doesn't exist")
    }
  }
  if (missing(lookupTables)) {
    stop("You must provide a list of path to access lookupTables")
  }
  
  #Create year vector
  rangeYear = c()
  rangeYear <- c(rangeYear, minYear:maxYear)
  
  ####PROV
  #Load lookup table to extract Prov/States
  regionCodes <- read.csv(file = unlist(lookupTables[names(lookupTables)=='regionCodes']), header = TRUE, sep=",")

  # Read source weather file
  path2weather <- file.path(normalizePath(fweather))
  fweather <- read.csv(file = path2weather, header = TRUE)
  
  # Subset geographical range using pc_location 
  pc_location$acronym <- substr(pc_location$location_name, 4, 5)
  pc_location$StateNum <- regionCodes$StateNum[match(pc_location$acronym, regionCodes$Acronym)]
  pc_location$CountryNum <-regionCodes$CountryNum[match(pc_location$acronym, regionCodes$Acronym)]
  pc_location$Route <- sub(".*[:]([^.]+)[:].*", "\\1", pc_location$location_name)
  weatherincluded <-subset(fweather, fweather$CountryNum %in% pc_location$CountryNum & fweather$StateNum %in% pc_location$StateNum & fweather$Route %in% pc_location$Route)
  # Recover Prov/States acronym
  weathermerge <-merge(weatherincluded, regionCodes[, c("StateNum", "CountryNum","Acronym")], by=c("StateNum","CountryNum"))

  # Subset temporal range
  weather <-subset(weathermerge, weathermerge$Year %in% rangeYear)
  
  # subset protocol (RPID >199 are experimental)
  weather <-subset(weather, weather$RPID < 200)
  
  # Assign visit
  weather$survey_round <- sub(".*(\\d+$)", "\\1", weather$RPID)
  
  ### Create PCODE (prior to SS) to recover foreign key
  weather$dataset_name <- paste0("BBS",weather$Acronym)
  
  # Convert StartTime and EndTime
  # Mark 0 as NA
  weather$StartTime[weather$StartTime== 0] <- NA
  weather$EndTime[weather$EndTime== 0] <- NA
  # Convert StartTime
  leadTime_st <- mapply(function(x, y) paste0(rep(x, y, times = 1), collapse = ""), 0, 4 - nchar(weather$StartTime))
  weather$StartTime <- paste0(leadTime_st, weather$StartTime)
  weather$StartTime <- format(strptime(weather$StartTime, format="%H%M"), format = "%H:%M")
  weather$StartTime <- paste0(weather$StartTime, ":00")
  weather$StartTime <- invisible(times(weather$StartTime))
  # Convert EndTime
  leadTime_et <- mapply(function(x, y) paste0(rep(x, y, times = 1), collapse = ""), 0, 4 - nchar(weather$EndTime))
  weather$EndTime <- paste0(leadTime_et, weather$EndTime)
  weather$EndTime <- format(strptime(weather$EndTime, format="%H%M"), format = "%H:%M")
  weather$EndTime <- paste0(weather$EndTime, ":00")
  weather$EndTime <- times(weather$EndTime)

  ### Add stop1:stop50 prior to transpose the matrix
  x <- 1:50
  namevector <- paste0("Stop", x)
  weather[,namevector] <- NA
  # Transpose
  weathergather <- gather(weather, key=Stops, value =stop_no, Stop1:Stop50)
  weathergather$stop_no<- parse_number(weathergather$Stops)
  
  ###Output
  ##location_fk (subset the ones with XY)
  # Create location_name to recover location ID from pc_location
  weathergather$location_name <- paste0(weathergather$dataset_name,":",weathergather$Route,":",weathergather$Stops)
  weathergather <- merge(weathergather, pc_location[, c("location_id", "location_name")], by="location_name")
  location_fk <- weathergather$location_id
  ##visitautoid
  survey_id <- seq.int(nrow(weathergather))
  ##visit round
  survey_round <- weathergather$survey_round
  ##Year
  survey_year <- weathergather$Year
  ##Date
  date <- as.Date(with(weathergather, paste(Day,Month,Year, sep = "-")),format = "%d-%m-%Y")
  survey_date <- format(date,"%d-%b-%y")
  ##Protocol duration 4 (3 min)
  survey_protocol_duration <- 4
  ##Protocol distance 22 (0-400m)
  survey_protocol_distance <- 22
  ##Observer (internal ID number)
  survey_observer_code <- paste("BBS_", weathergather$ObsN)
  ##Database origin (BBS = 2)
  survey_origin_database <- 2
  
  ### Time incremental for 50 stop (Only when QualityCurrentID ==1, i.e., meet BBS weather, date, time, or route completion criteria).
  weathergather$time_int <- weathergather$EndTime - weathergather$StartTime
  weathergather$time_inc <- weathergather$time_int / 50
  weathergather$survey_time <- weathergather$StartTime + ((weathergather$stop_no - 1)*weathergather$time_inc)
  # Assign Time ==NA when does not meet one or more BBS weather, date, time, or route completion criteria
  weathergather[weathergather$QualityCurrentID==0, "Time"] <- NA
  survey_time <- weathergather$survey_time
  
  # Create PKEY to allow join later with PtCount table
  ###PKEY format  location_name:YY:round 
  PKEY <- paste0(weathergather$location_name,":",substr(weathergather$Year, 3, 4),":",weathergather$survey_round)
  
  # Order output columns 
  bbsVisit <- data.frame(survey_id,location_fk, survey_round, survey_year, survey_date,survey_time, survey_protocol_duration, survey_protocol_distance, survey_observer_code, survey_origin_database, PKEY)

  #---------
   return(bbsVisit)

}

