#################################
#
# This script aim to download WT data in point count format. 
# The script necessitate to source a config file that contains:
##   --WildTrax authentification :  Sys.setenv(WT_USERNAME = 'yourusername', WT_PASSWORD = 'yourpassword') 
##   --working directory : wd <- "put/your/path/where/download/occur"
##   --Project exclusion : exclusion <- "point/to/a/csv/if/some/project/shouldnt/be/downloaded"
#
# The script can based the comparison using the project_id (fast = TRUE) or checksum (fast= FALSE) on the file. Checksum allow to redownload projects that changed overtime in WildTrax. 
#################################
#remotes::install_github("ABbiodiversity/wildRtrax")
library(wildrtrax)
library(ggplot2)
library(data.table)
library(sf)
library(dplyr)
library(stringr)

## Config set : wd, exclusion, WildTrax authentification, WT_dir
config_path <- "E:/MelinaStuff/BAM/WildTrax/WTdownload"
config <- file.path(config_path, "config.R")
source(config)
wt_auth(force = TRUE)

#A. DOWNLOAD DATA FROM WILDTRAX#######################
#1. Get list of projects from WildTrax----
#sensor = PC gives all ARU and point count projects
project.list <- wt_get_download_summary(sensor_id = 'PC')

#2. Query the ones of interest
projects <- project.list %>% 
  dplyr::filter(!(project_id %in% exclusion$project_id))

#3. Get list of project_id already download----
dwd.list <- Sys.glob(file.path(wd, "*.csv")) %>%
  basename() %>%
  sub("_.*", "", .) %>%
  as.integer()

#4. Apply fast switch
#If switch = FALSE, checksum will be compute to test if file changed
fast <-  TRUE
translate <- FALSE

#5. Loop through projects to download data----
for(i in 1:nrow(projects)){
  if(fast){
    if(!(projects$project_id[i] %in% dwd.list)){
      print(paste0("Process dataset ", projects$project[i], " : ", i, " of ", nrow(projects), " projects"))
      dat.try <- try(wt_download_report(project_id = projects$project_id[i], sensor_id = projects$sensor[i], weather_cols = F, report="main"))
      if(any(class(dat.try)=="data.frame")){
        project_name <- gsub("[/\\\\:*?\"<>|]", "-", projects$project[i])
        write.csv(dat.try, file = file.path(wd, paste0(projects$project_id[i], "_", project_name, ".csv")), append = FALSE, row.names = FALSE)
      }
      translate <- FALSE
    }else{
      print(paste0("Dataset ", projects$project[i], " was already downloaded : ", i, " of ", nrow(projects)))
      translate <- FALSE
    }
  }else{
    dat.try <- try(wt_download_report(project_id = projects$project_id[i], sensor_id = projects$sensor[i], weather_cols = F, report="main"))
    translate <- TRUE
  }
  # Translate using checksum
  if(translate==TRUE){
    if(any(class(dat.try)=="data.frame")){
      if(file.exists(file.path(wd, paste0(projects$project[i], ".csv")))){
        f <- read_csv(file.path(wd, paste0(projects$project[i], ".csv")), show_col_types = FALSE)
        f$survey_date <- as.POSIXct(f$survey_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        g <- f %>% as_tibble()
        g <- g[, order(names(g))]
        attributes(g) <- NULL
        dat.try <- dat.try %>% as_tibble()
        dat.try <- dat.try[, order(names(dat.try))]
        attributes(dat.try) <- NULL
        if(digest(g, "md5") != digest(dat.try, "md5")){
          print(paste0("Dataset ", projects$project[i], " changed since last downloaded: ", i, " of ", nrow(projects), ". Archived old version."))
          project_name <- gsub("[/\\\\:*?\"<>|]", "-", projects$project[i])
          write.csv(g, file = file.path(wd, "archived", paste0(projects$project_id[i], "_", project_name, ".csv")), row.names = FALSE)
          write.csv(dat.try, file = file.path(wd, paste0(projects$project_id[i], "_", project_name, ".csv")), append = FALSE, row.names = FALSE)
          translate <- FALSE
        }else{
          print(paste0("Dataset ", projects$project[i], " was already downloaded : ", i, " of ", nrow(projects)))
          translate <- FALSE
        }
      }else{
        write.csv(dat.try, file = file.path(wd, paste0(projects$project[i], ".csv")), row.names = FALSE)
        print(paste0("Finished dataset ", projects$project[i], " : ", i, " of ", nrow(projects), " projects"))
        translate <- FALSE
      }
    }
  }
}

