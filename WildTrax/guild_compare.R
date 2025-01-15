#guild_compare
#' The function aims to compare a new dataset to what is already in WildTrax. 
#'
#' @param guild dataframe. A dataframe of combined WT report (main).
#'
#' @param new.project dataframe. A dataframe new point count data. The dataframe required location, latitude, 
#'                               longitude, survey_date, species_code and individual_count to be present. 
#'
#' @param type_test character; Indicate the way dataset will be compared.  type_test = visit uses latitude, longitude, survey_date; 
#'                              type_test = survey uses latitude, longitude, survey_date, total_birds; 
#'                              type_test = guild uses latitude, longitude, survey_date, total_birds, unique_species, bird_list.
#'                             
#'# guild_compare
guild_compare <- function(guild, test.project, type_test) { 
  required_cols <- c("location", "latitude", "longitude", "survey_date", "species_code", "individual_count")
  missing_cols_test <- setdiff(required_cols, colnames(test.project))
  missing_cols_new <- setdiff(required_cols, colnames(guild))
  
  # Raise a flag if there are missing columns
  if (length(missing_cols_test) > 0) {
    warning(paste("The following required columns are missing:", 
                  paste(missing_cols_test, collapse = ", ")))
    return(FALSE) # Flag failure
  }
  # Raise a flag if there are missing columns
  if (length(missing_cols_new) > 0) {
    warning(paste("The following required columns are missing:", 
                  paste(missing_cols_new, collapse = ", ")))
    return(FALSE) # Flag failure
  }
  
  wt.guild <- guild %>%
    filter(latitude >0) %>%
    group_by(project, latitude, longitude, survey_date) %>%
    reframe(
      total_birds = sum(as.numeric(individual_count), na.rm = TRUE),        # Total number of birds detected
      unique_species = n_distinct(species_code),              # Number of unique species detected
      bird_list = paste(sort(unique(species_code)), collapse = "_")
    ) %>%
    filter(total_birds > 0) # Drop 0
    
  test.guild <- test.project %>%
    group_by(latitude, longitude, survey_date) %>%
    reframe(
      total_birds = sum(as.integer(individual_count), na.rm = TRUE),        # Total number of birds detected
      unique_species = n_distinct(species_code),              # Number of unique species detected
      bird_list = paste(sort(unique(species_code)), collapse = "_")
    ) %>%
    filter(total_birds > 0) # Drop 0
  
  if(type_test == "visit"){
    guild_select <- wt.guild %>% 
      select(latitude, longitude, survey_date) %>%
      mutate(survey_date = as.character(survey_date))
    n_select <- test.guild %>%
      select(latitude, longitude, survey_date)
  } else if(type_test == "survey") {
    guild_select <- wt.guild %>% 
      select(latitude, longitude, survey_date, total_birds) %>%
      mutate(survey_date = as.character(survey_date))
    n_select <- test.guild %>%
      select(latitude, longitude, survey_date, total_birds)
  } else if(type_test == "guild"){
    guild_select <- wt.guild %>% 
      select(latitude, longitude, survey_date, total_birds, unique_species, bird_list) %>%
      mutate(survey_date = as.character(survey_date))
    n_select <- test.guild %>%
      select(latitude, longitude, survey_date, total_birds, unique_species, bird_list)
  }
  # Perform an inner join to find matches
  rows_in_A <- suppressMessages(semi_join(n_select, guild_select))
  matching_projects <- dplyr::inner_join(wt.guild, rows_in_A, by = colnames(guild_select))
  
  # Extract the unique projects
  result <- matching_projects %>%
    dplyr::select(project) %>%
    dplyr::distinct()
  # Convert the column to a vector
  projects_vec <- result$project
  message(paste("Project has ", as.character(nrow(rows_in_A)), "pc on a total of", as.character(nrow(test.guild)), "  already in WildTrax. Duplicate rows are found in ", projects_vec ))
  return(matching_projects)
}

##### Workflow
library(data.table)
library(wildrtrax)
library(dplyr)


# Set params
## Config set : wd, exclusion, WildTrax authentification, WT_dir
path2config <- "E:/MelinaStuff/BAM/WildTrax/WTdownload"
config <- file.path(path2config, "config.R")
source(config)


# RUN
project.list <- wt_get_download_summary(sensor_id = 'PC')
# set test df
test <- read.csv("E:/MelinaStuff/BAM/WildTrax/WTdownload/download/840_Labrador Highlands Research Group Point Counts 2007.csv")

#STEP 1. Get list of projects from WildTrax----
project.list <- wt_get_download_summary(sensor_id = 'PC')

#STEP 2. Query the ones of interest
projects <- project.list %>% 
  dplyr::filter(!(project_id %in% exclusion$project_id))


#STEP 3: 2 choices: 1) extract data from WT; 2) point to repo that contain downloaded data on local computer.
#STEP 3.1. Loop through projects to download data----
dat.list <- list()
for(i in 1:nrow(projects)){
      print(paste0("Process dataset ", projects$project[i], " : ", i, " of ", nrow(projects), " projects"))
      dat.try <- try(wt_download_report(project_id = projects$project_id[i], sensor_id = projects$sensor[i], weather_cols = F, report="main"))
      if(any(class(dat.try)=="data.frame")){
        dat.list[[i]] <- dat.try
      }
}


#STEP3.2. Loop through csv and append---- 
dwd.files <- Sys.glob(file.path(wd, "*.csv"))
reportList <- lapply(dwd.files, read.csv)

all.wt <- rbindlist(reportList, fill=TRUE)%>%
  filter(!is.na(individual_count)) %>%
  mutate(sensor = project.list$sensor[match(project_id, project.list$project_id)],
         status = project.list$status[match(project_id, project.list$project_id)],
         individual_count = case_when(is.na(individual_count) ~ "0",
                           grepl("[A-Za-z]", individual_count) ~ "0.1",
                           grepl("^[0-9]+$", individual_count) ~ individual_count)
  )
           

#STEP 4. Compare
e<-guild_compare(all.wt, test, type_test = "guild")


