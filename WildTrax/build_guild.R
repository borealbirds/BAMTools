##################################################################################
#' build_guild
#' 
#' Summarize site-specific observation
#'
#' @param reportList list. A list of WT report.
#'
#' @param project.list dataframe. A dataframe listing the projects that the user can download data on WildTrax.
#'
#' @param layer character; Name of the output layers of interest, either "mean", "sd" or "overextrapolated".
#'
#' @param crop logical. If \code{TRUE}, crop raster layers using \code{ext/}.
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' 
#'# guild_build
library(data.table)
guild_build <- function(reportList, project.list) { 
  
  all.wt <- rbindlist(reportList, fill=TRUE)%>%
    mutate(sensor = project.list$sensor[match(project_id, project.list$project_id)],
           status = project.list$status[match(project_id, project.list$project_id)],
           count = ifelse(individual_count =="TMTC", "0.1",individual_count)) %>%     #Flag TMTC
    group_by(organization, project, project_id, location, latitude, longitude, survey_date) %>%
    summarise(
      total_birds = sum(as.integer(count), na.rm = TRUE),        # Total number of birds detected
      unique_species = n_distinct(species_code),              # Number of unique species detected
      bird_list = paste(sort(unique(species_code)), collapse = "_"),
      .groups = "drop"                                   # Prevents unnecessary grouping
    ) %>%
    filter(total_birds > 0) # Drop 0
}

#guild_compare
# visit : lat-long-surveyDateTime
# survey : latitude, longitude, surveyDateTime, total_birds
# guild: latitude, longitude, surveyDateTime, total_birds, unique_species, bird_list
guild_compare <- function(guild, new.project, type_test) { 
  
  required_cols <- c("location", "latitude", "longitude", "surveyDateTime", "species", "individual_count")
  missing_cols <- setdiff(required_cols, colnames(new.project))
  
  # Raise a flag if there are missing columns
  if (length(missing_cols) > 0) {
    warning(paste("The following required columns are missing:", 
                  paste(missing_cols, collapse = ", ")))
    return(FALSE) # Flag failure
  }
  
  n <- new.project %>%
    group_by(latitude, longitude, surveyDateTime) %>%
    summarise(
      total_birds = sum(as.integer(ind_count), na.rm = TRUE),        # Total number of birds detected
      unique_species = n_distinct(species),              # Number of unique species detected
      bird_list = paste(sort(unique(species)), collapse = "_"),
      .groups = "drop"                                   # Prevents unnecessary grouping
    ) %>%
    filter(total_birds > 0) # Drop 0
  
  if(type_test == "visit"){
    guild_select <- guild %>% 
      rename(surveyDateTime = survey_date) %>%
      select(latitude, longitude, surveyDateTime)
    n_select <- n %>%
      select(latitude, longitude, surveyDateTime)
    # Check rows in B that are in A
    rows_in_A <- semi_join(n_select, guild_select)
    # Check if all rows in B are in A
    all_rows_in_A <- nrow(rows_in_A) == nrow(n_select)
  }
  guild_select <- guild %>% 
    rename(surveyDateTime = survey_date) %>%
    select(latitude, longitude, surveyDateTime, total_birds, unique_species, bird_list)
  rows_in_guild <- apply(n, 1, function(row) any(apply(guild_select, 1, function(x) all(x == row))))
  
}


rows_in_A <- n %>% 
  rowwise() %>% 
  mutate(is_in_A = list(c_across(everything())) %in% split(guild_select, seq_len(nrow(guild_select))))



# Source path
path <- "E:/MelinaStuff/BAM/WildTrax/WTdownload"
config <- file.path(path, "config.R")

## Config set : wd, exclusion, WildTrax authentification, WT_dir
source(config)
project.list <- wt_get_download_summary(sensor_id = 'PC')

dwd.files <- Sys.glob(file.path(wd, "*.csv"))
reportList <- lapply(dwd.files, read.csv)

guild <-guild_build(reportList, project.list)




# test
test <- read.csv("E:/MelinaStuff/BAM/WildTrax/WTdownload/download/840_Labrador Highlands Research Group Point Counts 2007.csv")
sample <- list(test)
guild <-guild_build(sample, project.list)

n <- data_flat
  

