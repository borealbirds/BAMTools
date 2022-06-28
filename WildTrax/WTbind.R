library(utils) #read.csv, read.table
library(stringr) #str_match
library(sp) #CRS, coordinates, proj4string
library(sf) # st_as_sf, st_read, st_geometry

# Assemble WildTrax data
#There are 5 filename that use apostrophe or special characters in filename. The system do not recognize them. Need to change the name manually

wdir <- "path/to/unzip/folder/"

wt_data <- "name/of/folder"
wtList <- list.files(file.path(wdir, wt_data), pattern = ".report.csv")
wt_report <- lapply(wtList, function(x) {
  print(x)
  f_wt <- read.csv(file.path(wdir, wt_data, x), sep=",", header = TRUE, stringsAsFactors = FALSE)
  #f_aru <- read_csv(fi, sep=",")
  return(f_wt)
})
wt_bind <-do.call(rbind, wt_report) 

## Extract Status and Sensor from abstract
wtproject <- list.files(file.path(wdir, wt_data), pattern = ".abstract.csv")
wt_report <- lapply(wtproject, function(x) {
  f_wt<- read.table(file.path(wdir, wt_data, x),sep=",", allowEscapes=TRUE)
  org_extract <- str_match(f_wt, "Organization: \\s*(.*?)\\s*\n")[,2]
  org_extract <- gsub("/.*$","",org_extract)
  prj_extract <- str_match(f_wt, "Project Name: \\s*(.*?)\\s*\n")[,2]
  stat_extract <- str_match(f_wt, "Project Status: \\s*(.*?)\\s*\n")[,2]
  sensor_extract <- str_match(f_wt, "Sensor: \\s*(.*?)\\s*\n")[,2]
  data.frame(organization = org_extract, project = prj_extract , status = stat_extract, sensor = sensor_extract, stringsAsFactors=FALSE)
})
prj_tbl <- do.call(rbind, wt_report)

# Merge
wt_data <- merge(wt_bind, prj_tbl, by = c("project", "organization"), all.x = TRUE)


#####################
#--  MAP
#####################
# Delete NAs
wt_geo <-wt_data[!(is.na(wt_data$latitude)),]
wt_geo <-wt_geo[!(is.na(wt_geo$longitude)),]

# common projections
DD <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
LAEA <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

coordinates(wt_geo) <- c("longitude", "latitude")
proj4string(wt_geo) <- DD

pc <- st_as_sf(wt_geo, coords = c("longitude", "latitude"), crs = DD)


## Map Studyregion
f_studyregion <- "Path/to/studyregion/shapefile.shp"
studyregion <- st_read(f_studyregion)
plot(st_geometry(studyregion))

## Add point (ARU, PC or PC2 (pending to WildTrax))
sensor <- pc$sensor
plot(st_geometry(pc), pch  = 20, col=ifelse(sensor=="ARU", "red",ifelse(sensor== "PC", "black", "blue")), add= TRUE)

