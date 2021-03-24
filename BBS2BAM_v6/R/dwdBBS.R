#' Download BBS Fifty from ftp site.
#'
#' To avoid downloading existing files, the function verify if file exists locally prior to download.
#' When file exists, the function compare checksum value from checksum logged from original download.
#'
#' @param urls A character string. Represents the url of file to be downloaded.
#' 
#' @param urlData A character string. Representing the URL to access the zipped Fifty.
#' 
#' @param datasetName A character string. Name of the dataset to download.
#'
#' @param outDir A character string giving the path where the downloaded file is saved.
#'
#' @param extract Logical. If \code{TRUE}, file is untar and/or unzip. Default is \code{FALSE}.
#' 
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils download.file
#' @importFrom digest digest
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname dwdBBS
#' @examples
#' url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use"
#' dataList <- "land_use/L040J03.zip"
#' dwdBBS(url = url, destfile = tempdir(), extract = FALSE)
dwdBBS <- function(urls, urlData, datasetName, outDir, extract = FALSE) {
  
  #---Test function params
  if (missing(outDir)) {
    outDir <- file.path(cwd,"data")
  }

  #Create outdir
  if (!file.exists(outDir)) {
    dir.create(outDir, showWarnings = FALSE)
    message(paste("Output folder doesn't exist. ",datasetName, " will be download."))
    bbsDownload <- TRUE
    localcheck <- FALSE
  }
  if (is.null(urlData)) {
    stop("You must provide the url to access the file in the ScienceBase Catalog.")
  }
  if (is.null(datasetName)) {
    stop("You must provide the name of the local zip file previously download to allow checksum comparison.")
  }
  
  localFifty <- file.path(outDir,datasetName)
  if (!file.exists(localFifty)) {
    message(paste(datasetName, " is missing, we can't check if fifty files are up to date. Need to download."))
    bbsDownload <- TRUE
    localcheck <- FALSE
  }else{
    localcheck <- TRUE
  }
  
  # Compare existing .zip with online
  if (localcheck){
    webFifty = paste(urls, urlData, sep = "")
    shl = paste0('curl -sL ', webFifty, '|md5sum')
    checksum = shell(shl, intern = T)
    urlchecksum = gsub("[ *-]", "", checksum)
  
    localchck = digest(localFifty, file = TRUE)
    if (!localchck == urlchecksum) {
      message(paste(datasetName, " is different. Local version is not up to date. Need to download."))
      bbsDownload <- TRUE
    }else{
      message(paste(datasetName, " is up to date. No download needed."))
      bbsDownload <- FALSE
    }
  }

  #DOWNLOAD
  if (bbsDownload) {
    download.file(file.path(paste(urls, urlData)), localFifty, method = "auto", mode = "wb")
  }

  # UNZIP if extract = TRUE will unzip all files
  if (extract) {
    unzip(localFifty, exdir= outDir)
    folderName <- sub("(.*)\\..*$","\\1", basename(datasetName))
    version <- list.files(file.path(outDir,folderName))
    flist <- list.files(file.path(outDir,folderName,version))
    lapply(flist,  function(x) {unzip(file.path(outDir,folderName,version,x), exdir= outDir)})
  }
}
