##############################################################
##############################################################
##      Batch download relative density estimate
##############################################################
##############################################################
library(RCurl)

#List birds table **** Note the list is incomplete. need to be review
birdtbl <- "C:/MelinaStuff/Contracts/BAM/geonetwork/GNpackage/R/birdAcronyme.csv"
bird <- read.csv(birdtbl, sep="\t", header=TRUE)
lsbird <- as.vector(bird[,1])

BAMwebsite <- "http://www.borealbirds.ca/files/results/relative_density_estimates"
jpgObj <- "XXXX_Density_April30_2012.jpg"
outpath <- "C:/MelinaStuff/Contracts/BAM/geonetwork/toLoad/RelativeDensity"
  
sapply(lsbird,function(x) {
          jpgObj_updated <- gsub("XXXX",x,jpgObj)
          toDwd <- file.path(BAMwebsite,jpgObj_updated)
          doExists <- url.exists(file.path(BAMwebsite,jpgObj_updated))
          if(doExists){
            download.file(toDwd, file.path(outpath, jpgObj_updated), method = "auto", mode = "wb")
          }else {
            print (x)
          }   
})


