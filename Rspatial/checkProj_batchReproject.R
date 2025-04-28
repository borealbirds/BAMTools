library(terra)
library(fs)

root_folder <- "G:/Shared drives/BAM_NationalModels5/PredictionRasters"

target_crs <- crs("EPSG:5072")

tif_files <- list.files(root_folder, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

non_matching <- list()

# Check each file
for (file in tif_files) {
  try({
    r <- rast(file)
    file_crs <- crs(r)
    if (file_crs != target_crs) {
      non_matching[[file]] <- file_crs
    }
  }, silent = TRUE)
}

# Show the list of files with mismatched CRS
if (length(non_matching) > 0) {
  message("Files with non-matching CRS:")
  print(names(non_matching))
} else {
  message("All files have the matching CRS.")
}


# Save elsewhere
out_folder <- "E:/MelinaStuff/BAM/NationalModelv5.0/PredictionRasters2"

for (infile in tif_files) {
  try({
    rel_path <- path_rel(infile, start = root_folder)
    
    # Build output file path
    outfile <- path(out_folder, rel_path)
    dir_create(path_dir(outfile))
    
    r <- rast(infile)
    
    if (crs(r) != target_crs) {
      r <- project(r, target_crs)
    }
    
    writeRaster(r, outfile, overwrite = TRUE)
    
    cat("Processed:", rel_path, "\n")
  }, silent = TRUE)
}
