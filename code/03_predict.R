
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(terra)
library(RemoteSensing) # for ndvi calculation

# Set parallel computing environment --------------------------------------
# library(parallel)
# library(doParallel)
# library(foreach)
# cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
# registerDoParallel(cluster)


# Load in trained model -------------------------------------------------------

  
  # load("./output/2022_08_4band_FCATtoCachi - balanced - 2022_04_03/2022_08_4band_FCATtoCachi - balanced - 2022_04_03.Rdata") ## test run for 2022_08
  # load("./output/2019_09_4band_FCATtoCachi - balanced - 2022_04_04/2019_09_4band_FCATtoCachi - balanced - 2022_04_04.Rdata")
  # load("./output/2022_08_4band_FCATtoCachi - balanced - 2022_04_18/2022_08_4band_FCATtoCachi - balanced - 2022_04_18.Rdata") # After adding more clouds to training set
  # load("./output/2019_09_4band_FCATtoCachi - 2022_04_19/2019_09_4band_FCATtoCachi - 2022_04_19.Rdata") # Testing out not using balanced dataset

# Load in merged Raster to predict on ------------------------------------------

  # Load raster
  ras <- terra::rast(path_to_merged_raster)
  
  # Set layer names
  names(ras) <- four_band_names  
    
  # Convert to matrix for prediction
  ras_dat <- values(ras, dataframe = TRUE) 
  object.size(ras_dat)/1e6 # in megabytes
  
  
# Calculate vegetation indices --------------------------------------------

  ras_dat <- ras_dat %>%
    mutate(ndvi = ndvi(red, nir),
           savi = savi(red, nir),  # Soil-Adjusted Vegetation Index
           arvi = arvi(red, nir, blue) # Atmosphere Antivegetation Index
    )

  head(ras_dat)

  
# Predict across entire raster --------------------------------------------
  
# Cut prediction raster into slices to speed things up and reduce amount of memory needed
  n_cuts = 10
  
  cuts <- round(seq(1, nrow(ras_dat), len = n_cuts))
  cut_starts <- cuts[1:(n_cuts - 1)]
  cut_ends <- cuts[2:n_cuts] - 1
  cut_ends[(n_cuts-1)] <- cut_ends[(n_cuts-1)] + 1
  
  cut_starts
  cut_ends
  
  # Initialize output data frame
  pred_out <- tibble(pixelID = 1:nrow(ras_dat),
                     top_pred = NA,
                     confidence = NA)
  
  
# Loop over intervals  
for(x in 1:(n_cuts - 1)) {
    
  cat("Working on cut: ", x, " ... \n" )
  
  # Set up new data frame
  newdata <- as.matrix(ras_dat[cut_starts[x]:cut_ends[x], c(predictors)])

  # Predict on model
  cat("Predicting ... \n" )
  
  pred_all <- predict(boost, 
                     newdata = newdata,
                     reshape = TRUE)
  # pred_all
  
  # Transform to tibble
  pred_all <- as_tibble(pred_all)
  # head(pred_all)
  
  cat("Choosing top prediction for each pixel ... \n" )
  top_pred <- apply(pred_all, MARGIN = 1, function(x) which.max(x))
  confidence <- apply(pred_all, MARGIN = 1, function(x) max(x))

  # Replace NAs
  top_pred[is.na(newdata[, 1])] <- NA # Will make NA 
  
  # Join with output dataset
  cat("Joining with output ... \n" )
  pred_out$top_pred[cut_starts[x]:cut_ends[x]] <- top_pred
  pred_out$confidence[cut_starts[x]:cut_ends[x]] <- confidence

  # Cleanup
  rm(newdata)
  rm(pred_all)
  rm(top_pred)
  rm(confidence)

} # End loop over cuts

# Counts of classes
table(pred_out$top_pred)

## Replace data into raster
ras_out <- ras[[c(1,2)]] # Initialize with just one layer to save memory

# Add in predictions
values(ras_out[[1]]) <- as.numeric(factor(pred_out$top_pred)) 

# Add in confidence
values(ras_out[[2]]) <- pred_out$confidence

# Reset names
names(ras_out) <- c("class", "confidence")


# Write raster to file ----------------------------------------------------

  writeRaster(ras_out, paste0("output/", run_name, "/", run_name, ".tif"),
              overwrite=TRUE)


