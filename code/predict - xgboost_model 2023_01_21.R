
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(sf)
library(raster)


# Load in merged Raster ---------------------------------------------------


## Load previous model run
load("./output/model_run - 8 band December 2022 2023-01-22.Rdata") ## 8 band run for Deceber 2022

  # 8 band raster
  ras <- raster::brick(path_to_raster)
  
  # Set layer names
  names(ras) <- band_names
  
  # Convert to matrix for prediction
  ras_dat <- as.matrix(ras) 

  
  
# Predict across entire raster --------------------------------------------
  
# Cut prediction raster into slices to speed things up
  
  n_cuts = 30
  
  cuts <- round(seq(1, nrow(ras_dat), len = n_cuts))
  cut_starts <- cuts[1:(n_cuts - 1)]
  cut_ends <- cuts[2:n_cuts] - 1
  cut_ends[(n_cuts-1)] <- cut_ends[(n_cuts-1)] + 1
  
  cut_starts
  cut_ends
  
  # Initialize output dataframe
  pred_out <- tibble(pixelID = 1:nrow(ras_dat),
                     top_pred = NA)
  
  
# Loop over intervals  
for(x in 1:(n_cuts - 1))  {
  
  cat("Working on cut: ", x, " ... \n" )
  
  # Set up new dataframe
  newdata <- as.matrix(ras_dat[cut_starts[x]:cut_ends[x], c(band_names)])

  # Predict on model
  cat("Predicting ... \n" )
  
  pred_all <- predict(boost, 
                     newdata = newdata,
                     reshape = TRUE)
  pred_all
  
  # Transform to tibble
  pred_all <- as_tibble(pred_all)
  # head(pred_all)
  
  # Add column names
  # colnames(pred_all) <- levels(factor(dat_train$class))
  # head(pred_all)
  
  # Save for later
  # pred_all_colnames <- colnames(pred_all)

  ## Accuracy - take top probability for each pixel
  # pred_all <- pred_all %>%
  #   mutate(top_pred = apply(pred_all, MARGIN = 1, function(x) pred_all_colnames[which.max(x)]),
  #          pixelID =  cut_starts[x]:cut_ends[x]
  # )
  
  cat("Choosing top prediction ... \n" )
  pred_all <- pred_all %>%
    mutate(top_pred = apply(pred_all, MARGIN = 1, function(x) which.max(x)),
           pixelID =  cut_starts[x]:cut_ends[x]
  )
  
  
  # Replace NAs
  pred_all[which(is.na(newdata[, 1])), "top_pred"] <- NA # Will make NA 
  
  # Join with output dataset
  cat("Joining with output ... \n" )
  pred_out$top_pred[cut_starts[x]:cut_ends[x]] <- pred_all$top_pred
  
  # Cleanup
  rm(newdata)
  rm(pred_all)
  

} # End loop over cuts


### Add in Shadow and Cloud classes from UDM rasters identified already by Planet - not perfect, maybe better to use XGboost?
# shadow_indexes <- udm_dat$cellindex[udm_dat$shadow == 1]
# cloud_indexes <- udm_dat$cellindex[udm_dat$cloud == 1]
# 
# pred_out$top_pred[shadow_indexes] <- "shadow"
# pred_out$top_pred[cloud_indexes] <- "cloud"

# Counts of classes
table(pred_out$top_pred)

## Replace data into raster
ras_out <- ras

rm(ras_dat) # To clear up memory for next step

values(ras_out) <- as.numeric(factor(pred_out$top_pred)) # Could add distinct levels here to make sure values are right

ras_out <- ras_out[[1]]
names(ras_out) <- "class"

## Write out raster

writeRaster(ras_out, paste0("output/land cover ", Sys.Date(), ".tif"), overwrite=TRUE)


