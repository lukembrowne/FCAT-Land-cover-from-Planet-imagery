

# Load libraries ----------------------------------------------------------

  library(terra)
  library(tidyverse)

# Load in GFW alerts - https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/
# https://data.globalforestwatch.org/datasets/gfw::integrated-deforestation-alerts/about
    alerts <- terra::rast("../../../Satellite imagery/GFW Deforestation alerts/10N_080W.tif")
    alerts
    
    # Clip to area of interest
    crop_boundary <- terra::vect("../../../Satellite imagery/Planet imagery/Planet area of interest 2023_01_19.kml")
    alerts_cropped <- terra::crop(alerts, crop_boundary)
  
  
  # Convert data
  # Each pixel (alert) encodes the date of disturbance and confidence level in one integer value. The leading integer of the decimal representation is 2 for a low-confidence alert, 3 for a high-confidence alert, and 4 for an alert detected by multiple alert systems, followed by the number of days since December 31, 2014. 0 is the no-data value. 
  
    baseline <- as.Date("2014-12-31")
    ras1 <- as.Date("2019-09-03")
    ras2 <- as.Date("2022-08-15")
    
    start_thresh <- as.numeric(difftime(ras1, baseline, units = "days"))
    start_thresh
    start_threshes <- as.numeric(paste0(c(2,3,4), start_thresh))
    start_threshes
    
    end_thresh <- as.numeric(difftime(ras2, baseline, units = "days"))
    end_thresh
    end_threshes <- as.numeric(paste0(c(2,3,4), end_thresh))
    end_threshes
    
  
    # Extract raster values  
    vals <- tibble(value = c(values(alerts_cropped)))
    vals
    
    vals <- vals %>%
            mutate(alert = case_when(
                    value >= start_threshes[1] & value <= end_threshes[1]  ~ 1,
                    value >= start_threshes[2] & value <= end_threshes[2] ~ 1,
                    value >= start_threshes[3] & value <= end_threshes[3] ~ 1,
            ))
    
    vals
    table(vals$alert)
    
    # Replace values into raster
    values(alerts_cropped) <- vals$alert
    
  
  # Write to file
  writeRaster(alerts_cropped, 
              "../../../Satellite imagery/GFW Deforestation alerts/10N_080W_cropped.tif",
              overwrite=TRUE)
  
  
  

# Load in Hansen tree loss data by year -------------------------------------------

  
  # Load in GFW alerts
  lossbyyear <- terra::rast("../../../Satellite imagery/Hansen_ForestLossByYear/Hansen_GFC-2021-v1.9_lossyear_10N_080W.tif")
  lossbyyear
  
  # Clip to area of interest
  crop_boundary <- terra::vect("../../../Satellite imagery/Planet imagery/Planet area of interest 2023_01_19.kml")
  lossbyyear_cropped <- terra::crop(lossbyyear, crop_boundary)
  
  # Convert data
  # 
  lossbyyear_cropped[!(lossbyyear_cropped %in% c(19,20,21,22))] <- NA
  lossbyyear_cropped[(lossbyyear_cropped %in% c(19,20,21,22))] <- 1
  plot(lossbyyear_cropped)
  
  
  # Write to file
  writeRaster(lossbyyear_cropped, 
              "../../../Satellite imagery/Hansen_ForestLossByYear/Hansen_GFC-2021-v1.9_lossyear_10N_080W_cropped_processed.tif",
              overwrite=TRUE)
  
  
  
  
