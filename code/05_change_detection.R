
# Load libraries ----------------------------------------------------------

  library(terra)

# Try reclassifying the rasters to a different scale of numbers, such as
# 
# urban = 1
# ag = 10
# forest = 100
# water = 1000
# Then just subtract the rasters in Raster Calculator:
#   
#   0 will mean unchanged.
# 9 will mean ag --> urban
# -9 will mean urban --> ag
# 99 will mean forest --> urban
# 90 will mean forest --> ag
# -99 will mean urban --> forest etc.



# Load in rasters ---------------------------------------------------------

ras2019 <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2022_04_20/2019_09_4band_FCATtoCachi - 2022_04_20_sieved100.tif")
ras2022 <- terra::rast("./output/2022_08_4band_FCATtoCachi - 2022_04_19/2022_08_4band_FCATtoCachi - 2022_04_19_sieved100.tif")



#  Convert raster values to: ----------------------------------------------

  # forest = 1
  # non-forest = 10
  # cloud = 100
  # shadow = 1000
  #
  # Then subtract rasters in raster calculator then
    # 0 = unchanged
    # 9 = non-forest to forest
    # -9 = forest to non-forest
    # 99 = cloud to forest
    # -99 = forest to cloud
    # 90 = cloud to non-forest
    # -90 = non-forest to cloud
    # 900 = shadow to cloud
    # -900 = cloud to shadow
    # 990 = shadow to non-forest
    # -990 = nonforest to shadow
    # 999 = shadow to forest
    # -999 = forest to shadow

  # freq(ras2019)
  # freq(ras2022)
  
  ras2019 <- terra::subst(ras2019, from = c(1, 2, 3, 4), to = c(1, 10, 100, 1000))
  ras2022 <- terra::subst(ras2022, from = c(0, 1, 2, 3, 4), to = c(NA, 1, 10, 100, 1000))
  ras2022[ras2022 < 1] <- NA # For some reason there are 0s in the 2022 raster - set them to NA
  
  # freq(ras2019)
  # freq(ras2022)


# Extend rasters to match extents ------------------------------------------

  ras2019 <- extend(ras2019, ext(ras2022), fill = NA)
  ras2019
  

# Subtract rasters to calculate change ------------------------------------

  ras_change <- ras2019-ras2022
  ras_change
  # plot(ras_change)
  freq(ras_change)

  
# Save to file ------------------------------------------------------------

  writeRaster(ras_change, 
              paste0("output/change 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)
  
  

# Convert so that only deforestation is shown -----------------------------

  # Need to set all cloud and shadow related values to NA?
  
  # Initialize
  ras_deforest <- ras_change  
  
  # Set anything related to clouds or shadows to NA
  ras_deforest[ras_deforest %in% c(99, -99, 90, -90, 900, -900, 990, -990, 999, -999)] <- NA 
  
  # Set reforestation as NA
  ras_deforest[ras_deforest %in% c(9)] <- NA 
  
  # Set deforestation to 1
  ras_deforest[ras_deforest == -9] <- 1 
  
  freq(ras_deforest)
  
  plot(ras_deforest)
  
  # Write to file
  writeRaster(ras_deforest, 
              paste0("output/deforestation 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)

  

# Smooth out and sieve deforestation raster -------------------------------

  ras_deforest_sieve100 <- terra::sieve(ras_deforest, threshold = 100)
  freq(ras_deforest_sieve100)

  # Resubstitute NA values  
  ras_deforest_sieve100[!(ras_deforest_sieve100 %in% c(0, 1))] <- NA
  
  # Write to file
  writeRaster(ras_deforest_sieve100, 
              paste0("output/deforestation sieve 100 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)
  
  

# Testing out moving windows ----------------------------------------------

  ras_deforest_sieve100_focal <- terra::focal(ras_deforest_sieve100, 
                       w = 33, # Multiply by this by 3m (resolution of Planet Imagery to get window size) - ~100m window size here
                       fun = function(x) sum(x, na.rm = TRUE)/sum(!is.na(x))) # Custom function to calculate %% deforestation in 

 plot(ras_deforest_sieve100_focal)  
  
# Write to file
  writeRaster(ras_deforest_sieve100_focal, 
              paste0("output/deforestation zonal 2019-2022.tif"),
              overwrite=TRUE)
  




