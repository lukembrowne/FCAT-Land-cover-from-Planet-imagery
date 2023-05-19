
# Load libraries ----------------------------------------------------------

  library(terra)



# Make output directory ---------------------------------------------------

# Create directory in output folder
out_dir <- paste0("./output/change analysis ", Sys.Date(), "/")
dir.create(out_dir)


# Load in rasters ---------------------------------------------------------

# ras2019 <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2022_04_20/2019_09_4band_FCATtoCachi - 2022_04_20_sieved100.tif")
# ras2022 <- terra::rast("./output/2022_08_4band_FCATtoCachi - 2022_04_19/2022_08_4band_FCATtoCachi - 2022_04_19_sieved100.tif")


ras2019 <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1_sieved100.tif")
ras2022 <- terra::rast("./output/2022_08_4band_FCATtoCachi - 2023_05_09/2022_08_4band_FCATtoCachi - 2023_05_09_sieved100.tif")


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
  
  ras2019 <- terra::subst(ras2019, from = c(0, 1, 2, 3, 4), to = c(NA, 1, 10, 100, 1000))
  ras2022 <- terra::subst(ras2022, from = c(0, 1, 2, 3, 4), to = c(NA, 1, 10, 100, 1000))

  # freq(ras2019)
  # freq(ras2022)


# Extend rasters to match extents ------------------------------------------

  ras2019 <- extend(ras2019, ext(ras2022), fill = NA)
  ras2019
  
  sum(is.na(c(values(ras2019)))) # Number of NAs
  sum(is.na(c(values(ras2022)))) # Number of NAs
  
  

# Subtract rasters to calculate change ------------------------------------

  ras_change <- ras2019-ras2022
  ras_change
  # plot(ras_change)
  freq(ras_change)
  sum(is.na(c(values(ras_change)))) # Number of NAs
  
  terra::global(x = ras_change, "isNA")

  
# Save to file ------------------------------------------------------------

  writeRaster(ras_change, 
              paste0(out_dir, "change 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)
  
  

# Convert so that only deforestation is shown -----------------------------

  # Initialize
  ras_deforest <- ras_change  
    
  # Set anything related to clouds or shadows to -99
  ras_deforest[ras_deforest %in% c(99, -99, 90, -90, 900, -900, 990, -990, 999, -999)] <- -999 
  
  # Set reforestation as 1
  ras_deforest[ras_deforest %in% c(9)] <- 1 
  
  # Set deforestation to -1
  ras_deforest[ras_deforest == -9] <- -1 
  
  freq(ras_deforest)
  
  plot(ras_deforest)
  
  sum(is.na(c(values(ras_deforest)))) # Number of NAs
  
  
  # Write to file
  writeRaster(ras_deforest, 
              paste0(out_dir, "forest change 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)

  

# Smooth out and sieve deforestation raster -------------------------------

  ras_deforest_sieve100 <- terra::sieve(ras_deforest, threshold = 100)
  freq(ras_deforest_sieve100)
  sum(is.na(c(values(ras_deforest_sieve100)))) # Number of NAs
  
  # Resubstitute NA values
  ras_deforest_sieve100[is.na(ras_deforest)] <- NA
  sum(is.na(c(values(ras_deforest_sieve100)))) # Number of NAs
  
  # Write to file
  writeRaster(ras_deforest_sieve100, 
              paste0(out_dir, "forest change sieve 100 2019-2022 - ", Sys.Date(), ".tif"),
              overwrite=TRUE)
  