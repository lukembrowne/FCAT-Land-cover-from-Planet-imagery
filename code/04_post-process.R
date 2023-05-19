
# Libraries ---------------------------------------------------------------

  library(terra)

# Load in trained model -------------------------------------------------------

  # load("./output/2022_08_4band_FCATtoCachi - 2022_04_19/2022_08_4band_FCATtoCachi - 2022_04_19.Rdata") 
# load("./output/2019_09_4band_FCATtoCachi - balanced - 2022_04_04/2019_09_4band_FCATtoCachi - balanced - 2022_04_04.Rdata")
# load("./output/2019_09_4band_FCATtoCachi - 2022_04_20/2019_09_4band_FCATtoCachi - 2022_04_20.Rdata") 
# load("./output/2019_09_4band_FCATtoCachi - 2023_05_02/2019_09_4band_FCATtoCachi - 2023_05_02.Rdata")
# load("./output/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1.Rdata")


  # load raster to sieve
  # ras_to_sieve <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2022_04_20/2019_09_4band_FCATtoCachi - 2022_04_20.tif")
  # ras_to_sieve <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2023_05_02/2019_09_4band_FCATtoCachi - 2023_05_02.tif")
  # ras_to_sieve <- terra::rast("./output/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1/2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1.tif")
  ras_to_sieve <- terra::rast("./output/2022_08_4band_FCATtoCachi - 2023_05_09/2022_08_4band_FCATtoCachi - 2023_05_09.tif")
  
  

# Sieve raster to smooth and reduce number of isolated pixels ---------------------------------------------------------


  # Set sieve values
  sieve_value <- 100
  
    
    # Sieve raster
    ras_out_sieve <- sieve(ras_to_sieve, sieve_value)
    
    # Write sieved raster to file
    # https://rspatial.github.io/terra/reference/tmpFile.html - would this help with error?
    writeRaster(ras_out_sieve, 
                paste0("output/", run_name, "/", run_name, "_sieved", sieve_value,".tif"),
                overwrite=TRUE)
    
    # Cleanup
    rm(ras_to_sieve)
    rm(ras_out_sieve)
    

