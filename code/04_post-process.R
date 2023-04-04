
# Libraries ---------------------------------------------------------------

  library(terra)

# Load in trained model -------------------------------------------------------

  load("./output/2022_08_4band_FCATtoCachi - balanced - 2022_04_03/2022_08_4band_FCATtoCachi - balanced - 2022_04_03.Rdata") ## test run for 2022_08

# Sieve raster to smooth and reduce number of isolated pixels ---------------------------------------------------------


  # Set sieve values
  sieve_value <- 50
  
  cat("Working on sieve value: ", sieve_value, " ... \n")
    
    # Need to load raster of predicted classes in each loop for some reason to avoid error- "Error: [sieve] cannot open this file as a SpatRaster:"
    ras_to_sieve <- terra::rast(paste0("output/", run_name, "/", run_name, ".tif"))
    
    # Sieve raster
    ras_out_sieve <- sieve(ras_to_sieve, sieve_value)
    
    # Write sieved raster to file
    writeRaster(ras_out_sieve, 
                paste0("output/", run_name, "/", run_name, "_sieved", sieve_value,".tif"),
                overwrite=TRUE)
    
    # Cleanup
    rm(ras_to_sieve)
    rm(ras_out_sieve)
    

