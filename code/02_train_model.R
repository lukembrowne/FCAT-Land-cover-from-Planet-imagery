
# Load libraries ---------------------------------------------------------------

  library(tidyverse)
  library(xgboost)
  library(sf)
  library(terra)
  library(caret)
  library(RemoteSensing) # install.packages("RemoteSensing", repos="http://R-Forge.R-project.org")
  

# Set parallel computing environment --------------------------------------
  library(parallel)
  library(doParallel)
  cluster <- makeCluster(detectCores() - 2) # convention to leave 1 core for OS
  registerDoParallel(cluster)


# Set seed for reproducibility --------------------------------------------

  set.seed(42)


# Set run name, paths to raster, and training polygons -------------------------------
  
  # Set name for model run
  # run_name <- "2022_08_4band_FCATtoCachi - balanced - 2022_04_03"
  
  run_name <- "2019_09_4band_FCATtoCachi - balanced - 2022_04_04"
  
  # Create directory in output folder
  dir.create(paste0("./output/", run_name))
  
  # Path to raster merged using 01_merge_raster.R
  # path_to_merged_raster <- "../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/20220815_merged_3B_AnalyticMS_SR_harmonized_clip.tif"
  path_to_merged_raster <- "../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/20190903_merged_3B_AnalyticMS_SR_harmonized_clip.tif"
  
  # Path to training polygons
  # path_to_training_polys <- "class_shapefiles/2022_08 classes.shp"
  path_to_training_polys <- "class_shapefiles/2019_09 classes.shp"
  
  
# Load in merged raster ---------------------------------------------------

  # Load raster
  ras <- terra::rast(path_to_merged_raster)
  
  # Set layer names for four band imagery
  four_band_names <- c("blue", "green", "red", "nir")
  names(ras) <- four_band_names  
  
    # If using 8 band Planet imagery
    # eight_band_names <- c("coastal_blue", "blue", "green_i", "green", "yellow",
    #                       "red", "rededge", "nir")
    # names(ras) <- eight_band_names

  
  
# Read in training polygons ------------------------------------------
  
  training_polys <- vect(path_to_training_polys)
  
  # Add values for ID
  training_polys$id <- 1:length(training_polys)
  
  # Quick plot
  plot(training_polys)
  
  # Compare areas of each class
  area <- tibble(class = training_polys$class, 
                 area = expanse(training_polys)/10000) %>%
    group_by(class) %>%
    summarize(area = sum(area),
              n = length(class))
  area

  

# Extract data for each class as a dataframe ------------------------------------
  
  dat <- terra::extract(ras, training_polys, 
                        xy = FALSE, exact = FALSE, 
                        touches = TRUE)

  # Join with class
  dat <- left_join(dat, values(training_polys), by = c("ID" = "id"))
  
  # Inspect data
  head(dat)
  dim(dat)
  object.size(dat)/1e6 # in megabytes
  
  

# Combine or remove classes -----------------------------------------------

  # Removing built environment class
  dat <- dat %>% 
    filter(class != 5)
  

# Calculate vegetation indices --------------------------------------------
# https://www.hindawi.com/journals/js/2017/1353691/
# Need remote sensing package
    dat <- dat %>%
    mutate(ndvi = RemoteSensing::ndvi(red, nir),
           savi = RemoteSensing::savi(red, nir),  # Soil-Adjusted Vegetation Index
           arvi = RemoteSensing::arvi(red, nir, blue) # Atmosphere Antivegetation Index
    )


# Summarize spectral signatures -------------------------------------------
  dat_sum <- dat %>%
    group_by(class) %>%
    summarize_all(mean) %>%
    dplyr::select(-ID) %>%
    pivot_longer(-class) %>%
    rename(mean = value)
  
  dat_sum <- left_join(dat_sum, 
                       dat %>%
                         group_by(class) %>%
                         summarize_all(sd) %>%
                         dplyr::select(-ID) %>%
                         pivot_longer(-class) %>%
                         rename(sd = value))
  
  ggplot(dat_sum, aes(x = as.factor(class), y = mean, col = as.factor(class))) + 
    geom_pointrange(aes(ymax = mean + sd, ymin = mean - sd), 
                    position=position_dodge(0.3)) +
    facet_wrap(~name, scales = "free_y") + 
    theme_bw()

  rm(dat_sum)



# Split into training and testing data based on polygons   ----------------------------------


  # Subset to distinct polygons
  dat_poly <- dat %>% 
              distinct(ID, .keep_all = TRUE)
  
  # Create training and testing data partitions
  train_index <- caret::createDataPartition(dat_poly$class,
                                            p = 0.8,
                                            list = FALSE,
                                            times = 1)

  dat_poly$train <- FALSE
  dat_poly$train[train_index] <- TRUE
  
  table(dat_poly$class, dat_poly$train)
  
  # Subset down into training and testing sets
  dat_train <- dat %>%
                filter(ID %in% dat_poly$ID[train_index])
  dat_test <- dat %>%
                filter(!(ID %in% dat_poly$ID[train_index]))

  dim(dat_train)
  dim(dat_test)
  
  # Count pixels in each class
  pixel_count_train <- dat_train %>% group_by(class) %>% tally()
  pixel_count_test <- dat_test %>% group_by(class) %>% tally()
  
  pixel_count_train
  pixel_count_test
  
  # Balance sample sizes among classes
  dat_train_balanced <- dat_train %>% 
                  group_by(class) %>%
                  sample_n(size = min(pixel_count_train$n))
  
  dat_test_balanced <- dat_test %>% 
                  group_by(class) %>% 
                  sample_n(size = min(pixel_count_test$n))

  
# Setting up hyperparameter optimization ----------------------------------

  # Define hyperparameters to optimize
  params <- list(
    eta = c(0.01, 0.05, 0.1, 0.3, .5, .9),
    max_depth = c(3, 5, 7, 9),
    min_child_weight = c(1, 4, 7),
    subsample = c(.33, .66, 1),
    gamma = 0,
    colsample_bytree = c(0.2, .5, 1),
    nrounds = c(200)
  )

  
  # Define search space
  search_space <- expand.grid(params) 
  dim(search_space)
  
  
  # Randomly subsample search space
  n_combos <- 5 # How many parameter combos to test?
  
  search_space <- search_space %>%
    sample_n(n_combos)
  
  # Set up hyperparameter optimization parameters
  ctrl <- trainControl(method = "cv", 
                       number = 5, 
                       verboseIter = TRUE,
                       search = "random",
                       allowParallel = TRUE)
  
  
  predictors <- c(four_band_names, "ndvi")
  

  # Use grid search
  start_time <- proc.time()
  
  xgb_grid <- caret::train(
                  x = as.matrix(dat_train_balanced[, predictors]), 
                  y = factor(dat_train_balanced$class),
                  method = "xgbTree",
                  trControl = ctrl,
                  tuneGrid = search_space,
                  verbose = FALSE
                  )
  
  # Calculate how long it took
  end_time <- proc.time()
  time_diff <- end_time - start_time
  print(time_diff["elapsed"]/60) # Time in minutes
  
  
  
  
  # Get the best hyperparameter combination
  best_params <- xgb_grid$bestTune
  
  xgb_grid$results
  xgb_grid$bestTune
  xgb_grid$metric
  
  # >   xgb_grid$bestTune
  # nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
  #  1000         5 0.05     0              0.9                1       0.6
   # best_params <- tibble(nrounds = 1000,
   #                        max_depth = 5,
   #                        eta = 0.05,
   #                        gamma = 0,
   #                        colsample_bytree = 0.9,
   #                        min_child_weight = 1,
   #                        subsample = 0.6)
  
## Run final model

  boost <- xgboost(data =  as.matrix(dat_train_balanced[, predictors]),
                   label = as.numeric(factor(dat_train_balanced$class)) - 1,
                   max_depth = best_params$max_depth,
                   eta = best_params$eta,
                   min_child_weight = best_params$min_child_weight,
                   subsample = best_params$subsample,
                   colsample_bytree = best_params$colsample_bytree,
                   nrounds = best_params$nrounds, # Do 500 or so for real model
                   early_stopping_rounds = 15, # stop if no improvement for 15 consecutive trees,
                   objective = "multi:softprob",
                   num_class = length(table(factor(dat_train_balanced$class))))
  
  boost
  
  
  ## Look at variable importance
  importance <- xgb.importance(feature_names = predictors, model = boost)
  importance
  xgb.plot.importance(importance)
  
  

# Save model output -------------------------------------------------------

save(boost, four_band_names, predictors, path_to_merged_raster, 
     run_name,
    file =  paste0("./output/", run_name, "/", run_name, ".Rdata"))


  
# Validation against testing data ---------------------------------------------

  pred = predict(boost,
                 newdata = as.matrix(dat_test_balanced[, predictors]),
                 reshape = TRUE)
  
  head(pred)
  
  pred <- as_tibble(pred)
  head(pred)
  
  colnames(pred) <- levels(factor(dat_test_balanced$class))
  head(pred)
  
  # Add true class
  pred$true_class <- dat_test_balanced$class
  
  # Add pixel ID
  pred$pixelID <- 1:nrow(pred)
  
  
  ## Accuracy - take top probability for each pixel
  top_preds <- pred %>%
    pivot_longer(-c(pixelID, true_class), names_to = "class", values_to = "prob") %>%
    group_by(pixelID) %>%
    top_n(1)
  
  sum(top_preds$class == top_preds$true_class) / nrow(top_preds)
  
  
  # Print confusion matrix
  confusionMatrix(factor(top_preds$true_class, 
                         levels(factor(dat_train_balanced$class))),
                  factor(top_preds$class, 
                         levels(factor(dat_train_balanced$class))),
                  mode = "prec_recall")
  






