
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
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)


# Set seed for reproducibility --------------------------------------------

  set.seed(42)
  

# Decide whether to do hyperparameter tuning ------------------------------

  hyperparameter_tuning = FALSE


# Set run name, paths to raster, and training polygons -------------------------------
  
  # Set name for model run
  # run_name <- "2019_09_4band_FCATtoCachi - 2023_05_09 - PCA filtered 1"
  run_name <- "2022_08_4band_FCATtoCachi - 2023_05_09"
  
  # Create directory in output folder
  dir.create(paste0("./output/", run_name))
  
  # Path to raster merged using 01_merge_raster.R
  # path_to_merged_raster <- "../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/20190903_merged_3B_AnalyticMS_SR_harmonized_clip.tif"
  path_to_merged_raster <- "../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/20220815_merged_3B_AnalyticMS_SR_harmonized_clip.tif"
  
  # Path to training polygons
  # path_to_training_polys <- "class_shapefiles/2019_09 classes.shp"
  path_to_training_polys <- "class_shapefiles/2022_08 classes.shp"
  
  # Set layer names for four band imagery 
  # If using 8 band Planet imagery - c("coastal_blue", "blue", "green_i", "green", "yellow",
  #                       "red", "rededge", "nir")
  # names(ras) <- eight_band_names
  four_band_names <- c("blue", "green", "red", "nir")
  
  # Set predictors in model
  predictors <- c(four_band_names, "ndvi", "ndwi")
  
  
# Load in merged raster ---------------------------------------------------

  # Load raster
  ras <- terra::rast(path_to_merged_raster)
  names(ras) <- four_band_names  
  
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
  
  # Combine cloud wispy cloud class into overall cloud class
  dat$class[dat$class == 6] <- 3
    
  

# Calculate vegetation indices --------------------------------------------
# https://www.hindawi.com/journals/js/2017/1353691/
# Need remote sensing package
    dat <- dat %>%
    mutate(ndvi = RemoteSensing::ndvi(red, nir),
           # savi = RemoteSensing::savi(red, nir),  # Soil-Adjusted Vegetation Index
           # arvi = RemoteSensing::arvi(red, nir, blue), # Atmosphere Antivegetation Index
           ndwi = RemoteSensing::ndwi(green, nir)
    )


# Plot and summarize spectral signatures -------------------------------------------
  dat_sum <- dat %>%
    group_by(class) %>%
    summarize_all(mean) %>%
    dplyr::select(-ID) %>%
    pivot_longer(-class) %>%
    rename(mean = value)
  
  dat_sum <- left_join(dat_sum, 
                       left_join(dat %>%
                         group_by(class) %>%
                         summarize_all(min) %>%
                         dplyr::select(-ID) %>%
                         pivot_longer(-class) %>%
                         rename(min = value),
                         dat %>%
                           group_by(class) %>%
                           summarize_all(max) %>%
                           dplyr::select(-ID) %>%
                           pivot_longer(-class) %>%
                           rename(max = value)))
  
  ggplot(dat_sum, aes(x = as.factor(class), y = mean, col = as.factor(class))) + 
    geom_pointrange(aes(ymax = max, ymin = min), 
                    position=position_dodge(0.3)) +
    facet_wrap(~name, scales = "free_y") + 
    theme_bw()

  rm(dat_sum)
  
  # Violin plot
  ggplot(dat %>% dplyr::select(-ID) %>% pivot_longer(-class), aes(x = as.factor(class), y = value, 
                                                                  col = as.factor(class), 
                                                                  fill = as.factor(class))) + 
    geom_violin(position=position_dodge(0.3)) +
    facet_wrap(~name, scales = "free_y") + 
    theme_bw()



# Unsupervised clustering to compare spectral signatures ------------------

  # Kmeans clustering - elbow method
  wss <- sapply(1:10, function(k) kmeans(dat %>% dplyr::select(-c(ID, class)), k)$tot.withinss) # calculate the WSS for k=1 to 10
  plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within-cluster sum of squares")

  # Run clustering
  kmeans_out <- kmeans(dat %>% dplyr::select(-c(ID, class)), 5)
  
  # Tally up predicted cluster vs. assigned
  out <- tibble(orig = dat$class,
                pred = kmeans_out$cluster)
  
  out
  
  out_sum <- out %>%
              group_by(pred, orig) %>%
              tally()
  
  # Plot
  ggplot(out_sum, aes(x = orig, y = n, fill = factor(pred))) + 
    geom_bar(stat = "identity") + 
    theme_bw(12)
  
  ggplot(out_sum, aes(x = pred, y = n, fill = factor(orig))) + 
    geom_bar(stat = "identity") + 
    theme_bw(12)
  
  
  # Test out PCA
  pca <- prcomp(dat %>% dplyr::select(-c(ID, class)) %>% mutate_all(scale))
  summary(pca)
  
  ggplot(data = as.data.frame(pca$x[,1:2]), aes(x = PC1, y = PC2, col = factor(dat$class))) + 
    geom_point(alpha = 0.05) + 
    scale_color_manual(values = c("green", "yellow", "grey20", "black", "red")) + 
    theme_bw(12)
  
  
  ## Filter based on PCA
  # dat <- dat[!(pca$x[, 1] < -1 & dat$class == 2), ]
  
  
  
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
  # dat_train <- dat_train %>%
  #                 group_by(class) %>%
  #                 sample_n(size = min(pixel_count_train$n))
  # 
  # dat_test <- dat_test %>%
  #                 group_by(class) %>%
  #                 sample_n(size = min(pixel_count_test$n))

  
# Setting up hyperparameter optimization ----------------------------------

if(hyperparameter_tuning){  
  
  # Define hyperparameters to optimize
  params <- list(
    eta = c(0.01, 0.1, 0.3, .9),
    max_depth = c(3, 5, 7, 9),
    min_child_weight = c(1, 4, 7),
    subsample = c(.33, .66, 1),
    gamma = 0,
    colsample_bytree = c(0.33, .66, 1),
    nrounds = c(200, 500, 1000)
  )

  
  # Define search space
  search_space_full <- expand.grid(params) 
  dim(search_space_full)
  
  # How many parameter combos to test?
  n_combos <- 100 
  
  for(combo in 1:n_combos){
    
    cat("Working on combo: ", combo, " ... \n")
    
    # Randomly subsample search space
    search_space <- search_space_full %>%
      sample_n(1)
    
    # Set up hyperparameter optimization parameters
    ctrl <- trainControl(method = "cv", 
                         number = 5, 
                         verboseIter = TRUE,
                         search = "random",
                         allowParallel = TRUE,
                         indexFinal = 1) # Use to avoid fitting a full final model
    
    
    
    # Use grid search
    start_time <- proc.time()
    
    xgb_grid <- caret::train(
                    x = as.matrix(dat_train[, predictors]), 
                    y = factor(dat_train$class),
                    method = "xgbTree",
                    trControl = ctrl,
                    tuneGrid = search_space,
                    verbose = TRUE
                    )
    
    # Calculate how long it took
    end_time <- proc.time()
    time_diff <- end_time - start_time
    print(time_diff["elapsed"]/60) # Time in minutes
    
    
    
    
    # Get the best hyperparameter combination
    # best_params <- xgb_grid$bestTune
    
    # Save to file
    write_csv(xgb_grid$results,
         file =  paste0("./output/", run_name, "/xgboost hyperparameter tuning.csv"), append = TRUE)
    
  } # End combo loop

} else {  
  
  grid_results <- read_csv(paste0("./output/", run_name, "/xgboost hyperparameter tuning.csv"), 
                           col_names = c("eta", "max_depth", "min_child_weight", "subsample", "gamma", "colsample_bytree", "nrounds",  "Accuracy", "Kappa", "AccuracySD", "KappaSD"))
  
  best_params <- grid_results %>%
                arrange(desc(Accuracy)) %>%
                head(1)
}

## Run final model
  boost <- xgboost(data =  as.matrix(dat_train[, predictors]),
                   label = as.numeric(factor(dat_train$class)) - 1,
                   max_depth = best_params$max_depth,
                   eta = best_params$eta,
                   min_child_weight = best_params$min_child_weight,
                   subsample = best_params$subsample,
                   colsample_bytree = best_params$colsample_bytree,
                   nrounds = best_params$nrounds, # Do 500 or so for real model
                   early_stopping_rounds = 15, # stop if no improvement for 15 consecutive trees,
                   objective = "multi:softprob",
                   nthread = 8,
                   num_class = length(table(factor(dat_train$class))))
  
  boost
  
  
  ## Look at variable importance
  importance <- xgb.importance(feature_names = predictors, model = boost)
  importance
  xgb.plot.importance(importance)
  

  
# Validation against testing data ---------------------------------------------

  pred = predict(boost,
                 newdata = as.matrix(dat_test[, predictors]),
                 reshape = TRUE)
  
  head(pred)
  
  pred <- as_tibble(pred)
  head(pred)
  
  colnames(pred) <- levels(factor(dat_test$class))
  head(pred)
  
  # Add true class
  pred$true_class <- dat_test$class
  
  # Add pixel ID
  pred$pixelID <- 1:nrow(pred)
  
  
  ## Accuracy - take top probability for each pixel
  top_preds <- pred %>%
    pivot_longer(-c(pixelID, true_class), names_to = "class", values_to = "prob") %>%
    group_by(pixelID) %>%
    top_n(1)
  
  sum(top_preds$class == top_preds$true_class) / nrow(top_preds)
  
  
  # Print confusion matrix
  confusion_mat <- confusionMatrix(factor(top_preds$true_class, 
                         levels(factor(dat_train$class))),
                  factor(top_preds$class, 
                         levels(factor(dat_train$class))),
                  mode = "prec_recall")
  
  confusion_mat

  
  
# Save model output -------------------------------------------------------
  save(boost, four_band_names, predictors, 
       path_to_merged_raster, 
       run_name, 
       confusion_mat,
       file =  paste0("./output/", run_name, "/", run_name, ".Rdata"))
  
  
  



