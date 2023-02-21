

## Todo?

## Merge together 8 band images in QGIS



## Add an if else statement about whether I'm going to fit model or not, or just use for predictions

## Normalize data from within each raster to deal with changes across time / scenes
## Does cacao show up as forest cover?
## Try using 8 band planet images
## Use surface reflectance instead of TOA?? Seems like surface reflectance is what we want
## Balance out training data
## Ask for map of cacao farms from Holden to ground truth / add cacao as another class?
  ## Surface reflectance product removes atmospheric artifacts which increases the consistency and comparability between images acquired at different times. Thus, this product can be leveraged to normalize scenes across time and sensor. Typical usage scenarios are:


## Use UDM mask to cancel out unusable pixels
## " Band 1: clear mask (a value of “1” indicates the pixel is clear, a value of “0” indicates that the pixel is not clear and is one of the 5 remaining classes below)"

## Someday
## Run on July 23, 2021 Planet image to find deforestation hotspots?




# Libraries ---------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(sf)
library(raster)
library(caret)
library(tabularaster)
library(XML)
library(xml2)


## Load in raster and extract out pixel level values -----------------------
## note all PlanetScope 4-band images have band order BGRN


path_to_images = "../../../Satellite imagery/Planet imagery/2022_08/FCAT_Reserve_-_August_2022_psscene_analytic_sr_udm2/files/"

## Use a for loop to loop through scenes
  scene_names <- c("20220815_151942_43_2483_3B",
                   "20220815_151940_15_2483_3B",
                   "20220815_144703_32_241e_3B",
                   "20220815_144701_02_241e_3B")
  
  # Set counter
  x = 1
  
  for(scene_name in scene_names){
    
    cat("Working on scene:", scene_name, " ... \n")
      
    # Read in raster image
    ras_temp <- raster::brick(paste0(path_to_images, scene_name, "_AnalyticMS_SR_harmonized_clip.tif"))
  
  
    # Convert raster values to tibble
    ras_dat <- as_tibble(ras_temp)
    ras_dat <- pivot_wider(ras_dat, names_from = dimindex, values_from = cellvalue)
    ras_dat <- ras_dat %>%
      rename(blue = `1`,
             green = `2`,
             red = `3`,
             nir = `4`)
  
    # Read in metadata to calculate TOA Reflectance
    cat("Normalizing TOA Reflectance ... \n")
    
    
    metadata <- read_xml(paste0(path_to_images, scene_name, "_AnalyticMS_metadata_clip.xml"))
    coefs <-  xml_double(xml_find_all(metadata, "//ps:reflectanceCoefficient"))
  
    ras_dat$blue <- ras_dat$blue * coefs[1]
    ras_dat$green <- ras_dat$green * coefs[2]
    ras_dat$red <- ras_dat$red * coefs[3]
    ras_dat$nir <- ras_dat$nir * coefs[4]
    
    # Set values back into raster
    ras_temp <- setValues(ras_temp, values = ras_dat$blue, layer = 1)
    ras_temp <- setValues(ras_temp, values = ras_dat$green, layer = 2)
    ras_temp <- setValues(ras_temp, values = ras_dat$red, layer = 3)
    ras_temp <- setValues(ras_temp, values = ras_dat$nir, layer = 4)
    
    # Assign to R object
    assign(paste0("ras_", x),  ras_temp)
    
    ## Read in UDM data
    cat("Loading UDM raster ... \n")
    udm <- raster::brick(paste0(path_to_images, scene_name, "_udm2_clip.tif"))
    
    ## Assing to R object
    assign(paste0("udm_", x), udm)
    
    # Increment X
    x = x + 1
    
    # Clean up
    rm(ras_dat)
    rm(ras_temp)
    rm(udm)
    
    # # Convert raster values to tibble
    # udm_dat <- as_tibble(udm)
    # udm_dat <- pivot_wider(udm_dat, names_from = dimindex, values_from = cellvalue)
    # udm_dat <- udm_dat %>%
    #   rename(clear = `1`,
    #          snow = `2`,
    #          shadow = `3`,
    #          haze_light = `4`,
    #          haze_heavy = `5`,
    #          cloud = `6`,
    #          confidence = `7`,
    #          udm1 = '8')
    
} # End scene name loop


## Merge rasters
ras <- raster::merge(ras_1, ras_2, ras_3, ras_4)
udm <- raster::merge(udm_1, udm_2, udm_3, udm_4)

# Set layer names
names(ras) <- c("blue", "green", "red", "nir")
names(udm) <- names(udm_1)

# Convert to tibble
ras_dat <- as_tibble(ras)
ras_dat <- pivot_wider(ras_dat, names_from = dimindex, values_from = cellvalue)
ras_dat <- ras_dat %>%
  rename(blue = `1`,
         green = `2`,
         red = `3`,
         nir = `4`)

plotRGB(ras, r = 3, g = 2, b = 1, scale = 1)


udm_dat <- as_tibble(udm)
udm_dat <- pivot_wider(udm_dat, names_from = dimindex, values_from = cellvalue)
udm_dat <- udm_dat %>%
  rename(clear = `1`,
         snow = `2`,
         shadow = `3`,
         haze_light = `4`,
         haze_heavy = `5`,
         cloud = `6`,
         confidence = `7`,
         udm1 = `8`)


# Read in training files for each class ------------------------------------------

  forest <- read_sf("./class shapefiles/forest class.shp")
  cloud <- read_sf("./class shapefiles/cloud class.shp")
  development <- read_sf("./class shapefiles/development class.shp")
  pasture <- read_sf("./class shapefiles/pasture class.shp")
  shadow <- read_sf("./class shapefiles/shadow class.shp")
  
  
  plot(forest, add = TRUE, col = "green")
  plot(cloud, add = TRUE, col = "blue")
  plot(pasture, add = TRUE, col = "brown")
  plot(development, add = TRUE, col = "purple")
  plot(shadow, add = TRUE, col = "grey90")



# Extract data for each class as a dataframe ------------------------------------

  dat_cloud <- raster::extract(ras, cloud, df = TRUE)
  dat_cloud$class = "cloud"
  dat_cloud <- dat_cloud %>% filter(!is.na(blue))
  
  dat_development <- raster::extract(ras, development, df = TRUE)
  dat_development$class = "development"
  dat_development <- dat_development %>% filter(!is.na(blue))
  
  dat_forest <- raster::extract(ras, forest, df = TRUE)
  dat_forest$class = "forest"
  dat_forest <- dat_forest %>% filter(!is.na(blue)) # Remove NAs
  
  dat_pasture <- raster::extract(ras, pasture, df = TRUE)
  dat_pasture$class = "pasture"
  dat_pasture <- dat_pasture %>% filter(!is.na(blue))
  
  dat_shadow <- raster::extract(ras, shadow, df = TRUE)
  dat_shadow$class = "shadow"
  dat_shadow <- dat_shadow %>% filter(!is.na(blue))

  
  # Bind into one dataset
  dat <- bind_rows(dat_forest,
                   dat_cloud,
                   dat_development,
                   dat_pasture,
                   dat_shadow)
  
# Summarize spectral signatures -------------------------------------------

  dat_sum <- dat %>%
    group_by(class) %>%
    summarize_all(mean) %>%
    dplyr::select(-ID) %>%
    pivot_longer(-class)
  
  ggplot(dat_sum, aes(x = name, y = value, col = class)) + 
    geom_point()



# Split into training and testing data  
  train_index <- createDataPartition(dat$class,
                                     p = 0.8,
                                     list = FALSE,
                                     times = 1)
  dat$train <- FALSE
  dat$train[train_index] <- TRUE
  
  table(dat$class, dat$train)
  
  dat_train <- dat[train_index, ]
  dat_test <- dat[-train_index, ]

  
  
  
  
  # Xgboost parameters
  # xgb_nfolds = c(5),
  # xgb_eta =  c(0.05, .1, .3, .5, .9),
  # xgb_nrounds = 5000,
  # xgb_max_depth =  c(1, 4, 7, 10, 15),
  # xgb_min_child_weight = c(1, 4, 7),
  # xgb_subsample = c(.33, .66, 1),
  # xgb_colsample_bytree = c(0.2, .5, 1)
  
  
  

# set.seed(42)
## Run final model
boost <- xgboost(data =  as.matrix(dat_train[, c("blue", "red", "green", "nir")]),
                 label = as.numeric(factor(dat_train$class)) - 1,
                 max_depth = 6,
                 eta = 0.3,
                 min_child_weight = 1,
                 subsample = 0.66,
                 colsample_bytree = 1,
                 nrounds = 500, # Do 500 or so for real model
                 early_stopping_rounds = 15, # stop if no improvement for 15 consecutive trees,
                 objective = "multi:softprob",
                 num_class = length(table(factor(dat_train$class))))

boost

### SAVE MODEL OUTPUT
# save(boost, file =  paste0("./output/model_run ", Sys.Date(), ".Rdata"))


## Load previous model run
load("./output/model_run 2023-01-19.Rdata")


## Predictions on testing data
  pred = predict(boost,
                 newdata = as.matrix(dat_test[, c("blue", "red", "green", "nir")]),
                 reshape = TRUE)
  

# Transform to rows as pixels and columns as classes
  # pred <- matrix(pred, 
  #                nrow = length(table(factor(dat_train$class))),
  #                ncol= length(pred)/length(table(factor(dat_train$class)))) %>%
  #   t() %>%
  #   data.frame()
  
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
  confusionMatrix(factor(top_preds$true_class, levels(factor(dat_train$class))),
                  factor(top_preds$class, levels(factor(dat_train$class))),
                  mode = "prec_recall")
  

# Predict across entire raster --------------------------------------------
  
# Cut prediction raster into slices to speed things up
  
  n_cuts = 10
  
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
  newdata <- as.matrix(ras_dat[cut_starts[x]:cut_ends[x], c("blue", "red", "green", "nir")])

  # Predict on model
  cat("Predicting ... \n" )
  
  pred_all <- predict(boost, 
                     newdata = newdata,
                     reshape = TRUE)
  pred_all
  
  # Transform to tibble
  pred_all <- as_tibble(pred_all)
  head(pred_all)
  
  # Add column names
  colnames(pred_all) <- levels(factor(dat_train$class))
  head(pred_all)
  
  # Save for later
  pred_all_colnames <- colnames(pred_all)

  ## Accuracy - take top probability for each pixel
  pred_all <- pred_all %>%
    mutate(top_pred = apply(pred_all, MARGIN = 1, function(x) pred_all_colnames[which.max(x)]),
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
shadow_indexes <- udm_dat$cellindex[udm_dat$shadow == 1]
cloud_indexes <- udm_dat$cellindex[udm_dat$cloud == 1]

pred_out$top_pred[shadow_indexes] <- "shadow"
pred_out$top_pred[cloud_indexes] <- "cloud"

# Counts of classes
table(pred_out$top_pred)

## Replace data into raster
ras_out <- ras
values(ras_out) <- as.numeric(factor(pred_out$top_pred)) # Could add distinct levels here to make sure values are right

ras_out <- ras_out[[1]]
names(ras_out) <- "class"

## Write out raster
writeRaster(ras_out, paste0("output/land cover ", Sys.Date(), ".tif"), overwrite=TRUE)






# 
# # CV
# boost.cv <- xgb.cv(data = xgdata,
#                    label = as.numeric(factor(tree_dat_training$taxonID[resample_row_indices])) - 1,
#                    max_depth = params$xgb_max_depth,
#                    eta = params$xgb_eta,
#                    min_child_weight = params$xgb_min_child_weight,
#                    subsample = params$xgb_subsample,
#                    colsample_bytree = params$xgb_colsample_bytree,
#                    nrounds = params$xgb_nrounds,
#                    nfold = 5,
#                    early_stopping_rounds = 10, # stop if no improvement for 15 consecutive trees,
#                    objective = "multi:softprob",
#                    stratified = TRUE,
#                    seed = 42,
#                    num_class = length(table( factor(tree_dat_training$taxonID[resample_row_indices]))))
# 
# boost.cv
# 
# 
# ## Save error rates
# 
# params$train_merror_mean <- min(boost.cv$evaluation_log$train_merror_mean)
# params$test_merror_mean <- min(boost.cv$evaluation_log$test_merror_mean)
# params$best_ntreelimit <- boost.cv$best_ntreelimit
# params$best_iteration <- boost.cv$best_iteration
# 













# Original code -----------------------------------------------------------


# 
# 
# ## Read in raster files
# ras1 <- raster::brick("../../../Satellite imagery/Planet imagery/August 2022/FCAT_Reserve_-_August_2022_psscene_analytic_sr_udm2/files/20220815_151942_43_2483_3B_AnalyticMS_SR_harmonized_clip.tif")
# 
# ras2 <- raster::brick("../../../Satellite imagery/Planet imagery/August 2022/FCAT_Reserve_-_August_2022_psscene_analytic_sr_udm2/files/20220815_151940_15_2483_3B_AnalyticMS_SR_harmonized_clip.tif")
# 
# ras <- raster::merge(ras1, ras2)
# names(ras) <- names(ras1)
# 
# ras_dat <- values(ras)
# # ras_dat <- as.tibble(ras_dat) %>% filter(!is.na(blue))
# 
# ras_dat2 <- as_tibble(ras)
# ras_dat2 <- pivot_wider(ras_dat2, names_from = dimindex, values_from = cellvalue)
# ras_dat2 <- ras_dat2 %>%
#   rename(blue = `1`,
#          green = `2`,
#          red = `3`,
#          nir = `4`)
# 
# plotRGB(ras, r = 3, g = 2, b = 1, scale = 10000)
# 
# 
# # Read in class files -----------------------------------------------------
# 
# forest <- read_sf("./class shapefiles/forest class.shp")
# cloud <- read_sf("./class shapefiles/cloud class.shp")
# development <- read_sf("./class shapefiles/development class.shp")
# pasture <- read_sf("./class shapefiles/pasture class.shp")
# shadow <- read_sf("./class shapefiles/shadow class.shp")
# 
# 
# plot(forest, add = TRUE, col = "green")
# plot(cloud, add = TRUE, col = "blue")
# plot(pasture, add = TRUE, col = "brown")
# plot(development, add = TRUE, col = "purple")
# plot(shadow, add = TRUE, col = "grey90")
# 
# 
# 
# # Extract data ------------------------------------------------------------
# 
# dat_forest <- raster::extract(ras, forest, df = TRUE)
# dat_forest$class = "forest"
# dat_forest <- dat_forest %>% filter(!is.na(blue))
# 
# dat_cloud <- raster::extract(ras, cloud, df = TRUE)
# dat_cloud$class = "cloud"
# dat_cloud <- dat_cloud %>% filter(!is.na(blue))
# 
# dat_development <- raster::extract(ras, development, df = TRUE)
# dat_development$class = "development"
# dat_development <- dat_development %>% filter(!is.na(blue))
# 
# dat_pasture <- raster::extract(ras, pasture, df = TRUE)
# dat_pasture$class = "pasture"
# dat_pasture <- dat_pasture %>% filter(!is.na(blue))
# 
# dat_shadow <- raster::extract(ras, shadow, df = TRUE)
# dat_shadow$class = "shadow"
# dat_shadow <- dat_shadow %>% filter(!is.na(blue))
# 
# 
# dat <- bind_rows(dat_forest,
#                  dat_cloud,
#                  dat_development,
#                  dat_pasture,
#                  dat_shadow)
# 
# 
# 
# # Summarize spectral signatures -------------------------------------------
# 
# dat_sum <- dat %>%
#   group_by(class) %>%
#   summarize_all(mean) %>%
#   dplyr::select(-ID) %>%
#   pivot_longer(-class)
# 
# ggplot(dat_sum, aes(x = name, y = value, col = class)) + 
#   geom_point()
# 
# 
# 
# # Fit xgboost model -------------------------------------------------------
# 
# train_index <- createDataPartition(dat$class,
#                                    p = 0.8,
#                                    list = FALSE,
#                                    times = 1)
# dat$train <- FALSE
# dat$train[train_index] <- TRUE
# 
# table(dat$class, dat$train)
# 
# dat_train <- dat[train_index, ]
# 
# mod <- xgboost(data = as.matrix(dat_train[, c("blue", "red", "green", "nir")]), 
#                label = factor(dat_train$class),
#                nrounds = 100)
# 
# 
# # model predictions
# pred_test <- predict(mod, as.matrix(dat[-train_index, c("blue", "red", "green", "nir")]))
# pred_test
# 
# levels(factor(dat_train$class))
# 
# 
# 
# 
# # Predict across entire raster --------------------------------------------
# 
# pred_all <- predict(mod, as.matrix(ras_dat[, c("blue", "red", "green", "nir")]))
# pred_all
# 
# pred_all[which(is.na(ras_dat[, 1]))] <- NA # Replace NAs
# 
# # Round to nearest value
# pred_all <- round(pred_all)
# table(pred_all)
# 
# ## Replace data into raster
# ras_out <- ras
# values(ras_out) <- pred_all
# 
# writeRaster(ras_out, paste0("output/test ", Sys.Date(), ".tif"), overwrite=TRUE)





path_to_images = "../../../Satellite imagery/Planet imagery/2022_12/December_23,_2022_psscene_analytic_sr_udm2/files/"

## Use a for loop to loop through scenes
scene_names <- c("20221223_144404_94_242b_3B",
                 "20221223_151656_23_249b_3B",
                 "20221223_151658_37_249b_3B",
                 "20221223_151700_51_249b_3B",
                 "20221223_152027_39_2475_3B",
                 "20221223_152029_53_2475_3B")

# Set counter
x = 1

for(scene_name in scene_names){
  
  cat("Working on scene:", scene_name, " ... \n")
  
  # Read in raster image
  ras_temp <- raster::brick(paste0(path_to_images, scene_name, "_AnalyticMS_SR_harmonized_clip.tif"))
  
  
  # Convert raster values to tibble
  ras_dat <- as_tibble(ras_temp)
  ras_dat <- pivot_wider(ras_dat, names_from = dimindex, values_from = cellvalue)
  ras_dat <- ras_dat %>%
    rename(blue = `1`,
           green = `2`,
           red = `3`,
           nir = `4`)
  
  # Read in metadata to calculate TOA Reflectance
  cat("Normalizing TOA Reflectance ... \n")
  
  
  metadata <- read_xml(paste0(path_to_images, scene_name, "_AnalyticMS_metadata_clip.xml"))
  coefs <-  xml_double(xml_find_all(metadata, "//ps:reflectanceCoefficient"))
  
  ras_dat$blue <- ras_dat$blue * coefs[1]
  ras_dat$green <- ras_dat$green * coefs[2]
  ras_dat$red <- ras_dat$red * coefs[3]
  ras_dat$nir <- ras_dat$nir * coefs[4]
  
  # Set values back into raster
  ras_temp <- setValues(ras_temp, values = ras_dat$blue, layer = 1)
  ras_temp <- setValues(ras_temp, values = ras_dat$green, layer = 2)
  ras_temp <- setValues(ras_temp, values = ras_dat$red, layer = 3)
  ras_temp <- setValues(ras_temp, values = ras_dat$nir, layer = 4)
  
  # Assign to R object
  assign(paste0("ras_", x),  ras_temp)
  
  ## Read in UDM data
  cat("Loading UDM raster ... \n")
  udm <- raster::brick(paste0(path_to_images, scene_name, "_udm2_clip.tif"))
  
  ## Assing to R object
  assign(paste0("udm_", x), udm)
  
  # Increment X
  x = x + 1
  
  # Clean up
  rm(ras_dat)
  rm(ras_temp)
  rm(udm)
  
  # # Convert raster values to tibble
  # udm_dat <- as_tibble(udm)
  # udm_dat <- pivot_wider(udm_dat, names_from = dimindex, values_from = cellvalue)
  # udm_dat <- udm_dat %>%
  #   rename(clear = `1`,
  #          snow = `2`,
  #          shadow = `3`,
  #          haze_light = `4`,
  #          haze_heavy = `5`,
  #          cloud = `6`,
  #          confidence = `7`,
  #          udm1 = '8')
  
} # End scene name loop

ras <- raster::merge(ras_1, ras_2, ras_3, ras_4, ras_5, ras_6)

# Set layer names
names(ras) <- c("blue", "green", "red", "nir")
# names(udm) <- names(udm_1)

# Convert to tibble
ras_dat <- as_tibble(ras)
ras_dat <- pivot_wider(ras_dat, names_from = dimindex, values_from = cellvalue)
ras_dat <- ras_dat %>%
  rename(blue = `1`,
         green = `2`,
         red = `3`,
         nir = `4`)



# Predict across entire raster --------------------------------------------

# Cut prediction raster into slices to speed things up
cuts <- round(seq(1, nrow(ras_dat), len = 5))
cut_starts <- cuts[1:4]
cut_ends <- cuts[2:5] - 1
cut_ends[4] <- cut_ends[4] + 1

cut_starts
cut_ends

# Initialize output dataframe
pred_out <- tibble(pixelID = 1:nrow(ras_dat),
                   top_pred = NA)


# Loop over intervals  
for(x in 1:4)  {
  
  # Set up new dataframe
  newdata <- as.matrix(ras_dat[cut_starts[x]:cut_ends[x], c("blue", "red", "green", "nir")])
  
  # Predict on model
  pred_all <- predict(boost, 
                      newdata = newdata,
                      reshape = TRUE)
  pred_all
  
  # Transform to tibble
  pred_all <- as_tibble(pred_all)
  head(pred_all)
  
  # Add column names
  colnames(pred_all) <- levels(factor(dat_train$class))
  head(pred_all)
  
  # Save for later
  pred_all_colnames <- colnames(pred_all)
  
  ## Accuracy - take top probability for each pixel
  pred_all <- pred_all %>%
    mutate(top_pred = apply(pred_all, MARGIN = 1, function(x) pred_all_colnames[which.max(x)]),
           pixelID =  cut_starts[x]:cut_ends[x]
    )
  
  
  # Replace NAs
  pred_all[which(is.na(newdata[, 1])), "top_pred"] <- NA # Will make NA 
  
  # Join with output dataset
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
values(ras_out) <- as.numeric(factor(pred_out$top_pred)) # Could add distinct levels here to make sure values are right

ras_out <- ras_out[[1]]
names(ras_out) <- "class"

## Write out raster
writeRaster(ras_out, paste0("output/test ", Sys.Date(), ".tif"), overwrite=TRUE)


