
# Load libaries ----------------------------------------------------------

  library(terra)




# Merge August 2022 rasters from planet -----------------------------------

  # Load rasters - should be 7 from this scene
  ras144658 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144658_72_241e_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras144658_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144658_72_241e_3B_udm2_clip.tif")
  
  

  ras144701 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144701_02_241e_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras144701_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144701_02_241e_3B_udm2_clip.tif")
  
  

  ras144703 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144703_32_241e_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras144703_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144703_32_241e_3B_udm2_clip.tif")
  
  
  
  ras145518 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_145518_82_220b_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras145518_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_145518_82_220b_3B_udm2_clip.tif")
  
  
  ras151937 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151937_87_2483_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras151937_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151937_87_2483_3B_udm2_clip.tif")
  
  
  
  ras151940 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151940_15_2483_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras151940_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151940_15_2483_3B_udm2_clip.tif")
  
  
  
  ras151942 <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151942_43_2483_3B_AnalyticMS_SR_harmonized_clip.tif")
  
  ras151942_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151942_43_2483_3B_udm2_clip.tif")
  
  
  
# Load these into QGIS first and figure out best order with least amount of clouds. Then input this order into the merge function  
merged <- terra::merge(ras151937, ras151942, ras151940, 
                       ras144658, ras144701, ras144703, ras145518,
                       overwrite = TRUE,
                       filename = "../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/20220815_merged_3B_AnalyticMS_SR_harmonized_clip.tif")  

merged_udm <- terra::merge(ras151937_udm, ras151942_udm, ras151940_udm, 
                       ras144658_udm, ras144701_udm, ras144703_udm, 
                       ras145518_udm,
                       overwrite = TRUE,
                       filename = "../../../Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/20220815_merged_3B_AnalyticMS_SR_harmonized_clip_udm.tif")  
  

## Python console command in QGIS just in case
# processing.run("gdal:merge", {'INPUT':['G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151937_87_2483_3B_AnalyticMS_SR_harmonized_clip.tif', 'G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151942_43_2483_3B_AnalyticMS_SR_harmonized_clip.tif','G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_151940_15_2483_3B_AnalyticMS_SR_harmonized_clip.tif', 'G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144658_72_241e_3B_AnalyticMS_SR_harmonized_clip.tif','G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144701_02_241e_3B_AnalyticMS_SR_harmonized_clip.tif','G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_144703_32_241e_3B_AnalyticMS_SR_harmonized_clip.tif','G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/FCAT_+_Chachi_reserves_-_August_2022_4_band_psscene_analytic_sr_udm2/files/20220815_145518_82_220b_3B_AnalyticMS_SR_harmonized_clip.tif'],'PCT':False,'SEPARATE':False,'NODATA_INPUT':None,'NODATA_OUTPUT':None,'OPTIONS':'','EXTRA':'','DATA_TYPE':5,'OUTPUT':'G:/Shared drives/FCAT - Data/GIS/Satellite imagery/Planet imagery/2022_08 - FCAT to Chachi - 4 band harmonized/20220815_merged_3B_AnalyticMS_SR_harmonized_clip.tif'})






# Merge September 2019 rasters from planet -----------------------------------

# Load rasters - should be 7 from this scene
ras162859 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162859_15_106b_3B_AnalyticMS_SR_harmonized_clip.tif")

ras162859_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162859_15_106b_3B_udm2_clip.tif")



ras162901 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162901_25_106b_3B_AnalyticMS_SR_harmonized_clip.tif")

ras162901_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162901_25_106b_3B_udm2_clip.tif")



ras162857 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162857_05_106b_3B_AnalyticMS_SR_harmonized_clip.tif")

ras162857_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_162857_05_106b_3B_udm2_clip.tif")



ras152030 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152030_1027_3B_AnalyticMS_SR_harmonized_clip.tif")

ras152030_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152030_1027_3B_udm2_clip.tif")


ras152029 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152029_1027_3B_AnalyticMS_SR_harmonized_clip.tif")

ras152029_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152029_1027_3B_udm2_clip.tif")



ras152027 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152027_1027_3B_AnalyticMS_SR_harmonized_clip.tif")

ras152027_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152027_1027_3B_udm2_clip.tif")



ras152028 <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152028_1027_3B_AnalyticMS_SR_harmonized_clip.tif")

ras152028_udm <- terra::rast("../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/FCAT_+_Chachi_-_September_2019_-_4_band_harmonized_psscene_analytic_sr_udm2/files/20190903_152028_1027_3B_udm2_clip.tif")



# Load these into QGIS first and figure out best order with least amount of clouds. Then input this order into the merge function  
merged <- terra::merge(ras162859, ras162901, ras162857, 
                       ras152030, ras152029, ras152027, ras152028,
                       overwrite = TRUE,
                       filename = "../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/20190903_merged_3B_AnalyticMS_SR_harmonized_clip.tif")  

merged_udm <- terra::merge(ras162859_udm, ras162901_udm, ras162857_udm, 
                           ras152030_udm, ras152029_udm, ras152027_udm, 
                           ras152028_udm,
                           overwrite = TRUE,
                           filename = "../../../Satellite imagery/Planet imagery/2019_09 - 4 band harmonized/20190903_merged_3B_AnalyticMS_SR_harmonized_clip_udm.tif")  


