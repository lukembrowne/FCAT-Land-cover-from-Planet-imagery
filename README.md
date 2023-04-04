### Land cover classification of areas surrounding FCAT reserve

March 27th, 2023

**Overview**

**Brief methods**

-   Download 4 band harmonized surfance reflectance product from Planet
-   Load all rasters in QGIS and visualize the rasters to place them in an order that minimizes cloud cover
    -   UDM masks don't seem too helpful, so ignoring them for now
-   Merge the individual rasters using 01_merge_rasters.R
-   Load merged raster in QGIS to make training polygons
-   Change visualization of raster to improve classification
    -   Change band colors
-   Create a blank shapefile to add training polygons
    -   Add a data field for 'class' as an integer (in addition to ID field)
    -   Draw polygons of different land use types
    -   Using integer classifications:
        -   1 = Forest
        -   2 = Agriculture
        -   3 = Cloud
        -   4 = Shadow
        -   5 = Built environment (communities)
    -   Aiming for at least 30 polygons of each class spread across the map

### Issues

-   Reflections from roofs and clouds are very similar and get confused
