# ========================================================================================
# Project:  IRRIGATION MAP
# Subject:  Validating maps
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# load packman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot", "glue", "here")

# Load spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview", "terra", "sf")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# load synergy maps
synergy_2015 <- readRDS(file.path(proc_path, "synergy_map/synergy_2015.rds"))
synergy_2005 <- readRDS(file.path(proc_path, "synergy_map/synergy_2005.rds"))

# read samples
samples_raw <- st_read(file.path(raw_path, "iam_chn_2000/validationsamples/validationsamples_614.shp"))
names(samples)

# load adm
adm <- readRDS(file.path(proc_path, "adm/adm1.rds"))
plot(adm$geometry)

# 2000 cropland mask
cl_2000_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2000.tif"))

# 2005 cropland mask
cl_2005_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2005.tif"))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================

# Create cropland map for China to mask sample data
cl_2005 <- crop(cl_2005_raw, vect(adm))
cl_2005 <- mask(cl_2005, vect(adm))
names(cl_2005) <- "cropland_2005"
plot(cl_2005)


# Create synergy cropland maps
ir_2005 <- rast(synergy_2005[c("x", "y", "ir_mean")], type="xyz")
crs(ir_2005) <- "+init=epsg:4326"
plot(ir_2005)

ir_2015 <- rast(synergy_2015[c("x", "y", "ir_mean")], type="xyz")
crs(ir_2015) <- "+init=epsg:4326"
plot(ir_2015)

# Process samples
samples <- samples_raw %>%
  mutate(reference = factor(ifelse(irr_noirr == '灌溉站点', 1, 0))) %>%
  dplyr::select(-irr_noirr)


# ========================================================================================
# OVERLAY --------------------------------------------------------------------------------
# ========================================================================================

# Compare cropland map with samples. We need to exclude observations that are not in the
# cropland raster as by definition this will result in a FALSE.
mapviewOptions(fgb = FALSE)
mapview(raster(cl_2005)) + mapview(samples, zcol = "reference")

validation <- bind_cols(
  extract(cl_2005, vect(samples)),
          samples) %>%
  left_join(
    extract(ir_2005, vect(samples)),
  ) %>%
  filter(cropland_2005 >0 ) %>%
  mutate(predicted = factor(ifelse(!is.na(ir_mean), 1, 0)))


# ========================================================================================
# PLOT -----------------------------------------------------------------------------------
# ========================================================================================


mapviewOptions(fgb = FALSE)
mapview(raster(ir_2005)) + mapview(samples, zcol = "reference")

#NOTE: you can directly plot a polygon in ggplot with geom_sf!

# create map
synergy_2015 %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = ir_mean)) +
  geom_sf(data = adm, fill = "transparent", color = "black") +
  labs(main = "Irrigated area in 2005") +
  scale_fill_viridis_c() +
  geom_sf(data = samples, colour = "red", size = 1 , alpha=0.7) +
  coord_sf(xlim = c(70, 140), ylim = c(18, 55), expand = TRUE) +
  theme_void()


# ========================================================================================
# CONFUSION MATRIX -----------------------------------------------------------------------
# ========================================================================================
#p_load("caret", "InformationValue", "ISLR") STILL NEEDED?
# See https://classeval.wordpress.com/introduction/basic-evaluation-measures/#:~:text=The%20best%20accuracy%20is%201.0,dataset%20(P%20%2B%20N).
# for explanations.
p_load(ConfusionTableR)
remove.packages("rlang")
install.packages("rlang")
ConfusionTableR::binary_visualiseR(train_labels = validation$predicted,
                                   truth_labels= validation$reference,
                                   class_label1 = "Irrigated",
                                   class_label2 = "Not irrigated",
                                   quadrant_col1 = "#28ACB4",
                                   quadrant_col2 = "#4397D2",
                                   custom_title = "Confusion Matrix",
                                   text_col= "black")


# ========================================================================================
# PADDY CROPLAND MAP ---------------------------------------------------------------------
# ========================================================================================
#load map
nlcd_2015 <- rast(file.path(raw_path, "chinese_nlcd/Chinese-NLCD-1KM-2010-2020/Year2015/Lucc2015/lucc2015/dblbnd.adf"))
plot(nlcd_2015)

#reproject
nlcd_2015 <- project(nlcd_2015, "EPSG:4326")
plot(nlcd_2015)

# Recode all non-irrigated classes to 0 and irrigated class to 1
nlcd_2015[nlcd_2015 == 41524] <- 1
nlcd_2015[nlcd_2015!= 41524] <- 0

plot(nlcd_2015) #Error: [plot] something is wrong with the categories
hist(nlcd_2015)


# mapview  ---------------------------------------------------------------------------------
#create raster
ir_2005 <- rast(synergy_2005[c("x", "y", "ir_mean")], type="xyz")
crs(ir_2005) <- "+init=epsg:4326"

ir_2015 <- rast(synergy_2015[c("x", "y", "ir_mean")], type="xyz")
crs(ir_2015) <- "+init=epsg:4326"

nlcd_cor <- resample(rast(nlcd_2015), rast(ir_2015), method = "bilinear")

mapview::mapview(raster(nlcd_cor)) +
  mapview(raster(ir_2005)) +
  mapview(raster(ir_2015))

# scatterplot --------------------------------------------------------------------------------
#sample_2005 <- spatSample(ir_2005, size=250000, method = "regular", as.raster = TRUE)
#sample_2005 <- as.data.frame(sample_2005, xy = TRUE)

#sample_2015 <- spatSample(ir_2015, size=250000, as.raster = TRUE, method = "regular")
#sample_2015 <- as.data.frame(sample_2015, xy = TRUE)


