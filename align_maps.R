# ========================================================================================
# Project:  IRRIGATION MAP
# Subject:  Script to calculate irrigated area of irrigation products
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue)

# Load additional packages
p_load(sf, terra)

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET COUNTRY ----------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "CHN"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# China polygon
adm <- vect(file.path(proc_path, "adm/adm1.shp"))

# 2000 cropland mask
cl_2000_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2000.tif"))

# 2005 cropland mask
cl_2005_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2005.tif"))

# 2010 cropland mask
cl_2010_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2010.tif"))

# 2015 cropland mask
cl_2015_raw <- rast(file.path(raw_path, "esacci/cropland/esacci_2015.tif"))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================

# Create folder for maps
temp_path <- file.path(proc_path, "input_maps")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

# Create cropland maps for China
cl_2000 <- crop(cl_2000_raw, adm)
cl_2000 <- mask(cl_2000, adm)
names(cl_2000) <- "cropland_2000"
plot(cl_2000)

cl_2005 <- crop(cl_2005_raw, adm)
cl_2005 <- mask(cl_2005, adm)
names(cl_2005) <- "cropland_2005"
plot(cl_2005)

cl_2010 <- crop(cl_2010_raw, adm)
cl_2010 <- mask(cl_2010, adm)
names(cl_2010) <- "cropland_2010"
plot(cl_2010)

cl_2015 <- crop(cl_2015_raw, adm)
cl_2015 <- mask(cl_2015, adm)
names(cl_2015) <- "cropland_2015"
plot(cl_2015)


# ========================================================================================
# PROCESS ESA 2005 -----------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "esacci/raw/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r!=20] <- 0
r[r == 20] <- 1
plot(r)

# ESA maps have a resolution of 10 arcsec, while our target resolution is 30 arcsec.
# As these are an exact multiplication of 3, the rasters exactly overlap and we can simply average.
# This means we counts all 10 arcsec cells with value = 1 (irrigated) and divide by the total number
# 10 arcsec cells that fit into a 30 arcsec cell.
# This will give the share of irrigated area in a 30 arcsec grid cell.
r <- aggregate(r, fact = 3, na.rm = TRUE)
names(r) <- "esa_2005"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
hist(r)

# Correct values so that they are <= to cropland mask
# Need to resample the cl_2007 map so extends are exactly the same before we can combine
cl_2005_esa <- resample(cl_2005, r, method = "bilinear")
r <- c(cl_2005_esa, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(esa_2005 = ifelse(esa_2005 > cropland_2005, cropland_2005, esa_2005))
summary(r)
r <- rast(r[c("x", "y", "esa_2005")], type="xyz", crs = "EPSG:4326")

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "esa_2005_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS ESA 2015 -----------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "esacci/raw/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r!=20] <- 0
r[r == 20] <- 1
plot(r)

# ESA maps have a resolution of 10 arcsec, while our target resolution is 30 arcsec.
# As these are an exact multiplication of 3, the rasters exactly overlap and we can simply average.
# This means we counts all 10 arcsec cells with value = 1 (irrigated) and divide by the total number
# 10 arcsec cells that fit into a 30 arcsec cell.
# This will give the share of irrigated area in a 30 arcsec grid cell.
r <- aggregate(r, fact = 3, na.rm = TRUE)
names(r) <- "esa_2015"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
hist(r)

# Correct values so that they are <= to cropland mask
# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2015_esa <- resample(cl_2015, r, method = "bilinear")
r <- c(cl_2015_esa, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(esa_2015 = ifelse(esa_2015 > cropland_2015, cropland_2015, esa_2015))
summary(r)
r <- rast(r[c("x", "y", "esa_2015")], type="xyz", crs = "EPSG:4326")

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "esa_2015_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS gmia_nagaraj_2015 --------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "gmia_nagaraj/v3b_combined_2015.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r!=2] <- 0
r[r == 2] <- 1
plot(r)

# gmia_nagaraj map has a resolution of 9 km, while our target resolution is 30 arsec = 1 km.
# with disagg() the cell size can be adjusted to 9 times smaller
r <- disagg(r, fact = 10)
names(r) <- "gmia_nagaraj_2015"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
plot(r)
hist(r)

# Calculate size of irrigated area
# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2015_gmia_nagaraj <- resample(cl_2015, r, method = "bilinear")
r_area <- cellSize(r, unit = "ha")
r <- cl_2015_gmia_nagaraj*r*r_area
names(r) <- "gmia_nagaraj_2015"
plot(r)
global(r, "sum", na.rm = TRUE)
hist(r)

# Save
writeRaster(r, file.path(temp_path, "gmia_nagaraj_CHN_2015.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS gmia_nagaraj_2005 --------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "gmia_nagaraj/v3b_combined_2005.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r!=2] <- 0
r[r == 2] <- 1
plot(r)

# gmia map has a resolution of 9 km, while our target resolution is 30 arsec = 1 km.
# with disagg() the cell size can be adjusted to 9 times smaller
r <- disagg(r, fact = 10)
names(r) <- "gmia_nagaraj_2005"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
plot(r)
hist(r)

# Calculate size of irrigated area
# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2005_gmia_nagaraj <- resample(cl_2005, r, method = "bilinear")
r_area <- cellSize(r, unit = "ha")
r <- cl_2005_gmia_nagaraj*r*r_area
names(r) <- "gmia_nagaraj_2005"
plot(r)
global(r, "sum", na.rm = TRUE)
hist(r)

# Save
writeRaster(r, file.path(temp_path, "gmia_nagaraj_CHN_2005.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS GIA  ------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "gia/global_irrigated_areas_crs.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r== 0] <- 0
r[r != 0] <- 1
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
names(r) <- "gia_2010"
r
plot(r)
hist(r)

# Calculate size of irrigated area
# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2010_gia <- resample(cl_2010, r, method = "bilinear")
r_area <- cellSize(r, unit = "ha")
r <- cl_2010_gia*r*r_area
names(r) <- "gia_2010"
plot(r)
global(r, "sum", na.rm = TRUE)
hist(r)

# Save
writeRaster(r, file.path(temp_path, "gia_2010_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS zohaib  ------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "zohaib/global_actual_irrigated_area.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
# The map contains only irrigated values, non-irrigated areas are not included.
# therefore only class 1 is mapped and zero values are added
r[r!=0] <- 1
r[is.na(r)] <- 0
r <- mask(r, adm)
plot(r)

# zohaib map has a resolution of 25 km, while our target resolution is 30 arsec = 1 km.
# with disagg() the cell size can be adjusted to 30 times smaller, which is nearly the desired resolution.
r <- disagg(r, fact = 30)
names(r) <- "zohaib_2015"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
plot(r)
hist(r)

# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2015_zohaib <- resample(cl_2015, r, method = "bilinear")
r_area <- cellSize(r, unit = "ha")
r <- cl_2015_zohaib*r*r_area
names(r) <- "zohaib_2015"
plot(r)
global(r, "sum", na.rm = TRUE)
hist(r)

# Save
writeRaster(r, file.path(temp_path, "zohaib_2015_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS GRIPC  ------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "gripc/GRIPCmap/GRIPC_irrigated_area.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Values represent irrigated hectares in a 5 arcmin grid cell, we create shares.
# Calculate grid cell in ha
r_area <- cellSize(r, unit = "ha")
r <- r/r_area
plot(r)
freq(r)
hist(r)

# Disaggregate to 30 arcsec
r <- disagg(r, fact = 10)
names(r) <- "gripc_2005"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
plot(r)
hist(r)

# Need to resample the cl_2007 map so extends are exactly the same before we can combine
cl_2005_gripc <- resample(cl_2005, r, method = "bilinear")
r <- c(cl_2005_gripc, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(gripc_2005 = ifelse(gripc_2005 > cropland_2005, cropland_2005, gripc_2005))
summary(r)
r <- rast(r[c("x", "y", "gripc_2005")], type="xyz", crs = "EPSG:4326")

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "gripc_2005_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS iam_asia_2000 ------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "iam_asia/irri_area_asia_2000/IRRA_Asia_Agriculture_2000_V21.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

##################### CHECK WHICH ONE IS CORRECT?
# Recode all non-irrigated classes to 0 and irrigated class to 1
#tryout
# r[r %in%  c(0, 1, 2,3,4)] <- 0
# r[r %in% c(4,5,10)] <- 1
# plot(r)


#actual
r[r %in%  c(0, 4, 5, 10)] <- 0
r[r %in% c(1:3)] <- 1
plot(r)
r

# iam_asia maps have a resolution of (0.002083, 0.002083), while our target resolution is (0.008333 x 0.008333).
# by aggregating the map with factor 4, the correct resolution is set
r <- aggregate(r, fact = 4, na.rm = TRUE)
names(r) <- "iam_asia_2000"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
hist(r)

# Need to resample the cl_2007 map so extends are exactly the same before we can combine
cl_2000_iam_asia_2000 <- resample(cl_2000, r, method = "bilinear")
r <- c(cl_2000_iam_asia_2000, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(iam_asia_2000 = ifelse(iam_asia_2000 > cropland_2000, cropland_2000, iam_asia_2000))
summary(r)
r <- rast(r[c("x", "y", "iam_asia_2000")], type="xyz", crs = "EPSG:4326")
plot(r)

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "iam_asia_2000_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS iam_asia_2010 ------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "iam_asia/irri_area_asia_2010/iam_asia_2010_irrigated_area.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)

# Recode all non-irrigated classes to 0 and irrigated class to 1
r[r %in% c(0, 1, 2)] <- 1
r[r %in% c(3, 4, 5, 6)] <- 0
plot(r)

# iam_asia maps have a resolution of (0.002083, 0.002083), while our target resolution is (0.008333 x 0.008333).
# by aggregating the map with factor 4, the correct resolution is set
r <- aggregate(r, fact = 4, na.rm = TRUE)
names(r) <- "iam_asia_2010"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r
hist(r)

#check recoding
plot(r)

# Need to resample the cl_2015 map so extends are exactly the same before we can combine
cl_2010_iam_asia_2010 <- resample(cl_2010, r, method = "bilinear")
r <- c(cl_2010_iam_asia_2010, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(iam_asia_2010 = ifelse(iam_asia_2010 > cropland_2010, cropland_2010, iam_asia_2010))
summary(r)
r <- rast(r[c("x", "y", "iam_asia_2010")], type="xyz", crs = "EPSG:4326")
plot(r)

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "iam_asia_2010_CHN.tif"), overwrite = TRUE)


# ========================================================================================
# PROCESS iam_chn_2000 ------------------------------------------------------------------
# ========================================================================================

# Note that this file has strange attributes (histogram), which are not read correctly by rast
# but do work with raster. To solve this we rewrite it. This only has to be done once
#input <- file.path(raw_path, "iam_chn_2000/iirmapped/mapped/irrigatedland2000_china_albers.tif")
# r <- raster::raster(input)
# writeRaster(r, file.path(raw_path, "iam_chn_2000/iirmapped/mapped/irrigatedland2000_china_albers_m.tif"))

input <- file.path(raw_path, "iam_chn_2000/iirmapped/mapped/irrigatedland2000_china_albers_m.tif")

# reproject
r <- project(rast(input), "EPSG:4326")
r <- mask(r, adm)
r <- crop(r, adm)
names(r) <- "iam_chn_2000"
plot(r)

# Check that the resolution is correct (0.008333 x 0.008333) and values are between 0 and 1
r <- r/100
hist(r)

# Resample to correct resolution
r <- resample(r, cl_2000, method = "bilinear")
plot(r)

# Correct values so that they are <= to cropland mask
r <- c(cl_2000, r)
r <- as.data.frame(r, xy = TRUE) %>%
  mutate(iam_chn_2000 = ifelse(iam_chn_2000 > cropland_2000, cropland_2000, iam_chn_2000))
summary(r)
r <- rast(r[c("x", "y", "iam_chn_2000")], type="xyz", crs = "EPSG:4326")
plot(r)

# Create grid cell area map using ESA 2005 as template
r_area <- cellSize(r, unit = "ha")
r <- r*r_area
plot(r)
global(r, "sum", na.rm = TRUE)

# Save
writeRaster(r, file.path(temp_path, "iam_chn_2000.tif"), overwrite = TRUE)


# ========================================================================================
# GMIA -------------------------------------------------------------------------------------
# ========================================================================================

# Clip from global map
input <- file.path(raw_path, "gmia/gmia_v5_aei_ha_crs.tif")
r <- crop(rast(input), adm)
r <- mask(r, adm)
plot(r)
names(r) <- "gmia_2000_2008"

# Check that the resolution is correct (0.008333 x 0.008333) and coordinate system EPSG:4326
r
crs(r) <- "+init=epsg:4326"
r[r == 0] <- NA
na.omit(r)
hist(r)

# Save
writeRaster(r, file.path(temp_path, "gmia.tif"), overwrite = TRUE)

