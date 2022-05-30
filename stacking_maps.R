# ========================================================================================
# Project:  IRRIGATION MAP
# Subject:  Stacking maps
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
p_load(here, tidyverse, readxl, stringr, scales, glue)

# Load spatial packages
p_load(sf, terra)

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


# 2005 -----------------------------------------------------------------------------------
gripc_2005 <- file.path(proc_path, "input_maps/gripc_2005_CHN.tif")
esa_2005 <- file.path(proc_path, "input_maps/esa_2005_CHN.tif")
iam_asia_2000 <- file.path(proc_path, "input_maps/iam_asia_2000_CHN.tif")
iam_chn_2000 <- file.path(proc_path, "input_maps/iam_chn_2000.tif")
gmia_nagaraj_2005 <- file.path(proc_path, "input_maps/gmia_nagaraj_CHN_2005.tif")

# pal = mapviewPalette("mapviewSpectralColors")
# pal2 = mapviewPalette("mapviewTopoColors")
#
# mapview(raster(gripc_2005), col.regions = pal(100)) +
#   mapview(raster(esa_2000)) + # , col.regions = "red"
#   mapview(raster(iam_asia_2000), col.regions = pal2(100), alpha.regions = 0.5) +
#   mapview(raster(iam_chn_2000)) +
#   mapview(raster(gmia_nagaraj_2005))

# 2015 -------------------------------------------------------------------------------------
zohaib_2015 <- file.path(proc_path, "input_maps/zohaib_2015_CHN.tif")
esa_2015 <- file.path(proc_path, "input_maps/esa_2015_CHN.tif")
iam_asia_2010 <- file.path(proc_path, "input_maps/iam_asia_2010_CHN.tif")
gia_2010 <- file.path(proc_path, "input_maps/gia_2010_CHN.tif")
gmia_nagaraj_2015 <- file.path(proc_path, "input_maps/gmia_nagaraj_CHN_2015.tif")

# pal = mapviewPalette("mapviewSpectralColors")
# pal2 = mapviewPalette("mapviewTopoColors")
#
# mapview(raster(zohaib_2015), col.regions = pal(100)) +
#   mapview(raster(esa_2015)) + # , col.regions = "red"
#   mapview(raster(iam_asia_2010), col.regions = pal2(100), alpha.regions = 0.5) +
#   mapview(raster(gia_2010)) +
#   mapview(raster(gmia_nagaraj_2015))

# China polygon
adm <- vect(file.path(proc_path, "adm/adm1.shp"))
plot(adm)


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================

# Create folder for maps
temp_path <- file.path(proc_path, "synergy_map")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

# Rasterize adm
adm_2015 <- rasterize(adm, rast(esa_2015), field = "NAME_1")
plot(adm_2015)

# Rasterize
adm_2005 <- rasterize(adm, rast(esa_2005), field = "NAME_1")
plot(adm_2005)


# ========================================================================================
# STACK 2005 -----------------------------------------------------------------------------
# ========================================================================================

# convert to SpatRaster
gripc_2005 <- rast(gripc_2005)
esa_2005 <- rast(esa_2005)
iam_asia_2000 <- rast(iam_asia_2000)
iam_chn_2000 <- rast(iam_chn_2000)
gmia_nagaraj_2005 <- rast(gmia_nagaraj_2005)

# Check extend, if error trim all maps to the same dimensions and resolution
gripc_2005            #extend differs from esa
esa_2005
iam_asia_2000        #ymin extend differs from esa
iam_chn_2000               #ymin extend differs from esa
gmia_nagaraj_2005      #only xmax is similar

# trim maps with resample to the dimension and resolution of esa
gripc_cor <- resample(gripc_2005, esa_2005, method = "bilinear")
gmia_nagaraj_cor <- resample(gmia_nagaraj_2005, esa_2005, method = "bilinear")
iam_asia_cor <- resample(iam_asia_2000, esa_2005, method = "bilinear")
iam_chn_cor <- resample(iam_chn_2000, esa_2005, method = "bilinear")

# Combine rasters in 'terra' package with 'c'
chn_stack_2005 <- c(gripc_cor, esa_2005, iam_asia_cor, iam_chn_cor, gmia_nagaraj_cor, adm_2005)

global(chn_stack_2005, "sum", na.rm = TRUE)
plot(chn_stack_2005)
chn_df_2005 <- as.data.frame(chn_stack_2005, xy=TRUE)


# ========================================================================================
# STACK 2015 -----------------------------------------------------------------------------
# ========================================================================================

# convert to SpatRaster
zohaib_2015 <- rast(zohaib_2015)
esa_2015 <- rast(esa_2015)
iam_asia_2010 <- rast(iam_asia_2010)
gia_2010 <- rast(gia_2010)
gmia_nagaraj_2015 <- rast(gmia_nagaraj_2015)

# Check extend, if error trim all maps to the same dimensions and resolution
zohaib_2015            #extend differs from esa
esa_2015
iam_asia_2010           #ymin extend differs from esa
gia_2010
gmia_nagaraj_2015      #only xmax is similar

#trim maps with resample to the dimension and resolution of esa
iam_asia_2010_cor <- resample(iam_asia_2010, esa_2015, method = "bilinear")
gmia_nagaraj_cor <- resample(gmia_nagaraj_2015, esa_2015, method = "bilinear")
zohaib_cor <- resample(zohaib_2015, esa_2015, method = "bilinear")
gia_cor <- resample(gia_2010, esa_2015, method = "bilinear")

# Combine rasters in 'terra' package with 'c'
chn_stack_2015 <- c(gia_cor, gmia_nagaraj_cor, zohaib_cor, esa_2015, iam_asia_2010_cor, adm_2015)
global(chn_stack_2015, "sum", na.rm = TRUE)
plot(chn_stack_2015)
chn_df_2015 <- as.data.frame(chn_stack_2015, xy = TRUE)


# ========================================================================================
# SAVE ----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(chn_df_2005, file.path(temp_path, "chn_df_2005.rds"))
saveRDS(chn_df_2015, file.path(temp_path, "chn_df_2015.rds"))
