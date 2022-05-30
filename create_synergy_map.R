# ========================================================================================
# Project:  IRRIGATION MAP
# Subject:  CREATE SYNERGY MAP
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
p_load(mapview, terra, sf)

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Scoring table
st_2005_raw <- read_excel(file.path(proc_path, paste0("priority_ranking/priority_ranking.xlsx")), sheet = "2005")
st_2015_raw <- read_excel(file.path(proc_path, paste0("priority_ranking/priority_ranking.xlsx")), sheet = "2015")

# Subnational irrigation data
ir_statistics <- read_excel(file.path(proc_path, "tables/provinces_chn.xlsx"), col_types = c("guess", "numeric", "numeric"))

# Synergy 2005 data.frame
chn_df_2005_raw <- readRDS(file.path(proc_path, "synergy_map/chn_df_2005.rds"))

# Synergy 2015 data.frame
chn_df_2015_raw <- readRDS(file.path(proc_path, "synergy_map/chn_df_2015.rds"))


# ========================================================================================
# CREATE SYNERGY MAP 2005 ---------------------------------------------------------------
# ========================================================================================

# ADD SCORE ------------------------------------------------------------------------------
# Create combined codes in scoring table
st_2005 <- st_2005_raw %>%
  gather(source, code_digit, -agreement, -score) %>%
  mutate(code = ifelse(code_digit == 1, source, 0)) %>%
  group_by(agreement, score) %>%
  summarize(code = paste0(code, collapse = "-"), .groups = "keep")
n_distinct(st_2005$code)
table(st_2005$code)

# Add grid_id, put in long format, calculate area and remove zeros
chn_df_2005 <- chn_df_2005_raw %>%
  mutate(grid_id = rownames(.)) %>%
  pivot_longer(-c(grid_id, x, y, NAME_1), names_to = "source", values_to = "area") %>%
  filter(area != 0)
summary(chn_df_2005)
str(chn_df_2005)

# irrigated area per input product
chn_df_2005 %>%
  group_by(source) %>%
  summarize(area = sum(area, na.rm = T))

# Code combinations. NOTE ORDER FACTOR SHOULD BE THE SAME AS SCORING TABLE!
ir_code_2005 <- chn_df_2005 %>%
  mutate(code = source,
         source = factor(source, levels = c("iam_asia_2000", "esa_2005", "gripc_2005", "iam_chn_2000", "gmia_nagaraj_2005"))) %>%
  dplyr::select(-area) %>%
  spread(source, code, fill = "0") %>%
  pivot_longer(-c(grid_id, x, y, NAME_1), names_to = "source", values_to = "code") %>%
  group_by(grid_id, x, y, NAME_1) %>%
  summarize(code = paste0(code, collapse = "-"),
            .groups = "drop") %>%
  left_join(st_2005)
summary(ir_code_2005)


# CALCULATE MEAN AREA PER GRID CELL AND COMBINE -------------------------------------------

ir_area_2005 <-  chn_df_2005 %>%
  group_by(grid_id) %>%
  summarize(ir_mean = mean(area, na.rm = T),
            ir_min = min(area, na.rm = T),
            ir_max = max(area, na.rm = T),
            .groups = "drop")
summary(ir_area_2005)
sum(ir_area_2005$ir_mean)
sum(ir_area_2005$ir_min)
sum(ir_area_2005$ir_max)

# combine
ir_df_2005 <- left_join(ir_code_2005, ir_area_2005, by = "grid_id")
summary(ir_df_2005)

# set slack to x times max grid size
slack = 5*max(chn_df_2005$area)

# Only include cells up to statistical area
synergy_2005 <- ir_df_2005 %>%
  left_join(ir_statistics) %>%
  ungroup() %>%
  arrange(NAME_1, score, desc(ir_mean)) %>%
  group_by(NAME_1) %>%
  mutate(adm1_cum = cumsum(ir_mean)) %>%
  filter(adm1_cum <= ir_2005*1000 + slack)
table(synergy_2005$score) # Max rank should be > 0
sum(synergy_2005$ir_mean)
sum(synergy_2005$ir_max)
sum(synergy_2005$ir_min)
sum(ir_statistics$ir_2005)*1000

# Create maps
synergy_map_2005 <- rast(synergy_2005[c("x", "y", "ir_mean", "ir_min", "ir_max", "score")], type="xyz")
crs(synergy_map_2005) <- "EPSG:4326"
plot(synergy_map_2005)
saveRDS(synergy_2005, file.path(proc_path, "synergy_map/synergy_2005.rds"))


# ========================================================================================
# CREATE SYNERGY MAP 2015 ----------------------------------------------------------------
# ========================================================================================

# ADD SCORE ------------------------------------------------------------------------------
# Create combined codes in scoring table
st_2015 <- st_2015_raw %>%
  gather(source, code_digit, -agreement, -score) %>%
  mutate(code = ifelse(code_digit == 1, source, 0)) %>%
  group_by(agreement, score) %>%
  summarize(code = paste0(code, collapse = "-"), .groups = "keep")
n_distinct(st_2015$code)
table(st_2015$code)

# Add grid_id, put in long format, calculate area and remove zeros
chn_df_2015 <- chn_df_2015_raw %>%
  mutate(grid_id = rownames(.)) %>%
  pivot_longer(-c(grid_id, x, y, NAME_1), names_to = "source", values_to = "area") %>%
  filter(area != 0)
summary(chn_df_2015)
str(chn_df_2015)

# irrigated area per input product
chn_df_2015 %>%
  group_by(source) %>%
  summarize(area = sum(area, na.rm = T))

# Code combinations. NOTE ORDER FACTOR SHOULD BE THE SAME AS SCORING TABLE!
ir_code_2015 <- chn_df_2015 %>%
  mutate(code = source,
         source = factor(source, levels = c("iam_asia_2010", "esa_2015", "gia_2010", "gmia_nagaraj_2015", "zohaib_2015"))) %>%
  dplyr::select(-area) %>%
  spread(source, code, fill = "0") %>%
  pivot_longer(-c(grid_id, x, y, NAME_1), names_to = "source", values_to = "code") %>%
  group_by(grid_id, x, y, NAME_1) %>%
  summarize(code = paste0(code, collapse = "-"),
            .groups = "drop") %>%
  left_join(st_2015)
summary(ir_code_2015)


# CALCULATE MEAN AREA PER GRID CELL AND COMBINE -------------------------------------------

ir_area_2015 <-  chn_df_2015 %>%
  group_by(grid_id) %>%
  summarize(ir_mean = mean(area, na.rm = T),
            ir_min = min(area, na.rm = T),
            ir_max = max(area, na.rm = T),
            .groups = "drop")
summary(ir_area_2015)
sum(ir_area_2015$ir_mean)
sum(ir_area_2015$ir_min)
sum(ir_area_2015$ir_max)

# combine
ir_df_2015 <- left_join(ir_code_2015, ir_area_2015, by = "grid_id")
summary(ir_df_2015)

# set slack to x times max grid size
slack = 5*max(chn_df_2015$area)

# Only include cells up to statistical area
synergy_2015 <- ir_df_2015 %>%
  left_join(ir_statistics) %>%
  ungroup() %>%
  arrange(NAME_1, score, desc(ir_mean)) %>%
  group_by(NAME_1) %>%
  mutate(adm1_cum = cumsum(ir_mean)) %>%
  filter(adm1_cum <= ir_2015*1000 + slack)
table(synergy_2015$score) # Max rank should be > 0
sum(synergy_2015$ir_mean)
sum(synergy_2015$ir_max)
sum(synergy_2015$ir_min)
sum(ir_statistics$ir_2015)*1000

# Create maps
synergy_map_2015 <- rast(synergy_2015[c("x", "y", "ir_mean", "ir_min", "ir_max", "score")], type="xyz")
crs(synergy_map_2015) <- "EPSG:4326"
plot(synergy_map_2015)
saveRDS(synergy_2015, file.path(proc_path, "synergy_map/synergy_2015.rds"))


# ========================================================================================
# COMPARE MAIN RESULTS -------------------------------------------------------------------
# ========================================================================================

par(mfrow=c(1,2))
plot(synergy_map_2005$ir_mean)
plot(synergy_map_2015$ir_mean)
