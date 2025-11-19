# ------------------------------------------------------------------------------
# Overview: builds component 1 (non-extended Grassland) of the LUCAS 2027 sample
# from the 2022 survey weights and data.
# Main inputs:
#   - Survey_2022_wgt_2nd_phase.txt (weights/attributes of 2022 points)
#   - effective_points_modules.xlsx (actually observed module points)
#   - grassland_sample_2022.csv (Grassland 2022 sample with weights)
# Output:
#   - Grassland2027_component1.csv (selected points with weight correction)
# ------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Selection of 2027 Grassland sub-sample (component Grassland not extended)
#--------------------------------------------------------------------------
# --- Environment setup and core inputs ---
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)
library(openxlsx)
# --- Load effective points and Grassland 2022 sample ---
# Read observed points in module sub-samples
points <- read.xlsx("effective_points_modules.xlsx")
# Read 2022 Grassland sub-sample
grass22 <- read.csv2("grassland_sample_2022.csv",dec=".")
# --- Keep only observed points falling into classes D/E ---
# Select actually observed points
grass22obs <- grass22[grass22$ID %in% points$POINT_ID_Grassland,]
# Select actually observed points in "E" and "D"
grass22obs <- merge(grass22obs,lucas22[,c("POINT_ID","LC1")],by.x="ID",by.y="POINT_ID")
grass22obs <- grass22obs[grass22obs$LC1 %in% c("E","D"),]
nrow(grass22obs)
# [1] 11099
# --- Compute weight correction by grassland stratum ---
# Calculate calibrated weights
g22_dom <- merge(grass22, lucas22[, c("POINT_ID","LC1")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- g22_dom[g22_dom$LC1 %in% c("E","D"), ]

full_sums <- as.data.table(g22_dom)[
  # , .(sum_full = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  # , .(sum_full = sum(WGT_LUCAS * WGT_GRASSLAND, na.rm = TRUE)),
  , .(sum_full = sum(WGT_GRASSLAND, na.rm = TRUE)),
  by = STRATUM_GRASSLAND
]

obs_sums <- as.data.table(grass22obs)[
  # , .(sum_obs = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  # , .(sum_obs = sum(WGT_LUCAS * WGT_GRASSLAND, na.rm = TRUE)),
  , .(sum_obs = sum(WGT_GRASSLAND, na.rm = TRUE)),
  by = STRATUM_GRASSLAND
]

wgts <- merge(full_sums, obs_sums, by = "STRATUM_GRASSLAND", all.x = TRUE)
wgts[, wgt_correction := sum_full / sum_obs]

grass22obs <- merge(grass22obs, wgts[, .(STRATUM_GRASSLAND, wgt_correction)], by = "STRATUM_GRASSLAND", all.x = TRUE)

# --- Consistency checks on weighted totals ---
# Coherence check
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_GRASSLAND / g22_dom$eligibility_rate_grassland)
# sum(grass22obs$WGT_LUCAS * grass22obs$WGT_GRASSLAND / grass22obs$eligibility_rate_grassland * grass22obs$wgt_correction)
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_GRASSLAND)
# sum(grass22obs$WGT_LUCAS * grass22obs$WGT_GRASSLAND * grass22obs$wgt_correction)
sum(g22_dom$WGT_GRASSLAND)
# [1] 46530.1
sum(grass22obs$WGT_GRASSLAND * grass22obs$wgt_correction)
# [1] 46530.1
# --- Prepare final variables and export component 1 ---
colnames(grass22obs)[2] <- "POINT_ID"
write.table(grass22obs,"Grassland2027_component1.csv",sep=",",quote=T,row.names=F)



