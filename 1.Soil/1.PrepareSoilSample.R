# ================================================================
# Script: 1.PrepareSoilSample.R
# Description: prepares the 2027 soil sub-sample starting from 2022 data,
#              filtering the effectively observed points, applying the provided
#              exclusions, and recalibrating the sampling weights.
# Input: Survey_2022_wgt_2nd_phase.txt, effective_points_modules.xlsx,
#        soil_sample_2022.csv, Soil_to_be_removed.xlsx
# Output: soil2022_processed.csv
# ================================================================
#------------------------------------
# Preparation of 2027 Soil sub-sample
#------------------------------------
# ---- Set the working environment and load the main data ----
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
library(openxlsx)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)
library(openxlsx)
# ---- Import operational datasets ----
# Observed points in the sub-sampling modules
points <- read.xlsx("effective_points_modules.xlsx")
# Full 2022 soil sample
soil22 <- read.csv2("soil_sample_2022.csv",dec=".")
# ---- Selection of the actually observed points ----
# Note: ask for the difference between Standard and Bulk and whether both can be used
soil22obs <- soil22[soil22$ID %in% points$POINT_ID_Soil.Standard | soil22$ID %in% points$POINT_ID_Soil.Bulk,]
# Select observed points with target land cover
soil22obs <- merge(soil22obs,lucas22[,c("POINT_ID","LC1")],by.x="ID",by.y="POINT_ID")
soil22obs <- soil22obs[soil22obs$LC1 %in% c("B","C","E","H"),]
nrow(soil22obs)
# [1] 28046
# ---- Apply the exclusion lists ----
# Exclusion of points (5959)
excl <- read.xlsx("Soil_to_be_removed.xlsx")
to_be_excluded <- c(excl$`POINT_ID.(XX01)`[!is.na(excl$`POINT_ID.(XX01)`)],
                    excl$`POINT_ID.(AB03)`[!is.na(excl$`POINT_ID.(AB03)`)],
                    excl$`POINT_ID.(SA07)`[!is.na(excl$`POINT_ID.(SA07)`)],
                    excl$`POINT_ID.(FA04)`[!is.na(excl$`POINT_ID.(FA04)`)],
                    excl$`POINT_ID.(AB02)`[!is.na(excl$`POINT_ID.(AB02)`)],
                    excl$`POINT_ID.(AB10)`[!is.na(excl$`POINT_ID.(AB10)`)])
to_be_excluded <- to_be_excluded[!duplicated(to_be_excluded)]
length(to_be_excluded)
# 7716    : it should be 5959!

soil22obs <- soil22obs[!(soil22obs$ID %in% to_be_excluded),]
nrow(soil22obs)
# [1] 28010

# ---- Compute corrected weights by stratum ----
# Calculate calibrated weights
g22_dom <- merge(soil22, lucas22[, c("POINT_ID","LC1")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- g22_dom[g22_dom$LC1 %in% c("B","C","E","H"), ]

full_sums <- as.data.table(g22_dom)[
  # , .(sum_full = sum(WGT_LUCAS * WGT_SOIL / eligibility_rate_soilland, na.rm = TRUE)),
  # , .(sum_full = sum(WGT_LUCAS * WGT_SOIL, na.rm = TRUE)),
  , .(sum_full = sum(WGT_SOIL, na.rm = TRUE)),
  by = STRATUM_SOIL
]

obs_sums <- as.data.table(soil22obs)[
  # , .(sum_obs = sum(WGT_LUCAS * WGT_SOIL / eligibility_rate_soilland, na.rm = TRUE)),
  # , .(sum_obs = sum(WGT_LUCAS * WGT_SOIL, na.rm = TRUE)),
  , .(sum_obs = sum(WGT_SOIL, na.rm = TRUE)),
  by = STRATUM_SOIL
]

wgts <- merge(full_sums, obs_sums, by = "STRATUM_SOIL", all.x = TRUE)
wgts[, wgt_correction := sum_full / sum_obs]

soil22obs <- merge(soil22obs, wgts[, .(STRATUM_SOIL, wgt_correction)], by = "STRATUM_SOIL", all.x = TRUE)

# ---- Consistency check and save ----
# Coherence check
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_SOIL / g22_dom$eligibility_rate_soilland)
# sum(soil22obs$WGT_LUCAS * soil22obs$WGT_SOIL / soil22obs$eligibility_rate_soilland * soil22obs$wgt_correction)
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_SOIL)
# sum(soil22obs$WGT_LUCAS * soil22obs$WGT_SOIL * soil22obs$wgt_correction)
sum(g22_dom$WGT_SOIL)
# [1] 162811.8
sum(soil22obs$WGT_SOIL * soil22obs$wgt_correction)
# [1] 162810.8
colnames(soil22obs)[2] <- "POINT_ID"
write.table(soil22obs,"soil2022_processed.csv",sep=",",quote=T,row.names=F)



