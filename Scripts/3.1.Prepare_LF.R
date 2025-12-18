###############################################################
# Script: 1.Prepare_LF.R
# Purpose: Prepare the 2022 LF observed sample with calibrated
#          weights per LF stratum for later allocation steps.
# Main steps:
# 1) Load master frame and 2022 survey microdata.
# 2) Ingest the effective LF sub-sample and derive LF strata.
# 3) Identify actually observed and eligible LF points.
# 4) Calibrate LF weights within each LF stratum.
# 5) Export the cleaned, calibrated 2022 LF observations.
# Inputs:
# - master_complete.RData
# - Survey_2022_wgt_2nd_phase.txt
# - effective_points_modules.xlsx
# - LF_sample_2022.csv
# Outputs:
# - LF_sample_2022_obs.csv
###############################################################
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
library(openxlsx)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LCobs <- substr(lucas22$SURVEY_LC1,1,1)
lucas22$LUobs <- substr(lucas22$SURVEY_LU1,1,3)
library(openxlsx)

load("master_complete.RData")

# ---- Load LF subsample inputs ----------------------------------------
# Read observed points in module sub-samples and LF 2022 sample
points <- read.xlsx("effective_points_modules.xlsx")
LF22 <- read.csv2("LF_sample_2022.csv",dec=".")
LF22$LC_pred <- NULL
LF22 <- merge(LF22,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
LF22 <- merge(LF22,lucas22[,c("POINT_ID","LCobs","LUobs")],by.x="ID",by.y="POINT_ID")
LF22$STRATUM_LF <- paste(LF22$NUTS2_24,LF22$STR25,sep="*")

# ---- Determine actually observed and eligible LF points --------------
# Select actually observed points
LF22obs <- LF22[LF22$ID %in% points$POINT_ID_LF,]


# Select eligible points
LF22obs <- LF22obs[LF22obs$STR25 %in% c(1,2,3) | LF22obs$LUobs == "U11",]
nrow(LF22obs)


# ---- Calibrate weights within LF strata ------------------------------
# Compute calibration factors to align observed LF weights to the full
# sample totals within each LF stratum
# g22_dom <- merge(LF22obs, lucas22[, c("POINT_ID","LCobs","LUobs")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- LF22
g22_dom <- g22_dom[g22_dom$STR25 %in% c(1,2,3) | g22_dom$LUobs == "U11",]

full_sums <- as.data.table(g22_dom)[
  # , .(sum_full = sum(WGT_LUCAS * WGT_LF / eligibility_rate_LF, na.rm = TRUE)),
  # , .(sum_full = sum(WGT_LUCAS * WGT_LF, na.rm = TRUE)),
  , .(sum_full = sum(WGT_LF, na.rm = TRUE)),
  by = STRATUM_LF
]
obs_sums <- as.data.table(LF22obs)[
  # , .(sum_obs = sum(WGT_LUCAS * WGT_LF / eligibility_rate_LF, na.rm = TRUE)),
  # , .(sum_obs = sum(WGT_LUCAS * WGT_LF, na.rm = TRUE)),
  , .(sum_obs = sum(WGT_LF, na.rm = TRUE)),
  by = STRATUM_LF
]
sum(g22_dom$WGT_LF,na.rm=T)

wgts <- merge(full_sums, obs_sums, by = "STRATUM_LF", all.x = TRUE)
wgts[, wgt_correction := sum_full / sum_obs]
wgts <- wgts[!is.na(wgts$STRATUM_LF),]

LF22obs <- merge(LF22obs, wgts[, .(STRATUM_LF, wgt_correction)], by = "STRATUM_LF", all.x = TRUE)
summary(g22_dom$WGT_LF)

# Coherence check
sum(g22_dom$WGT_LF,na.rm=T)

sum(LF22obs$WGT_LF * LF22obs$wgt_correction)

colnames(LF22obs)[colnames(LF22obs) == "ID"] <- "POINT_ID"
LF22obs <- LF22obs[!duplicated(LF22obs$POINT_ID),]

str(LF22obs)
table(LF22obs$STR25)

# ---- Export calibrated LF 2022 observations --------------------------
write.csv(LF22obs, "LF_sample_2022_obs.csv", row.names = FALSE, quote = TRUE)
