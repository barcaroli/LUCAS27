#------------------------------------------------
# Preparation of 2027 LF sub-sample 
#------------------------------------------------
# Description: prepares the 2027 LF sample
# by filtering observed 2022 LF points, re-calibrating weights
#------------------------------------------------
# Input datasets:
# - Survey_2022_wgt_2nd_phase.txt (observed 2022 survey)
# - effective_points_modules.xlsx (actually observed points by module)
# - LF_sample_2022.csv (original LF subsample with design info)
#------------------------------------------------
# Output datasets:
# - LF_sample_202_obs.csv 
#------------------------------------------------

# ---- Setup environment ------------------------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
library(openxlsx)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LCobs <- substr(lucas22$SURVEY_LC1,1,1)
lucas22$LUobs <- substr(lucas22$SURVEY_LU1,1,3)
library(openxlsx)

load("master_complete.RData")

# ---- Load LF subsample inputs ----------------------------------------
# Read observed points in module sub-samples
points <- read.xlsx("effective_points_modules.xlsx")
# Read 2022 LF sub-sample
LF22 <- read.csv2("LF_sample_2022.csv",dec=".")
LF22$LC_pred <- NULL
LF22 <- merge(LF22,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
LF22 <- merge(LF22,lucas22[,c("POINT_ID","LCobs","LUobs")],by.x="ID",by.y="POINT_ID")
LF22$STRATUM_LF <- paste(LF22$NUTS2_24,LF22$STR25,sep="*")

# ---- Determine actually observed and eligible LF points --------------
# Select actually observed points
# N.B.: ask for the difference betweeen Standard and Bulk, and if we can use them all or not
LF22obs <- LF22[LF22$ID %in% points$POINT_ID_LF,]
# LF22obs <- merge(LF22obs,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
# Select actually observed points in "E" and "D"
# LF22obs1 <- LF22obs[(LF22obs$LUobs == "U11" & LF22obs$LCobs == "E") | (LF22obs$LCobs == "B"),] 
LF22obs1 <- LF22obs[(LF22obs$LUobs == "U11" & LF22obs$STR25 == 3) | (LF22obs$STR25 %in% c(1,2)),] 
nrow(LF22obs1)
# [1] 64089
# (LU11 = 1 AND STR25=3) OR (STR25=1,2)
# LF22obs2 <- LF22obs[LF22obs$LCobs %in% c("B","E") | LF22obs$LUobs == "U11",]  
# LF22obs2 <- LF22obs[(LF22obs$LUobs == "U11") | (LF22obs$STR25 %in% c(1,2,3)),]
# nrow(LF22obs2)
# [1] 78318

LF22obs <- LF22obs1

# ---- Calibrate weights within LF strata -------------------------------
# Calculate calibrated weights
# g22_dom <- merge(LF22obs, lucas22[, c("POINT_ID","LCobs","LUobs")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- LF22
g22_dom <- g22_dom[(g22_dom$LUobs == "U11" & g22_dom$STR25 == 3) | (g22_dom$STR25 %in% c(1,2)),]

full_sums <- as.data.table(g22_dom)[
  # , .(sum_full = sum(WGT_LUCAS * WGT_LF / eligibility_rate_LFland, na.rm = TRUE)),
  # , .(sum_full = sum(WGT_LUCAS * WGT_LF, na.rm = TRUE)),
  , .(sum_full = sum(WGT_LF, na.rm = TRUE)),
  by = STRATUM_LF
]

obs_sums <- as.data.table(LF22obs)[
  # , .(sum_obs = sum(WGT_LUCAS * WGT_LF / eligibility_rate_LFland, na.rm = TRUE)),
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
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_LF / g22_dom$eligibility_rate_LFland)
# sum(LF22obs$WGT_LUCAS * LF22obs$WGT_LF / LF22obs$eligibility_rate_LFland * LF22obs$wgt_correction)
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_LF)
# sum(LF22obs$WGT_LUCAS * LF22obs$WGT_LF * LF22obs$wgt_correction)
sum(g22_dom$WGT_LF,na.rm=T)
# [1] 100569.7
sum(LF22obs$WGT_LF * LF22obs$wgt_correction)
# [1] 100569.7
colnames(LF22obs)[colnames(LF22obs) == "ID"] <- "POINT_ID"
LF22obs <- LF22obs[!duplicated(LF22obs$POINT_ID),]

str(LF22obs)
write.csv(LF22obs, "LF_sample_2022_obs.csv", row.names = FALSE, quote = TRUE)

