#------------------------------------------------
# Preparation of 2027 LF sub-sample (component 1)
#------------------------------------------------
# Description: Builds the 2027 panel component for the LF sample
# by filtering observed 2022 LF points, re-calibrating weights,
# and selecting 46 500 units through proportional allocation.
#------------------------------------------------
# Input datasets:
# - Survey_2022_wgt_2nd_phase.txt (observed 2022 survey)
# - effective_points_modules.xlsx (actually observed points by module)
# - LF_sample_2022.csv (original LF subsample with design info)
#------------------------------------------------
# Output datasets:
# - LF2027_panel.csv (component 1 sample with updated weights/PRN)
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

# ---- Determine actually observed and eligible LF points --------------
# Select actually observed points
# N.B.: ask for the difference betweeen Standard and Bulk, and if we can use them all or not
LF22obs <- LF22[LF22$ID %in% points$POINT_ID_LF,]
LF22obs <- merge(LF22obs,master_tot[,c("POINT_ID","STR25")],by.x="ID",by.y="POINT_ID")
# Select actually observed points in "E" and "D"
LF22obs <- merge(LF22obs,lucas22[,c("POINT_ID","LCobs","LUobs")],by.x="ID",by.y="POINT_ID")
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
g22_dom <- merge(LF22, lucas22[, c("POINT_ID","LCobs","LUobs")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- g22_dom[g22_dom$LCobs %in% c("B","E") | g22_dom$LUobs == "U11", ]

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
LF22obs$STRATUM_LF <- LF22obs$NUTS0_16
summary(g22_dom$WGT_LF)

# ---- Allocate 46 500 units proportionally ----------------------------
# Coherence check
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_LF / g22_dom$eligibility_rate_LFland)
# sum(LF22obs$WGT_LUCAS * LF22obs$WGT_LF / LF22obs$eligibility_rate_LFland * LF22obs$wgt_correction)
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_LF)
# sum(LF22obs$WGT_LUCAS * LF22obs$WGT_LF * LF22obs$wgt_correction)
sum(g22_dom$WGT_LF,na.rm=T)
# [1] 124307.7
sum(LF22obs$WGT_LF * LF22obs$wgt_correction)
# [1] 124307.7
colnames(LF22obs)[colnames(LF22obs) == "ID"] <- "POINT_ID"

n <- 93000 / 2
n
# [1] 46500
#---------------------------------------------------------------------------------
# The following code is for the selection of 46500 points from LFobs22 in this way:
# - using STRATUM_LF a selection stratum, 
# - assigning a Permanent Random Number (PRN) to each point in the stratum
# - ordering the points in each stratum by PRN (descending)
# - selecting a number of points in each stratum so that the total amount is 46500
# - calculating a wgt_selection_component1 equal to ratio of the number o points in the stratum
#   and the number of points in the stratum

# Allocate the target of 46 500 points proportionally across strata (base R only)
stratum_counts <- aggregate(rep(1, nrow(LF22obs)),
                            by = list(STRATUM_LF = LF22obs$STRATUM_LF),
                            FUN = sum)
colnames(stratum_counts)[colnames(stratum_counts) == "x"] <- "n_in_stratum"
stratum_counts$target_n <- stratum_counts$n_in_stratum / sum(stratum_counts$n_in_stratum) * n
stratum_counts$min_select <- ifelse(stratum_counts$n_in_stratum == 1L, 1L, 2L)
base_alloc <- floor(stratum_counts$target_n)
stratum_counts$frac_part <- stratum_counts$target_n - base_alloc
stratum_counts$n_select <- pmax(base_alloc, stratum_counts$min_select)
stratum_counts$n_select <- pmin(stratum_counts$n_select, stratum_counts$n_in_stratum)
remaining <- n - sum(stratum_counts$n_select)
if (remaining > 0) {
  order_idx <- order(-stratum_counts$frac_part, stratum_counts$STRATUM_LF)
  for (i in order_idx) {
    if (remaining == 0) break
    if (stratum_counts$n_select[i] < stratum_counts$n_in_stratum[i]) {
      stratum_counts$n_select[i] <- stratum_counts$n_select[i] + 1L
      remaining <- remaining - 1L
    }
  }
}
if (remaining < 0) {
  order_idx <- order(stratum_counts$frac_part, stratum_counts$STRATUM_LF)
  for (i in order_idx) {
    if (remaining == 0) break
    if (stratum_counts$n_select[i] > stratum_counts$min_select[i]) {
      stratum_counts$n_select[i] <- stratum_counts$n_select[i] - 1L
      remaining <- remaining + 1L
    }
  }
}
stopifnot(remaining == 0L)
stratum_counts <- stratum_counts[order(stratum_counts$STRATUM_LF), ]
stratum_counts$wgt_selection_component1 <- ifelse(stratum_counts$n_select > 0,
                                                  stratum_counts$n_in_stratum / stratum_counts$n_select,
                                                  NA_real_)
stratum_counts$target_n <- NULL
stratum_counts$frac_part <- NULL
stratum_counts$min_select <- NULL
stopifnot(sum(stratum_counts$n_select, na.rm = TRUE) == n)

# ---- Randomize within strata and keep allocated units -----------------
# Assign PRN, order within strata, and keep the allocated number of points
set.seed(2027)
LF22obs$PRN_component1 <- runif(nrow(LF22obs))
LF22obs <- merge(LF22obs, stratum_counts, by = "STRATUM_LF", all.x = TRUE, sort = FALSE)
LF22obs <- LF22obs[order(LF22obs$STRATUM_LF, -LF22obs$PRN_component1), ]
LF22obs$rank_in_stratum <- ave(LF22obs$PRN_component1, LF22obs$STRATUM_LF,
                               FUN = function(x) seq_along(x))
LF22obs <- LF22obs[LF22obs$rank_in_stratum <= LF22obs$n_select, ]
LF22obs$rank_in_stratum <- NULL
LF22obs$n_in_stratum <- NULL
LF22obs$n_select <- NULL

sum(g22_dom$WGT_LF,na.rm=T)
# [1] 124307.7
sum(LF22obs$WGT_LF * LF22obs$wgt_correction * LF22obs$wgt_selection_component1)
# [1] 124307.7


# ---- Persist component-1 panel ---------------------------------------
write.table(LF22obs,"LF2027_panel.csv",sep=",",quote=T,row.names=F)



