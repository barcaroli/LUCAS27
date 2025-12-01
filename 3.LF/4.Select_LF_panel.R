#------------------------------------------------
# Selection of 2027 LF sub-sample (component 1)
#------------------------------------------------
# Description: Selects the 2027 panel component for the LF sample
#------------------------------------------------
# Input datasets:
# - LF_sample_2022_obs.csv (original LF subsample with design info)
#------------------------------------------------
# Output datasets:
# - LF2027_panel.csv (component 1 sample with updated weights/PRN)
#------------------------------------------------
panel_units <- 46500
# ---- Setup environment ------------------------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")
LF22 <- read.csv("LF_sample_2022_obs.csv",dec=".")
load("master_complete.RData")
LF22 <- merge(LF22,master_tot[,c("POINT_ID","PRN")])
# table(LF22$STRATUM_LF)

#---------------------------------------------------------------------------------
# The following code is for the selection of 46500 points from LFobs22 in this way:
# - using STRATUM_LF a selection stratum, 
# - assigning a Permanent Random Number (PRN) to each point in the stratum
# - ordering the points in each stratum by PRN (descending)
# - selecting a number of points in each stratum so that the total amount is 46500
# - calculating a wgt_selection_component1 equal to ratio of the number o points in the stratum
#   and the number of points in the stratum

n <- panel_units
n
# [1] 46500

# Choose the allocation (proportional or optimal)
allocation <- read.csv("LF_strata_with_bethel_allocation.csv")
head(allocation)
stratum_counts <- aggregate(rep(1, nrow(LF22)),
                            by = list(STRATUM_LF = LF22$STRATUM_LF),
                            FUN = sum)
stratum_counts <- merge(stratum_counts,allocation[,c("STRATO","nh_opt","nh_prop"),],by.x="STRATUM_LF",by.y="STRATO")
colnames(stratum_counts)[colnames(stratum_counts) == "x"] <- "n_in_stratum"
# stratum_counts$target_n <- stratum_counts$n_in_stratum / sum(stratum_counts$n_in_stratum) * n
#--- Here we choose the optimal allocation -----
stratum_counts$target_n <- stratum_counts$nh_opt
#-----------------------------------------------
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
# set.seed(2027)
# LF22$PRN_component1 <- runif(nrow(LF22))
LF22 <- merge(LF22, stratum_counts, by = "STRATUM_LF", all.x = TRUE, sort = FALSE)
LF22 <- LF22[order(LF22$STRATUM_LF, -LF22$PRN), ]
LF22$rank_in_stratum <- ave(LF22$PRN, LF22$STRATUM_LF,
                               FUN = function(x) seq_along(x))
LF22samp <- LF22[LF22$rank_in_stratum <= LF22$n_select, ]
LF22samp$rank_in_stratum <- NULL
LF22samp$n_in_stratum <- NULL
LF22samp$n_select <- NULL

sum(LF22$WGT_LF,na.rm=T)
# [1] 97635.88
sum(LF22samp$WGT_LF * LF22samp$wgt_selection_component1)
# [1] 97635.88


# ---- Persist component-1 panel ---------------------------------------
samp <- NULL
samp$POINT_ID <- LF22samp$POINT_ID
samp$module <- "LF"
samp$component <- "panel"
samp$NUTS2 <- LF22samp$NUTS2_24
samp$LC_pred <- LF22samp$LC_pred
samp$STR25 <- LF22samp$STR25
samp$WGT_LUCAS <- LF22samp$WGT_LUCAS 
samp$WGT_comp <- LF22samp$wgt_selection_component1
samp$eligibility_comp <- LF22samp$eligibility_rate_LF
samp$wgt_correction <- LF22samp$wgt_correction
samp$wgt_selection <- LF22samp$WGT_LF
samp <- as.data.frame(samp)
write.table(samp,"LF2027_panel.csv",sep=",",quote=T,row.names=F)



