###############################################################
# Script: 7.Compute_LF_weights.R
# Purpose: Calibrate the 2027 LF sample to the master frame and
#          produce the final module weights (WGT_module_27).
# What it does now:
# - Loads LF component 1 and 2 and joins them
# - Loads master_complete.RData.
# - Recreates the LF population of interest and LF strata.
# - Calibrates each component to LF stratum totals, then merges and
#   performs a final calibration across all strata.
# - Exports diagnostic barplots (NUTS0, LC_pred) and overwrites
#   LF2027_sample.csv with the calibrated weights.
# Outputs:
# - LF2027_sample.csv (with WGT_module_27)
# - LF27_distribution_by_NUTS0.png
# - LF27_distribution_by_LCpred.png
###############################################################
setwd("D:/Google Drive/LUCAS 2026/dati")
comp1 <- read.csv("LF2027_panel.csv")
comp2 <- read.csv("LF2027_nonpanel.csv")
# ---- Merge components and export sample ------------------------------
LF <- rbind(comp1,comp2)
LF <- LF[!duplicated(LF$POINT_ID),]

head(LF[LF$component=="panel",])
head(LF[LF$component=="nonpanel",])
table(LF$component)


load("master_complete.RData")
# Population of interest
frame <- master_tot[(master_tot$LU11pred == 1) | (master_tot$STR25 %in% c(1,2,3)),]
nrow(frame)
 
sum(LF$WGT_LUCAS*(1/LF$eligibility_comp)*LF$wgt_module_22*LF$wgt_correction_22)
 
sum(LF$WGT_LUCAS[LF$component=="panel"]*
      (1/LF$eligibility_comp[LF$component=="panel"])*
      LF$wgt_module_22[LF$component=="panel"]*
      LF$wgt_correction_22[LF$component=="panel"])
 
sum(LF$WGT_LUCAS[LF$component=="nonpanel"]*
      (1/LF$eligibility_comp[LF$component=="nonpanel"])*
      LF$wgt_module_22[LF$component=="nonpanel"]*
      LF$wgt_correction_22[LF$component=="nonpanel"])
 
# boxplot(WGT_LUCAS~component,data=LF)
# boxplot(eligibility_comp~component,data=LF)
# boxplot(wgt_module_22~component,data=LF)
# boxplot(wgt_correction_22~component,data=LF)
# boxplot(WGT_comp_27~component,data=LF)

LF$STRATUM <- paste(LF$NUTS2,LF$STR25,sep="*")
frame$STRATUM <- paste(frame$NUTS2_24,frame$STR25,sep="*")
frame$ones <- 1
frame_totals <- aggregate(ones~STRATUM,data=frame,FUN=sum)
sum(frame_totals$ones)
 
#--------------------------------------------------------
# Calibration component 1
panel <- LF[LF$component == "panel",]
panel$WGT_LF <- panel$WGT_LUCAS *
                (1/panel$eligibility_comp) *
                panel$wgt_module_22 *
                panel$wgt_correction_22 *
                panel$WGT_comp_27
sum(panel$WGT_LF)
 
sum(frame_totals$ones)
 
sample_totals_1 <- aggregate(WGT_LF~STRATUM,data=panel,FUN=sum)
correction <- merge(frame_totals,sample_totals_1,by="STRATUM")
correction$correction_factor <- correction$ones / correction$WGT_LF
# Here there is all.x=TRUE because some strata in panel are not present in correction
panel <- merge(panel,correction[,c("STRATUM","correction_factor")],all.x=TRUE)
panel$correction_factor[is.na(panel$correction_factor)] <- 1
summary(panel$correction_factor)
panel$WGT_panel <- panel$WGT_LF * panel$correction_factor
sum(panel$WGT_panel)
 
panel$WGT_LF <- panel$correction_factor <- NULL
colnames(panel)[colnames(panel)=="WGT_panel"] <- "WGT_LF"

#--------------------------------------------------------
# Calibration component 2
nonpanel <- LF[LF$component == "nonpanel",]
summary(nonpanel$eligibility_comp)
nonpanel$WGT_LF <- nonpanel$WGT_LUCAS *
                   # (1/nonpanel$eligibility_comp) *  # modify to adopt the same mean eligibility used in panel component
                   (1/mean(nonpanel$eligibility_comp)) *
                   nonpanel$wgt_module_22 *
                   nonpanel$wgt_correction_22 *
                   nonpanel$WGT_comp_27
sum(nonpanel$WGT_LF)
 
sum(frame_totals$ones)
 
sample_totals_2 <- aggregate(WGT_LF~STRATUM,data=nonpanel,FUN=sum)
correction <- merge(frame_totals,sample_totals_2,by="STRATUM")
correction$correction_factor <- correction$ones / correction$WGT_LF
nonpanel <- merge(nonpanel,correction[,c("STRATUM","correction_factor")])
nonpanel$WGT_nonpanel <- nonpanel$WGT_LF * nonpanel$correction_factor
sum(nonpanel$WGT_nonpanel)
 
nonpanel$WGT_LF <- nonpanel$correction_factor <- NULL
colnames(nonpanel)[colnames(nonpanel)=="WGT_nonpanel"] <- "WGT_LF"

#--------------------------------------------------------
# Final calibration
LF <- rbind(panel,nonpanel)
sum(LF$WGT_LF)
 
correction <- merge(frame_totals,sample_totals_1,by="STRATUM")
correction <- merge(correction,sample_totals_2,by="STRATUM")
correction$WGT_LF_tot <- correction$WGT_LF.x + correction$WGT_LF.y
correction$correction_factor_1 <- correction$WGT_LF.x / correction$WGT_LF_tot
correction$correction_factor_2 <- correction$WGT_LF.y / correction$WGT_LF_tot
LF <- merge(LF,correction[,c("STRATUM","correction_factor_1","correction_factor_2")],all.x=TRUE)
LF$correction_factor_1[is.na(LF$correction_factor_1)] <- 1
LF$correction_factor_2[is.na(LF$correction_factor_2)] <- 1
LF$WGT_module_27[LF$component == "panel"] <- LF$WGT_LF[LF$component == "panel"] * 
  LF$correction_factor_1[LF$component == "panel"]
LF$WGT_module_27[LF$component == "nonpanel"] <- LF$WGT_LF[LF$component == "nonpanel"] * 
  LF$correction_factor_2[LF$component == "nonpanel"]
sum(LF$WGT_module_27)
 
LF$WGT_LF <- LF$correction_factor_1 <- LF$correction_factor_2 <- LF$STRATUM_LF <- NULL

write.table(LF,"LF2027_sample.csv",sep=",",quote=F,row.names=F)

# ---- Diagnostic barplots: sample vs master distributions (percentages) -----------------------
# helper to align levels and compute percentage shares
build_percentage_matrix <- function(sample_counts, master_counts) {
  all_levels <- union(names(sample_counts), names(master_counts))
  sample_counts <- sample_counts[all_levels]
  master_counts <- master_counts[all_levels]
  sample_counts[is.na(sample_counts)] <- 0
  master_counts[is.na(master_counts)] <- 0
  rbind(Sample = 100 * sample_counts / sum(sample_counts),
        Master = 100 * master_counts / sum(master_counts))
}

# NUTS0 (first 2 chars of NUTS2)
nuts0_sample <- table(substr(LF$NUTS2, 1, 2))
nuts0_master <- table(substr(frame$NUTS2_24, 1, 2))
nuts0_pct_matrix <- build_percentage_matrix(nuts0_sample, nuts0_master)
ylim_nuts0 <- c(0, max(nuts0_pct_matrix, na.rm = TRUE) * 1.1)

barplot(nuts0_pct_matrix,
        beside = TRUE,
        main = "LF 2027 distribution by NUTS0 (percentage)",
        xlab = "NUTS0",
        ylab = "Share of points (%)",
        col = c("orange", "steelblue"),
        las = 2,
        ylim = ylim_nuts0)
legend("topright", legend = c("Sample", "Master"), fill = c("orange", "steelblue"), bty = "n")

png("3.6.1.LF27_distribution_by_NUTS0.png", width = 1200, height = 800)
barplot(nuts0_pct_matrix,
        beside = TRUE,
        main = "LF 2027 distribution by NUTS0 (percentage)",
        xlab = "NUTS0",
        ylab = "Share of points (%)",
        col = c("orange", "steelblue"),
        las = 2,
        ylim = ylim_nuts0)
legend("topright", legend = c("Sample", "Master"), fill = c("orange", "steelblue"), bty = "n")
dev.off()

# LC_pred distribution
lcpred_sample <- table(LF$LC_pred)
lcpred_master <- table(frame$LC_pred)
lcpred_pct_matrix <- build_percentage_matrix(lcpred_sample, lcpred_master)
ylim_lcpred <- c(0, max(lcpred_pct_matrix, na.rm = TRUE) * 1.1)

barplot(lcpred_pct_matrix,
        beside = TRUE,
        main = "LF 2027 distribution by LC_pred (percentage)",
        xlab = "LC_pred",
        ylab = "Share of points (%)",
        col = c("orange", "steelblue"),
        las = 2,
        ylim = ylim_lcpred)
legend("topright", legend = c("Sample", "Master"), fill = c("orange", "steelblue"), bty = "n")

png("3.6.2.LF27_distribution_by_LCpred.png", width = 1200, height = 800)
barplot(lcpred_pct_matrix,
        beside = TRUE,
        main = "LF 2027 distribution by LC_pred (percentage)",
        xlab = "LC_pred",
        ylab = "Share of points (%)",
        col = c("orange", "steelblue"),
        las = 2,
        ylim = ylim_lcpred)
legend("topright", legend = c("Sample", "Master"), fill = c("orange", "steelblue"), bty = "n")
dev.off()
