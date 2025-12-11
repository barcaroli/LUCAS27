###############################################################
# Script: 7.Compute_LF_weights.R
# Purpose: Calibrate the 2027 LF sample to the master frame and
#          produce the final module weights (WGT_module_27).
# What it does now:
# - Loads LF2027_sample.csv and master_complete.RData.
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
LF <- read.csv("LF2027_sample.csv")
head(LF[LF$component=="panel",])
head(LF[LF$component=="nonpanel",])
table(LF$component)
load("master_complete.RData")
# Population of interest
frame <- master_tot[(master_tot$LU11pred == 1) | (master_tot$STR25 %in% c(1,2,3)),]
nrow(frame)
# [1] 434155
sum(LF$WGT_LUCAS*(1/LF$eligibility_comp)*LF$wgt_module_22*LF$wgt_correction_22)
# [1] 567893.1
sum(LF$WGT_LUCAS[LF$component=="panel"]*
      (1/LF$eligibility_comp[LF$component=="panel"])*
      LF$wgt_module_22[LF$component=="panel"]*
      LF$wgt_correction_22[LF$component=="panel"])
# 278445.8
sum(LF$WGT_LUCAS[LF$component=="nonpanel"]*
      (1/LF$eligibility_comp[LF$component=="nonpanel"])*
      LF$wgt_module_22[LF$component=="nonpanel"]*
      LF$wgt_correction_22[LF$component=="nonpanel"])
# 289650.8
# boxplot(WGT_LUCAS~component,data=LF)
# boxplot(eligibility_comp~component,data=LF)
# boxplot(wgt_module_22~component,data=LF)
# boxplot(wgt_correction_22~component,data=LF)
# boxplot(WGT_comp_27~component,data=LF)

LF$STRATUM_LF <- paste(LF$NUTS2,LF$STR25,sep="*")
frame$STRATUM_LF <- paste(frame$NUTS2_24,frame$STR25,sep="*")
frame$ones <- 1
frame_totals <- aggregate(ones~STRATUM_LF,data=frame,FUN=sum)
sum(frame_totals$ones)
# [1] 434155
#--------------------------------------------------------
# Calibration component 1
panel <- LF[LF$component == "panel",]
panel$WGT_LF <- panel$WGT_LUCAS *
                (1/panel$eligibility_comp) *
                panel$wgt_module_22 *
                panel$wgt_correction_22 *
                panel$WGT_comp_27
sum(panel$WGT_LF)
# [1] 477565.2
sum(frame_totals$ones)
# [1] 434155
sample_totals <- aggregate(WGT_LF~STRATUM_LF,data=panel,FUN=sum)
correction <- merge(frame_totals,sample_totals,by="STRATUM_LF")
correction$correction_factor <- correction$ones / correction$WGT_LF
# Here there is all.x=TRUE because some strata in panel are not present in correction
panel <- merge(panel,correction[,c("STRATUM_LF","correction_factor")],all.x=TRUE)
panel$correction_factor[is.na(panel$correction_factor)] <- 1
summary(panel$correction_factor)
panel$WGT_panel <- panel$WGT_LF * panel$correction_factor
sum(panel$WGT_panel)
#  433620.6
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
# [1] 453979.3
sum(frame_totals$ones)
# [1] 434155
sample_totals <- aggregate(WGT_LF~STRATUM_LF,data=nonpanel,FUN=sum)
correction <- merge(frame_totals,sample_totals,by="STRATUM_LF")
correction$correction_factor <- correction$ones / correction$WGT_LF
nonpanel <- merge(nonpanel,correction[,c("STRATUM_LF","correction_factor")])
nonpanel$WGT_nonpanel <- nonpanel$WGT_LF * nonpanel$correction_factor
sum(nonpanel$WGT_nonpanel)
#  433362
nonpanel$WGT_LF <- nonpanel$correction_factor <- NULL
colnames(nonpanel)[colnames(nonpanel)=="WGT_nonpanel"] <- "WGT_LF"

#--------------------------------------------------------
# Final calibration
LF <- rbind(panel,nonpanel)
sum(LF$WGT_LF)
# [1] 866982.6
sample_totals <- aggregate(WGT_LF~STRATUM_LF,data=LF,FUN=sum)
correction <- merge(frame_totals,sample_totals,by="STRATUM_LF")
correction$correction_factor <- correction$ones / correction$WGT_LF
LF <- merge(LF,correction[,c("STRATUM_LF","correction_factor")],all.x=TRUE)
LF$correction_factor[is.na(LF$correction_factor)] <- 1
LF$WGT_module_27 <- LF$WGT_LF * LF$correction_factor
sum(LF$WGT_module_27)
# [1] 433620.6
LF$WGT_LF <- LF$correction_factor <- NULL


write.table(LF,"LF2027_sample.csv",sep=",",quote=F,row.names=F)

# ---- Diagnostic barplots: sample distribution -----------------------
# NUTS0 (first 2 chars of NUTS2)
png("LF27_distribution_by_NUTS0.png", width = 1200, height = 800)
nuts0_counts <- table(substr(LF$NUTS2, 1, 2))
barplot(nuts0_counts,
        main = "LF 2027 sample distribution by NUTS0",
        xlab = "NUTS0",
        ylab = "Number of sampled points",
        las = 2)
dev.off()

# LC_pred distribution
png("LF27_distribution_by_LCpred.png", width = 1200, height = 800)
lcpred_counts <- table(LF$LC_pred)
barplot(lcpred_counts,
        main = "LF 2027 sample distribution by LC_pred",
        xlab = "LC_pred",
        ylab = "Number of sampled points",
        las = 2)
dev.off()


