#---------------------------
# 2027 LF sub-sample 
#---------------------------
# Description: Combines the component 1 (panel) and component 2
# (non-panel) selections into a unified LF 2027 sample with the
# required weights and identifiers.
#---------------------------
# Input datasets:
# - LF2027_panel.csv (component 1 sample)
# - LF2027_nonpanel.csv (component 2 sample)
#---------------------------
# Output datasets:
# - LF2027_sample.csv (merged LF 2027 sample)
#---------------------------

# ---- Setup environment ------------------------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")

# ---- Prepare component 1 records -------------------------------------
comp1 <- read.csv("LF2027_panel.csv")
comp1b <- NULL
comp1b$POINT_ID <- comp1$ID
comp1b$component <- "component1"
comp1b$STRATUM <- comp1$STRATUM_LF
comp1b$WGT_LUCAS <- comp1$WGT_LUCAS 
comp1b$WGT_comp <- comp1$WGT_LF
comp1b$eligibility_comp <- comp1$eligibility_rate_LF
comp1b$LC1 <- comp1$LCobs
comp1b$wgt_correction <- comp1$wgt_correction
comp1b$wgt_selection <- comp1$wgt_selection_component1
comp1b <- as.data.frame(comp1b)


# ---- Prepare component 2 records -------------------------------------
comp2 <- read.csv("LF2027_nonpanel.csv")
comp2b <- NULL
comp2b$POINT_ID <- comp2$POINT_ID
comp2b$component <- "component2"
comp2b$STRATUM <- comp2$STRATUM_LF
comp2b$WGT_LUCAS <- 1 
comp2b$WGT_comp <- 1
comp2b$eligibility_comp <- 1
comp2b$LC1 <- NA
comp2b$wgt_correction <- 1
comp2b$wgt_selection <- comp2$wgt_selection_stratum
comp2b <- as.data.frame(comp2b)

# ---- Merge components and export sample ------------------------------
samp <- rbind(comp1b,comp2b)
table(samp$STRATUM)
write.table(samp,"LF2027_sample.csv",sep=",",quote=F,row.names=F)



