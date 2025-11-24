# ------------------------------------------------------------------------------
# Overview: final consolidation of components 1 (Grassland) and 2 (Extended
# Grassland) to assemble the full LUCAS 2027 sample.
# Main inputs:
#   - Grassland2027_component1.csv (output of component-1 script)
#   - Grassland2027_component2.csv (output of component-2 script)
# Output:
#   - Grassland2027_sample.csv (combined sample with unified metadata)
# ------------------------------------------------------------------------------
#---------------------------
# 2027 Grassland sub-sample 
#---------------------------
# --- Set working directory ---
setwd("D:/Google Drive/LUCAS 2026/dati")

# --- Load and harmonize component 1 ---
comp1 <- read.csv("Grassland2027_component1.csv")
comp1b <- NULL
comp1b$POINT_ID <- comp1$POINT_ID
comp1b$component <- "component1"
comp1b$STRATUM <- comp1$STRATUM_GRASSLAND
comp1b$WGT_LUCAS <- comp1$WGT_LUCAS 
comp1b$WGT_comp <- comp1$WGT_GRASSLAND
comp1b$eligibility_comp <- comp1$eligibility_rate_grassland
comp1b$LC1 <- comp1$LC1
comp1b$wgt_correction <- comp1$wgt_correction
comp1b$wgt_selection <- 1
comp1b <- as.data.frame(comp1b)

# --- Load and harmonize component 2 ---
comp2 <- read.csv("Grassland2027_component2.csv")
comp2b <- NULL
comp2b$POINT_ID <- comp2$ID
comp2b$component <- "component2"
comp2b$STRATUM <- comp2$STRATUM_LUCAS
comp2b$WGT_LUCAS <- comp2$WGT_LUCAS 
comp2b$WGT_comp <- comp2$WGT_EXT_GRASSLAND
comp2b$eligibility_comp <- comp2$eligibility_rate_ext_grassland
comp2b$LC1 <- comp2$LC1
comp2b$wgt_correction <- comp2$wgt_correction
comp2b$wgt_selection <- comp2$wgt_selection
comp2b <- as.data.frame(comp2b)

# --- Combine components and export final sample ---
samp <- rbind(comp1b,comp2b)
samp <- samp[!duplicated(samp$POINT_ID),]
write.table(samp,"Grassland2027_sample.csv",sep=",",quote=F,row.names=F)



