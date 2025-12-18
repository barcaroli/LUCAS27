# ------------------------------------------------------------------------------
# Overview
# Inputs:
#   - master_complete.RData (master_tot)
#   - lucas_soil_sample20251203.csv (raw soil sample)
#   - Soil_to_be_removed.xlsx (exclusion lists)
#   - Survey_2022_wgt_2nd_phase.txt (survey weights)
# Outputs:
#   - Soil_not_in_master.csv (soil points missing in master)
#   - lucas_soil_sample_2027_clean.csv (cleaned soil sample)
#   - soil_vs_master_by_NUTS0.png, soil_vs_master_by_LC_pred.png (comparative barplots)
#   - Soil2027_sample.csv (final export for 2027 soil sample)
# Purpose:
#   - Clean and enrich the 2027 soil sample against the master dataset
#   - Compute stratum-level weights for STR25 and LC_pred
#   - Produce comparison barplots (soil vs master) by NUTS0 and LC_pred
#   - Export the final soil sample to CSV
# ------------------------------------------------------------------------------

setwd("D:/Google Drive/LUCAS 2026/dati")
load("master_complete.RData")
library(openxlsx)
soil <- read.csv("lucas_soil_sample20251203.csv")
soil$POINT_ID <- soil$IDPOINT

# Identify soil points missing in master
a <- soil[!soil$POINT_ID %in% master_tot$POINT_ID,]
write.table(a,"Soil_not_in_master.csv",sep=",",quote=F,row.names=F)

# Remove flagged points from several exclusion lists
toberemoved <- read.xlsx("Soil_to_be_removed.xlsx")
soil <- soil[!soil$IDPOINT %in% toberemoved$`POINT_ID.(XX01)` & 
             !soil$IDPOINT %in% toberemoved$`POINT_ID.(AB03)` &
             !soil$IDPOINT %in% toberemoved$`POINT_ID.(SA07)` &
             !soil$IDPOINT %in% toberemoved$`POINT_ID.(FA04)` &
             !soil$IDPOINT %in% toberemoved$`POINT_ID.(AB02)` &
             !soil$IDPOINT %in% toberemoved$`POINT_ID.(AB10)`,  ]
addmargins(xtabs( ~ bio2027_flag, data=soil))
write.table(soil,"lucas_soil_sample_2027_clean.csv")

load("master_complete.RData")

# Attach survey weights from 2022
library(data.table)
s22 <- fread("Survey_2022_wgt_2nd_phase.txt")
soil <- merge(soil,s22[,c("POINT_ID","WGT_LUCAS"),],by="POINT_ID",all.x=TRUE)

a <- soil[soil$POINT_ID %in% s22$POINT_ID,]

# Build working soil_sample with requested fields
soil_sample <- merge(
  soil[,c("POINT_ID","WGT_LUCAS")],
  master_tot[c("POINT_ID", "STR25", "NUTS2_24", "LC_pred")],
  by = "POINT_ID",
  all.x = TRUE
)
summary(soil_sample)

# Define stratums for STR25 and LC_pred
soil_sample$STRATUM_STR25 <- interaction(
  soil_sample$NUTS2_24,
  soil_sample$STR25,
  sep = "_",
  drop = TRUE
)
soil_sample$STRATUM_LC <- interaction(
  soil_sample$NUTS2_24,
  soil_sample$LC_pred,
  sep = "_",
  drop = TRUE
)

# Counts in master and soil
master_STR25 <- table(interaction(
  master_tot$NUTS2_24,
  master_tot$STR25,
  sep = "_",
  drop = TRUE
))
master_LC <- table(interaction(
  master_tot$NUTS2_24,
  master_tot$LC_pred,
  sep = "_",
  drop = TRUE
))

soil_STR25 <- table(soil_sample$STRATUM_STR25)
soil_LC <- table(soil_sample$STRATUM_LC)

# Map counts back and compute weights
soil_sample$WGT_SOIL_STR25 <- as.numeric(master_STR25[soil_sample$STRATUM_STR25]) /
  as.numeric(soil_STR25[soil_sample$STRATUM_STR25])
soil_sample$WGT_SOIL_LC <- as.numeric(master_LC[soil_sample$STRATUM_LC]) /
  as.numeric(soil_LC[soil_sample$STRATUM_LC])

# Barplots: soil vs master distributions by NUTS0 and LC_pred (shown and saved as PNG)
soil_sample$NUTS0 <- substr(soil_sample$NUTS2_24, 1, 2)
master_NUTS0 <- table(substr(master_tot$NUTS2_24, 1, 2))
soil_NUTS0 <- table(soil_sample$NUTS0)
nuts0_levels <- union(names(master_NUTS0), names(soil_NUTS0))
nuts0_mat <- rbind(
  Soil = {
    v <- as.numeric(soil_NUTS0[nuts0_levels])
    v[is.na(v)] <- 0
    v / sum(v) * 100
  },
  Master = {
    v <- as.numeric(master_NUTS0[nuts0_levels])
    v[is.na(v)] <- 0
    v / sum(v) * 100
  }
)

barplot(
  nuts0_mat,
  beside = TRUE,
  main = "Soil vs master by NUTS0",
  xlab = "NUTS0",
  ylab = "Percentage of points",
  col = c("steelblue", "orange"),
  names.arg = nuts0_levels,
  las = 2,
  legend.text = TRUE,
  args.legend = list("topright", bty = "n")
)
png("1.1.Soil_vs_master_by_NUTS0.png", width = 1200, height = 800, res = 150)
barplot(
  nuts0_mat,
  beside = TRUE,
  main = "Soil vs master by NUTS0",
  xlab = "NUTS0",
  ylab = "Percentage of points",
  col = c("steelblue", "orange"),
  names.arg = nuts0_levels,
  las = 2,
  legend.text = TRUE,
  args.legend = list("topright", bty = "n")
)
dev.off()

master_LC_simple <- table(master_tot$LC_pred)
soil_LC_simple <- table(soil_sample$LC_pred)
lc_levels <- union(names(master_LC_simple), names(soil_LC_simple))
lc_mat <- rbind(
  Soil = {
    v <- as.numeric(soil_LC_simple[lc_levels])
    v[is.na(v)] <- 0
    v / sum(v) * 100
  },
  Master = {
    v <- as.numeric(master_LC_simple[lc_levels])
    v[is.na(v)] <- 0
    v / sum(v) * 100
  }
)

barplot(
  lc_mat,
  beside = TRUE,
  main = "Soil vs master by LC_pred",
  xlab = "LC_pred class",
  ylab = "Percentage of points",
  col = c("tan", "gray70"),
  names.arg = lc_levels,
  legend.text = TRUE,
  args.legend = list("topright", bty = "n")
)
png("1.2.Soil_vs_master_by_LC_pred.png", width = 1200, height = 800, res = 150)
barplot(
  lc_mat,
  beside = TRUE,
  main = "Soil vs master by LC_pred",
  xlab = "LC_pred class",
  ylab = "Percentage of points",
  col = c("tan", "gray70"),
  names.arg = lc_levels,
  legend.text = TRUE,
  args.legend = list("topright", bty = "n")
)
dev.off()

# Assemble final export table
samp <- NULL
samp$STRATUM <- paste(soil_sample$NUTS2_24,soil_sample$STR25,sep="*")
samp$POINT_ID <- soil_sample$POINT_ID
samp$module <- "SOIL"
samp$component <- ""
samp$NUTS2 <- soil_sample$NUTS2_24
samp$LC_pred <- soil_sample$LC_pred
samp$STR25 <- soil_sample$STR25
samp$WGT_LUCAS <- soil_sample$WGT_LUCAS 
samp$eligibility_comp <- ""
samp$wgt_module_22 <- 1
samp$wgt_correction_22 <- 1
samp$WGT_comp_27 <- 1
samp$WGT_module_27 <- soil_sample$WGT_SOIL_LC
samp <- as.data.frame(samp)

write.table(samp,"Soil2027_sample.csv",sep=",",quote=F,row.names=F)
