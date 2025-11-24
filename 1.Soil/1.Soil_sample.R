setwd("D:/Google Drive/LUCAS 2026/dati")
library(openxlsx)
soil <- read.xlsx("lucas2027_soil.xlsx")
soil$POINT_ID <- soil$IDPOINT
load("master_complete.RData")

# Build soil_sample with requested fields
soil_sample <- merge(
  soil["POINT_ID"],
  master_tot[c("POINT_ID", "STR25", "NUTS2_24", "LC_pred")],
  by = "POINT_ID",
  all.x = TRUE
)

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



samp <- NULL
samp$POINT_ID <- soil_sample$POINT_ID
samp$component <- ""
samp$STRATUM <- soil_sample$STRATUM_LC
samp$WGT_LUCAS <- 1 
samp$WGT_comp <- 1
samp$eligibility_comp <- ""
samp$LC1 <- soil_sample$LC_pred
samp$wgt_correction <- 1
samp$wgt_selection <- soil_sample$WGT_SOIL_LC
samp <- as.data.frame(samp)

write.table(samp,"Soil2027_sample.csv",sep=",",quote=F,row.names=F)
