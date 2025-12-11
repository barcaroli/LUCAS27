# ------------------------------------------------------------------------------
# Overview: Soil sample to be selected in 2027
# Main inputs:
#   - List of points to be selected: lucas2027_soil.xlsx 
#   - Master dataset
# Output:
#   - List of points to be selected enriched and in adjusted format: Soil2027_sample.csv 
# ------------------------------------------------------------------------------

setwd("D:/Google Drive/LUCAS 2026/dati")
load("master_complete.RData")
library(openxlsx)
soil <- read.csv("lucas_soil_sample20251203.csv")
soil$POINT_ID <- soil$IDPOINT
a <- soil[!soil$POINT_ID %in% master_tot$POINT_ID,]
write.table(a,"Soil_not_in_master.csv",sep=",",quote=F,row.names=F)

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

library(data.table)
s22 <- fread("Survey_2022_wgt_2nd_phase.txt")
soil <- merge(soil,s22[,c("POINT_ID","WGT_LUCAS"),],by="POINT_ID",all.x=TRUE)

a <- soil[soil$POINT_ID %in% s22$POINT_ID,]

# Build soil_sample with requested fields
soil_sample <- merge(
  soil[,c("POINT_ID","WGT_LUCAS")],
  master_tot[c("POINT_ID", "STR25", "NUTS2_24", "LC_pred")],
  by = "POINT_ID",
  all.x = TRUE
)
summary(soil_sample)

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
samp$STRATUM <- paste(soil_sample$NUTS2_24,soil_sample$STR25)
samp$POINT_ID <- soil_sample$POINT_ID
samp$module <- "SOIL"
samp$component <- ""
samp$NUTS2 <- soil_sample$NUTS2_24
samp$LC_pred <- soil_sample$LC_pred
samp$STR25 <- soil_sample$STR25
samp$WGT_LUCAS <- soil_sample$WGT_LUCAS 
samp$eligibility_comp <- ""
samp$WGT_module_22 <- 1
samp$wgt_correction_22 <- 1
samp$WGT_comp_27 <- 1
samp$WGT_module_27 <- soil_sample$WGT_SOIL_LC
samp <- as.data.frame(samp)

write.table(samp,"Soil2027_sample.csv",sep=",",quote=F,row.names=F)
