# ================================================================
# General purpose
# - Compute Copernicus weights
#
# Input datasets
# - master_complete.RData (object master_tot with LC_pred and coordinates)
# - Copernicus2027_sample.csv
#
# Outputs
# - Copernicus2027_sample.csv
#
# ================================================================

# ---- Load  ----
setwd("D:/Google Drive/LUCAS 2026/dati")
load("master_complete.RData")
master_tot$STRATUM <- paste(master_tot$NUTS2_24,master_tot$STR25,sep="*")
Copernicus <- read.csv("Copernicus2027_sample.csv")
Copernicus$ones <- 1
master_tot$ones <- 1

master_totals <- aggregate(ones~STRATUM,data=master_tot,FUN=sum)

sample_totals <- aggregate(ones~STRATUM,data=Copernicus,FUN=sum)

wgts <- merge(master_totals,sample_totals,by="STRATUM")
wgts$weight <- wgts$ones.x / wgts$ones.y

Copernicus <- merge(Copernicus,wgts[,c("STRATUM","weight")],by="STRATUM")
Copernicus$WGT_module_27 <- Copernicus$weight
sum(Copernicus$WGT_module_27)
Copernicus$ones <- Copernicus$weight <- NULL


write.table(Copernicus,"Copernicus2027_sample.csv",sep=",",quote=F,row.names=F)

