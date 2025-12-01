# ================================================================
# Script: 3.check.R
# Description: consolidates the thematic samples (Soil, Grassland, LF,
#              Copernicus) and compares their LC1 mix with master_tot
#              LC_pred through quick tabulations and a barplot.
# Input: Soil2027_sample.csv, Grassland2027_sample.csv,
#        LF2027_sample.csv, Copernicus2027_sample.csv,
#        master_complete.RData
# Output: LC distribution tabulations printed to console and barplot
#         visualisation on screen (no files written)
# ================================================================
library(data.table)
library(sf)
library(mapview)
library(leaflet)
library(RColorBrewer)
# ---- Load thematic samples and merge unique points ----
setwd("D:/Google Drive/LUCAS 2026/dati")

Soil <- read.csv("Soil2027_sample.csv")
Grass <- read.csv("Grassland2027_sample.csv")
LF <- read.csv("LF2027_sample.csv")
LF <- LF[!duplicated(LF$POINT_ID),]
Copernicus <- read.csv("Copernicus2027_sample.csv")

tot <- rbind(Soil,Grass,LF,Copernicus)
xtabs(~module,data=tot)

dt <- as.data.table(tot)
mods_by_id <- dt[, .(modules = list(unique(module))), by = POINT_ID]
mods_by_id[, combo := sapply(modules, function(x) paste(sort(x), collapse = "+"))]
overlap_counts <- mods_by_id[, .N, by = combo][order(-N)]
overlap_counts
# 1:                LF 80603
# 2:              SOIL 21681
# 3:         GRASSLAND  9653
# 4:      GRASSLAND+LF  9645
# 5:        Copernicus  5317
# 6:           LF+SOIL  2399
# 7: GRASSLAND+LF+SOIL   354
# 8:    GRASSLAND+SOIL   348
modules <- unique(dt$module)
overlap_matrix <- matrix(0, nrow=4, ncol=4,
                         dimnames=list(modules, modules))
for (m1 in modules) {
  for (m2 in modules) {
    ids1 <- unique(dt[module==m1, POINT_ID])
    ids2 <- unique(dt[module==m2, POINT_ID])
    overlap_matrix[m1, m2] <- length(intersect(ids1, ids2))
  }
}
overlap_matrix
#             SOIL GRASSLAND    LF Copernicus
# SOIL       24782       702  2753          0
# GRASSLAND    702     20000  9999          0
# LF          2753      9999 93001          0
# Copernicus     0         0     0       5317

tot2 <- tot[!duplicated(tot$POINT_ID),]
nrow(tot2)
round(prop.table(table(tot2$LC_pred)),5)
xtabs(~substr(NUTS2,1,2),data=tot2,addNA=TRUE)

# ---- Load master reference and compute proportions ----
load("master_complete.RData")
round(prop.table(table(master_tot$LC_pred)),5)

# ---- Visual comparison of LC distributions ----
# Barplot: distributions of tot$LC1 vs master_tot$LC_pred (percentages)

cats <- sort(unique(c(levels(factor(tot2$LC_pred)), levels(factor(master_tot$LC_pred)))))

perc_tot    <- as.numeric(100 * prop.table(table(factor(tot2$LC_pred,        levels = cats))))
perc_master <- as.numeric(100 * prop.table(table(factor(master_tot$LC_pred, levels = cats))))

M <- rbind(`Tot (LC1)` = perc_tot, `Master (LC_pred)` = perc_master)

op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)
par(mar = c(7, 4, 3, 1))

bp <- barplot(M,
              beside = TRUE,
              col = c("blue", "red"),
              border = NA,
              xaxt = "n",
              ylim = c(0, max(M) * 1.2),
              ylab = "Percentage",
              main = "LC1 (tot) vs LC_pred (master): percentages")

xg <- colMeans(bp)
axis(1, at = xg, labels = cats, las = 1, tick = FALSE)
legend("topright",
       legend = c("Tot (LC1)", "Master (LC_pred)"),
       fill   = c("blue", "red"),
       border = NA,
       bty    = "n")

# ---- Mappa interattiva  ----
table(tot$module)
tot_with_coords <- merge(tot,master_tot[,c("POINT_ID","X_WGS84","Y_WGS84")],by="POINT_ID")
library(sp)
coordinates(tot_with_coords) <- ~X_WGS84+Y_WGS84
proj4string(tot_with_coords) <- CRS("+init=epsg:4326")
tot_with_coords$module <- as.factor(tot_with_coords$module)
tot_with_coords$LC_pred <- as.factor(tot_with_coords$LC_pred)
# OpenStreetMap
# Esri.WorldImagery
# Esri.WorldStreetMap
# CartoDB.Positron
# Stamen.TonerLite
SOIL <- tot_with_coords[tot_with_coords$module=="SOIL",]
mapview(SOIL,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
GRASSLAND <- tot_with_coords[tot_with_coords$module=="GRASSLAND",]
mapview(GRASSLAND,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
LF <- tot_with_coords[tot_with_coords$module=="LF",]
mapview(LF,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=2)
Copernicus <- tot_with_coords[tot_with_coords$module=="Copernicus",]
mapview(Copernicus,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
