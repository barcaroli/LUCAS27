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
load("master_complete.RData")

Soil <- read.csv("Soil2027_sample.csv")
a <- Soil[!Soil$POINT_ID %in% master_tot$POINT_ID,]
Soil <- Soil[Soil$POINT_ID %in% master_tot$POINT_ID,]
Grass <- read.csv("Grassland2027_sample.csv")
b <- Grass[!Grass$POINT_ID %in% master_tot$POINT_ID,] 
LF <- read.csv("LF2027_sample.csv")
c <- LF[!LF$POINT_ID %in% master_tot$POINT_ID,] 
LF <- LF[LF$POINT_ID %in% master_tot$POINT_ID,]
Copernicus_add <- read.csv("Copernicus2027_sample_add.csv")


tot <- rbind(Soil,Grass,LF,Copernicus_add)
Copernicus <- tot[!duplicated(tot$POINT_ID),]
write.table(Copernicus,"Copernicus2027_sample.csv",sep=",",quote=F,row.names=F)

write.table(tot,"LUCAS2027_total_sample.csv",sep=",",quote=F,row.names=F)
xtabs(~module,data=tot)
# Copernicus  GRASSLAND         LF       SOIL 
#       5160      20000      93001      25000 

dt <- as.data.table(tot)



mods_by_id <- dt[, .(modules = list(unique(module))), by = POINT_ID]
mods_by_id[, combo := sapply(modules, function(x) paste(sort(x), collapse = "+"))]
overlap_counts <- mods_by_id[, .N, by = combo][order(-N)]
overlap_counts
# 1:                LF 80565
# 2:              SOIL 21850
# 3:         GRASSLAND  9644
# 4:      GRASSLAND+LF  9644
# 5:        Copernicus  5160
# 6:           LF+SOIL  2438
# 7:    GRASSLAND+SOIL   358
# 8: GRASSLAND+LF+SOIL   354
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
# SOIL       25000       712  2792          0
# GRASSLAND    712     20000  9998          0
# LF          2792      9998 93001          0
# Copernicus     0         0     0       5160

tot2 <- tot[!duplicated(tot$POINT_ID),]

nrow(tot2)
round(prop.table(table(tot2$LC_pred)),5)
xtabs(~substr(NUTS2,1,2),data=tot2,addNA=TRUE)

# ---- Master reference and compute proportions ----
round(prop.table(table(master_tot$LC_pred)),5)

# ---- Visual comparison of LC distributions ----
# Barplot: distributions of tot$LC1 vs master_tot$LC_pred (percentages)

render_barplot <- function(mat, cats, title, legend_labels, colors, filename = NULL, mar = c(10, 4, 3, 1)) {
  draw_plot <- function() {
    bp <- barplot(mat,
                  beside = TRUE,
                  col = colors,
                  border = NA,
                  xaxt = "n",
                  ylim = c(0, max(mat) * 1.2),
                  ylab = "Percentage",
                  main = title)
    xg <- colMeans(bp)
    axis(1, at = xg, labels = cats, las = 2, tick = FALSE, cex.axis = 0.8)
    legend("topright", legend = legend_labels, fill = colors, border = NA, bty = "n")
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = mar)
  draw_plot()
  
  if (!is.null(filename)) {
    png(filename, width = 1600, height = 900, res = 150)
    par(mar = mar + c(3, 0, 0, 0)) # slightly wider bottom margin for saved plot
    draw_plot()
    dev.off()
  }
}

cats <- sort(unique(c(levels(factor(tot2$LC_pred)), levels(factor(master_tot$LC_pred)))))

perc_tot    <- as.numeric(100 * prop.table(table(factor(tot2$LC_pred,        levels = cats))))
perc_master <- as.numeric(100 * prop.table(table(factor(master_tot$LC_pred, levels = cats))))

 M <- rbind(`Sample (LC1)` = perc_tot, `Master (LC_pred)` = perc_master)
render_barplot(M, cats,
               title = "LC1 (sample) vs LC_pred (master): percentages",
               legend_labels = c("Sample (LC1)", "Master (LC_pred)"),
               colors = c("blue", "red"),
               filename = "LC_distribution_tot_vs_master.png")

# ---- Visual comparison of NUTS0 distributions ----
# NUTS0 derived as the first two characters of NUTS2
master_nuts2 <- if ("NUTS2" %in% names(master_tot)) {
  master_tot$NUTS2
} else if ("NUTS2_24" %in% names(master_tot)) {
  master_tot$NUTS2_24
} else {
  stop("master_tot missing NUTS2/NUTS2_24 column")
}
tot2_nuts2 <- if ("NUTS2" %in% names(tot2)) {
  tot2$NUTS2
} else if ("NUTS2_24" %in% names(tot2)) {
  tot2$NUTS2_24
} else {
  stop("tot2 missing NUTS2/NUTS2_24 column")
}

master_nuts0 <- substr(master_nuts2, 1, 2)
tot2_nuts0 <- substr(tot2_nuts2, 1, 2)

nuts0_levels <- sort(unique(c(levels(factor(master_nuts0)), levels(factor(tot2_nuts0)))))
perc_tot_nuts0 <- as.numeric(100 * prop.table(table(factor(tot2_nuts0,    levels = nuts0_levels))))
perc_master_nuts0 <- as.numeric(100 * prop.table(table(factor(master_nuts0, levels = nuts0_levels))))

M_nuts0 <- rbind(`Sample (NUTS0)` = perc_tot_nuts0, `Master (NUTS0)` = perc_master_nuts0)
render_barplot(M_nuts0, nuts0_levels,
               title = "NUTS0 (sample) vs NUTS0 (master): percentages",
               legend_labels = c("Sample (NUTS0)", "Master (NUTS0)"),
               colors = c("darkgreen", "orange"),
               filename = "NUTS0_distribution_tot_vs_master.png",
               mar = c(7, 4, 3, 1))

tot2$NUTS0 <- substr(tot2$NUTS2,1,2)
addmargins(xtabs(~module+NUTS0,data=tot2))
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
