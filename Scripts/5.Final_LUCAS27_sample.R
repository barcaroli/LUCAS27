# Script to merge LUCAS 2027 thematic samples, keep only points in master_tot, and export a unified sample.
# Inputs: master_complete.RData (provides master_tot), 
#         Soil2027_sample.csv, 
#         Grassland2027_sample.csv, 
#         LF2027_sample.csv, 
#         Copernicus2027_sample.csv
# Output: LUCAS27_sample.csv with module presence flags and weights per module
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
# b <- Grass[!Grass$POINT_ID %in% master_tot$POINT_ID,] 
LF <- read.csv("LF2027_sample.csv")
# c <- LF[!LF$POINT_ID %in% master_tot$POINT_ID,] 
LF <- LF[LF$POINT_ID %in% master_tot$POINT_ID,]
Copernicus <- read.csv("Copernicus2027_sample.csv")

tot <- NULL
tot <- merge(Soil[,c("POINT_ID","module","WGT_module_27")],
             Grass[,c("POINT_ID","module","WGT_module_27")],by="POINT_ID",
             all.x=T,all.y=T)
colnames(tot) <- c("POINT_ID","SOIL","WGT_SOIL","GRASSLAND","WGT_GRASSLAND")
table(tot$SOIL)
table(tot$GRASSLAND)
tot <- merge(tot,
             LF[,c("POINT_ID","module","WGT_module_27")],by="POINT_ID",
             all.x=T,all.y=T)
colnames(tot)[6:7] <- c("LF","WGT_LF")
table(tot$LF)
tot <- merge(tot,
             Copernicus[,c("POINT_ID","module","WGT_module_27")],by="POINT_ID",
             all.x=T,all.y=T)
colnames(tot)[8:9] <- c("COPERNICUS","WGT_COPERNICUS")

# Overlap combinations and pairwise matrix
if (!exists("modules")) {
  modules <- c("SOIL","GRASSLAND","LF","COPERNICUS")
}
if (!exists("presence_dt")) {
  presence_dt <- as.data.table(tot)[, c("POINT_ID", modules), with = FALSE]
  for (m in modules) presence_dt[[m]] <- !is.na(presence_dt[[m]])
}
tot_long <- melt(presence_dt, id.vars = "POINT_ID", variable.name = "module", value.name = "present")
tot_long <- tot_long[present == TRUE, .(POINT_ID, module)]

mods_by_id <- tot_long[, .(modules = list(sort(module))), by = POINT_ID]
mods_by_id[, combo := sapply(modules, function(x) if (length(x) == 0) "None" else paste(x, collapse = "+"))]
overlap_counts <- mods_by_id[, .N, by = combo][order(-N)]
print(overlap_counts)

overlap_matrix <- matrix(0, nrow = length(modules), ncol = length(modules),
                         dimnames = list(modules, modules))
for (m1 in modules) {
  ids1 <- unique(tot_long[module == m1, POINT_ID])
  for (m2 in modules) {
    ids2 <- unique(tot_long[module == m2, POINT_ID])
    overlap_matrix[m1, m2] <- length(intersect(ids1, ids2))
  }
}
print(overlap_matrix)

# Recode module presence to 1/0 flags
for (m in modules) {
  tot[[m]] <- ifelse(!is.na(tot[[m]]), 1, 0)
}

tot$WGT_SOIL <- ifelse(!is.na(tot$WGT_SOIL),tot$WGT_SOIL,0)
tot$WGT_GRASSLAND <- ifelse(!is.na(tot$WGT_GRASSLAND),tot$WGT_GRASSLAND,0)
tot$WGT_LF <- ifelse(!is.na(tot$WGT_LF),tot$WGT_LF,0)
tot$WGT_COPERNICUS <- ifelse(!is.na(tot$WGT_COPERNICUS),tot$WGT_COPERNICUS,0)

table(tot$SOIL)
table(tot$GRASSLAND)
table(tot$LF)
table(tot$COPERNICUS)

sum(tot$WGT_SOIL)
sum(tot$WGT_GRASSLAND)
sum(tot$WGT_LF)
sum(tot$WGT_COPERNICUS)

tot <- merge(tot,
             master_tot[,c("POINT_ID","NUTS2_24","STR25","LC_pred","reach_prob","X_WGS84","Y_WGS84")],
             by="POINT_ID")
colnames(tot)[colnames(tot)=="NUTS2_24"] <- "NUTS2"
tot$STRATUM <- paste(tot$NUTS2,tot$STR25,sep="*")

write.table(tot,"LUCAS27_sample.csv",sep=",",quote=FALSE,row.names=F)

tot2 <- as.data.table(tot[!duplicated(tot$POINT_ID),])

nrow(tot2)
round(prop.table(table(tot2$LC_pred)),5)
xtabs(~substr(NUTS2,1,2),data=tot2,addNA=TRUE)

# ---- Master reference and compute proportions ----
round(prop.table(table(master_tot$LC_pred)),5)

# ---- Visual comparison of LC distributions ----
# Barplot: distributions of tot$LC1 vs master_tot$LC_pred (percentages)

lc_sample_col <- if ("LC1" %in% names(tot2)) "LC1" else "LC_pred"
lc_sample <- tot2[[lc_sample_col]]
cats <- sort(unique(c(levels(factor(lc_sample)), levels(factor(master_tot$LC_pred)))))

perc_sample <- as.numeric(100 * prop.table(table(factor(lc_sample,levels = cats))))
perc_master <- as.numeric(100 * prop.table(table(factor(master_tot$LC_pred, levels = cats))))

lc_labels <- c(sprintf("Sample (%s)", lc_sample_col), "Master (LC_pred)")
M <- rbind(`Sample` = perc_sample, `Master` = perc_master)
lc_title <- sprintf("LandCover distribution: sample (%s) vs master (LC_pred)", lc_sample_col)
lc_colors <- c("steelblue", "firebrick")
op <- par(no.readonly = TRUE)
par(mar = c(12, 5, 4, 1))
bp <- barplot(M,
              beside = TRUE,
              col = lc_colors,
              border = NA,
              xaxt = "n",
              ylim = c(0, max(M) * 1.2),
              ylab = "Percentage",
              main = lc_title)
axis(1, at = colMeans(bp), labels = cats, las = 2, tick = FALSE, cex.axis = 0.8)
legend("topright", legend = lc_labels, fill = lc_colors, border = NA, bty = "n")
png("5.1.LC_distribution_tot_vs_master.png", width = 1600, height = 900, res = 150)
par(mar = c(15, 5, 4, 1))
bp <- barplot(M,
              beside = TRUE,
              col = lc_colors,
              border = NA,
              xaxt = "n",
              ylim = c(0, max(M) * 1.2),
              ylab = "Percentage",
              main = lc_title)
axis(1, at = colMeans(bp), labels = cats, las = 2, tick = FALSE, cex.axis = 0.8)
legend("topright", legend = lc_labels, fill = lc_colors, border = NA, bty = "n")
dev.off()
par(op)

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

M_nuts0 <- rbind(`Sample` = perc_tot_nuts0, `Master` = perc_master_nuts0)
op <- par(no.readonly = TRUE)
par(mar = c(7, 4, 3, 1))
bp <- barplot(M_nuts0,
              beside = TRUE,
              col = c("darkgreen", "orange"),
              border = NA,
              xaxt = "n",
              ylim = c(0, max(M_nuts0) * 1.2),
              ylab = "Percentage",
              main = "NUTS0 distribution: sample vs master")
axis(1, at = colMeans(bp), labels = nuts0_levels, las = 2, tick = FALSE, cex.axis = 0.8)
legend("topright", legend = c("Sample", "Master"), fill = c("darkgreen", "orange"), border = NA, bty = "n")
png("5.2.NUTS0_distribution_tot_vs_master.png", width = 1600, height = 900, res = 150)
par(mar = c(10, 4, 3, 1))
bp <- barplot(M_nuts0,
              beside = TRUE,
              col = c("darkgreen", "orange"),
              border = NA,
              xaxt = "n",
              ylim = c(0, max(M_nuts0) * 1.2),
              ylab = "Percentage",
              main = "NUTS0 distribution: sample vs master")
axis(1, at = colMeans(bp), labels = nuts0_levels, las = 2, tick = FALSE, cex.axis = 0.8)
legend("topright", legend = c("Sample", "Master"), fill = c("darkgreen", "orange"), border = NA, bty = "n")
dev.off()
par(op)
tot2$NUTS0 <- substr(tot2$NUTS2,1,2)
tot2_long <- merge(tot_long, tot2[, .(POINT_ID, NUTS0)], by = "POINT_ID", all.x = TRUE)
addmargins(xtabs(~module+NUTS0,data=tot2_long))

# ---- Mappa interattiva  ----
library(sp)
coordinates(tot2) <- ~X_WGS84+Y_WGS84
proj4string(tot2) <- CRS("+init=epsg:4326")
# tot$module <- as.factor(tot$module)
tot$LC_pred <- as.factor(tot2$LC_pred)
# OpenStreetMap
# Esri.WorldImagery
# Esri.WorldStreetMap
# CartoDB.Positron
# Stamen.TonerLite
country <- "IT"
SOIL <- tot2[tot2$SOIL ==1 & substr(tot$NUTS2,1,2) == country,]
mapview(SOIL,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
GRASSLAND <- tot2[tot2$GRASSLAND == 1 & substr(tot$NUTS2,1,2) == country,]
mapview(GRASSLAND,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
LF <- tot2[tot2$LF == 1 & substr(tot$NUTS2,1,2) == country,]
mapview(LF,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=2)
Cop <- tot2[tot2$COPERNICUS == 1 & substr(tot$NUTS2,1,2) == country,]
mapview(Cop,
        zcol="LC_pred", 
        map.types = c("Esri.WorldStreetMap"),
        cex=3)
