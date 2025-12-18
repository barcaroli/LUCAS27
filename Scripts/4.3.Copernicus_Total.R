# ================================================================
# General purpose
# - Consolidate thematic samples (Soil, Grassland, LF, Copernicus) into a single dataset
# - Deduplicate by POINT_ID and write the final sample and total files
# - Compare LC and NUTS0 distributions of the sample against master_tot
#
# Input datasets
# - master_complete.RData (object master_tot with LC_pred and coordinates)
# - Soil2027_sample.csv, Grassland2027_sample.csv, LF2027_sample.csv,
#   Copernicus2027_sample_add.csv
#
# Outputs
# - Copernicus2027_sample.csv, LUCAS2027_total_modules.csv
# - LC_distribution_tot_vs_master.png, NUTS0_distribution_tot_vs_master.png
# - Control tabulations printed to console and interactive maps via mapview
#
# Main steps
# - Harmonize weights for Grassland, bind samples, and deduplicate on POINT_ID
# - Compute module overlaps and LC/NUTS0 percentage distributions
# - Barplots (sample vs master) 
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
# b <- Grass[!Grass$POINT_ID %in% master_tot$POINT_ID,] 
LF <- read.csv("LF2027_sample.csv")
# c <- LF[!LF$POINT_ID %in% master_tot$POINT_ID,] 
LF <- LF[LF$POINT_ID %in% master_tot$POINT_ID,]
Copernicus_add <- read.csv("Copernicus2027_sample_add.csv")

tot <- rbind(Soil,Grass,LF,Copernicus_add)
Copernicus <- tot[!duplicated(tot$POINT_ID),]
Copernicus$module <- "Copernicus"
table(Copernicus$module)

write.table(Copernicus,"Copernicus2027_sample.csv",sep=",",quote=F,row.names=F)

dt <- as.data.table(tot)

names(Grass)[!names(Grass) %in% names(Soil)]

mods_by_id <- dt[, .(modules = list(unique(module))), by = POINT_ID]
mods_by_id[, combo := sapply(modules, function(x) paste(sort(x), collapse = "+"))]
overlap_counts <- mods_by_id[, .N, by = combo][order(-N)]
overlap_counts

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


tot2 <- tot[!duplicated(tot$POINT_ID),]

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
M <- rbind(`Campione` = perc_sample, `Master` = perc_master)
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
png("4.3.1.LC_distribution_tot_vs_master_by_LCpred.png", width = 1600, height = 900, res = 150)
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

M_nuts0 <- rbind(`Campione` = perc_tot_nuts0, `Master` = perc_master_nuts0)
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
png("4.3.2.NUTS0_distribution_tot_vs_master.png", width = 1600, height = 900, res = 150)
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
addmargins(xtabs(~module+NUTS0,data=tot2))
