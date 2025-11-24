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
# ---- Load thematic samples and merge unique points ----
setwd("D:/Google Drive/LUCAS 2026/dati")

Soil <- read.csv("Soil2027_sample.csv")
Grass <- read.csv("Grassland2027_sample.csv")
LF <- read.csv("LF2027_sample.csv")
Copernicus <- read.csv("Copernicus2027_sample.csv")

tot <- rbind(Soil,Grass,LF,Copernicus)

tot <- tot[!duplicated(tot$POINT_ID),]
nrow(tot)
round(prop.table(table(tot$LC1)),5)

# ---- Load master reference and compute proportions ----
load("master_complete.RData")
round(prop.table(table(master_tot$LC_pred)),5)

# ---- Visual comparison of LC distributions ----
# Barplot: distributions of tot$LC1 vs master_tot$LC_pred (percentages)

cats <- sort(unique(c(levels(factor(tot$LC1)), levels(factor(master_tot$LC_pred)))))

perc_tot    <- as.numeric(100 * prop.table(table(factor(tot$LC1,        levels = cats))))
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
