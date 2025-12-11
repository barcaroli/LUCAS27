# ================================================================
# General purpose
# - Calibrate Copernicus sample weights to the master frame totals by STRATUM
# - Produce diagnostics comparing sample vs master for NUTS0 and LC_pred
# - Save the calibrated Copernicus sample to file
#
# Input datasets
# - master_complete.RData (object master_tot with LC_pred, NUTS2_24, STR25)
# - Copernicus2027_sample.csv
#
# Outputs
# - copernicus2027_sample.csv (with calibrated weights)
# - copernicus27_distribution_NUTS0_vs_master.png
# - copernicus27_distribution_LCpred_vs_master.png
# ================================================================
setwd("D:/Google Drive/LUCAS 2026/dati")
copernicus <- read.csv("Copernicus2027_sample.csv")
table(copernicus$module)
load("master_complete.RData")
master_tot$STRATUM <- paste(master_tot$NUTS2_24,master_tot$STR25,sep="*")
nrow(master_tot)
# [1] 1029661
summary(copernicus$WGT_module_27)
sum(copernicus$WGT_module_27)
# [1] 1355052

# Calibration 
master_tot$ones <- 1
frame_totals <- aggregate(ones~STRATUM,data=master_tot,FUN=sum)
sum(frame_totals$ones)
# [1] 1029661
sample_totals <- aggregate(WGT_module_27~STRATUM,data=copernicus,FUN=sum)
correction <- merge(frame_totals,sample_totals,by="STRATUM")
correction$correction_factor <- correction$ones / correction$WGT_module_27
copernicus <- merge(copernicus,correction[,c("STRATUM","correction_factor")])
# copernicus$correction_factor[is.na(copernicus$correction_factor)] <- 0.1
summary(copernicus$correction_factor)

copernicus$WGT_final <- copernicus$WGT_module_27 * copernicus$correction_factor
sum(copernicus$WGT_final,na.rm=TRUE)
#  1029361

copernicus$WGT_module_27 <- copernicus$WGT_final
copernicus$WGT_final <- copernicus$correction_factor <- NULL

# ---- Helper for comparison barplots --------------------------------
render_barplot <- function(mat, cats, title, legend_labels, colors, filename, mar = c(10, 4, 3, 1)) {
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
    axis(1, at = xg, labels = cats, las = 2, tick = FALSE, cex.axis = 0.9)
    legend("topright", legend = legend_labels, fill = colors, border = NA, bty = "n")
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  png(filename, width = 1400, height = 900, res = 150)
  par(mar = mar)
  draw_plot()
  dev.off()
}

# ---- Diagnostic barplots: sample vs master -------------------------
# NUTS0 (first 2 chars of NUTS2 or NUTS2_24)
master_nuts2 <- if ("NUTS2" %in% names(master_tot)) master_tot$NUTS2 else master_tot$NUTS2_24
cop_nuts0 <- substr(copernicus$NUTS2, 1, 2)
master_nuts0 <- substr(master_nuts2, 1, 2)
nuts0_levels <- sort(unique(c(levels(factor(cop_nuts0)), levels(factor(master_nuts0)))))

perc_sample_nuts0 <- as.numeric(100 * prop.table(table(factor(cop_nuts0, levels = nuts0_levels))))
perc_master_nuts0 <- as.numeric(100 * prop.table(table(factor(master_nuts0, levels = nuts0_levels))))

M_nuts0 <- rbind(`Sample` = perc_sample_nuts0, `Master` = perc_master_nuts0)
render_barplot(M_nuts0, nuts0_levels,
               title = "NUTS0 distribution: sample vs master",
               legend_labels = c("Sample", "Master"),
               colors = c("steelblue", "orange"),
               filename = "copernicus27_distribution_NUTS0_vs_master.png",
               mar = c(10, 5, 4, 1))

# LC_pred distribution
cats_lc <- sort(unique(c(levels(factor(copernicus$LC_pred)), levels(factor(master_tot$LC_pred)))))
perc_sample_lc <- as.numeric(100 * prop.table(table(factor(copernicus$LC_pred, levels = cats_lc))))
perc_master_lc <- as.numeric(100 * prop.table(table(factor(master_tot$LC_pred, levels = cats_lc))))

M_lc <- rbind(`Sample` = perc_sample_lc, `Master` = perc_master_lc)
render_barplot(M_lc, cats_lc,
               title = "LC_pred distribution: sample vs master",
               legend_labels = c("Sample", "Master"),
               colors = c("darkgreen", "firebrick"),
               filename = "copernicus27_distribution_LCpred_vs_master.png",
               mar = c(12, 5, 4, 1))

write.table(copernicus,"copernicus2027_sample.csv",sep=",",quote=F,row.names=F)
