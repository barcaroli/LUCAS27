# ================================================================
# Script: 2.Select_soil.R
# Description: performs the sampling design for the 2027 soil
#              sub-sample by allocating units per stratum and selecting
#              random points with consistent weights.
# Input: soil2022_processed.csv
# Output: Soil2027_sample.csv, soil27_ratio_by_stratum.png
# ================================================================
#----------------------------------
# Selection of 2027 Soil sub-sample
#----------------------------------
# ---- Initial setup and data import ----
setwd("D:/Google Drive/LUCAS 2026/dati")

# Read 2022 processed Soil sub-sample
soil22 <- read.csv("soil2022_processed.csv",dec=".")
nrow(soil22)
# [1] 28010
table(soil22$STRATUM_SOIL)

# # Assign PRN and select proportionally within each STRATUM_SOIL (base R only)
set.seed(1234)

N_total <- nrow(soil22)
target_total <- 25000L

# ---- Target allocation per stratum using the largest remainder method ----
counts <- aggregate(list(N = rep(1L, N_total)),
                    by = list(STRATUM_SOIL = soil22$STRATUM_SOIL),
                    FUN = sum)
counts <- counts[order(counts$STRATUM_SOIL), ]
counts$alloc_raw <- target_total * counts$N / N_total
counts$alloc <- floor(counts$alloc_raw)
rem <- target_total - sum(counts$alloc)
if (rem > 0L) {
  nS <- nrow(counts)
  base_add <- rem %/% nS
  extra <- rem %% nS
  if (base_add > 0L) counts$alloc <- counts$alloc + base_add
  if (extra > 0L) {
    frac <- counts$alloc_raw - floor(counts$alloc_raw)
    ord <- order(frac, decreasing = TRUE)
    idx <- ord[seq_len(extra)]
    counts$alloc[idx] <- counts$alloc[idx] + 1L
  }
}
counts$alloc_raw <- NULL

# Random priority within stratum
soil22$PRN <- runif(N_total)

# ---- Within-stratum randomisation and selection ----
# Join allocation to rows
counts_sub <- counts[, c("STRATUM_SOIL", "N", "alloc")]
selected <- merge(soil22, counts_sub, by = "STRATUM_SOIL", all.x = TRUE)

# Rank within stratum by PRN (descending) and select top alloc per stratum
selected$rank_in_stratum <- ave(selected$PRN, selected$STRATUM_SOIL,
                                FUN = function(x) rank(-x, ties.method = "first"))
selected <- selected[selected$rank_in_stratum <= selected$alloc, ]

# Weights
# ---- Weight calculations and coherence checks ----
selected$wgt_selection_total <- N_total / target_total
selected$wgt_selection_stratum <- selected$N / selected$alloc

# Verification
ver_sel <- aggregate(list(selected_n = rep(1L, nrow(selected))),
                     by = list(STRATUM_SOIL = selected$STRATUM_SOIL),
                     FUN = sum)
ver_check <- merge(counts[, c("STRATUM_SOIL", "alloc")], ver_sel,
                   by = "STRATUM_SOIL", all.x = TRUE)
ver_check$selected_n[is.na(ver_check$selected_n)] <- 0L

cat("Total selected:", nrow(selected), "\n")
cat("Sum allocations:", sum(counts$alloc), "\n")
cat("Allocation matches selection by stratum:", all(ver_check$alloc == ver_check$selected_n), "\n")

stopifnot(nrow(selected) == target_total)
stopifnot(sum(counts$alloc) == target_total)
stopifnot(all(ver_check$alloc == ver_check$selected_n))

# ---- Build the final sample table ----
samp <- NULL
samp$POINT_ID <- selected$POINT_ID
samp$component <- ""
samp$STRATUM <- selected$STRATUM_SOIL
samp$WGT_LUCAS <- selected$WGT_LUCAS 
samp$WGT_comp <- selected$WGT_SOIL
samp$eligibility_comp <- selected$eligibility_rate_soil
samp$LC1 <- selected$LC1
samp$wgt_correction <- selected$wgt_correction
samp$wgt_selection <- selected$wgt_selection_stratum
samp <- as.data.frame(samp)

# Output
# ---- Export the sample and summary plot ----
write.table(samp, "Soil2027_sample.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Final barplot: selected / total per stratum (base R)
plot_df <- merge(counts[, c("STRATUM_SOIL", "N")], ver_sel,
                 by = "STRATUM_SOIL", all.x = TRUE)
plot_df$selected_n[is.na(plot_df$selected_n)] <- 0L
plot_df$ratio <- plot_df$selected_n / plot_df$N
plot_df <- plot_df[order(plot_df$STRATUM_SOIL), ]

png("soil27_ratio_by_stratum.png", width = 1600, height = 900)
op <- par(mar = c(10, 5, 4, 2) + 0.1)
barplot(plot_df$ratio,
        names.arg = plot_df$STRATUM_SOIL,
        las = 2, cex.names = 0.6,
        ylim = c(0, 1),
        ylab = "Share selected (selected / total)",
        xlab = "STRATUM_SOIL",
        main = "Selected / Total by STRATUM_SOIL")
abline(h = target_total / N_total, col = "red", lty = 2)
legend("topright", legend = sprintf("Overall share = %.3f", target_total / N_total),
       lty = 2, col = "red", bty = "n")
par(op)
dev.off()



