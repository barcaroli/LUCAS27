#----------------------------------------------
# Selection of 2027 LF sub-sample (component 2)
#----------------------------------------------
# Description: Selects the non-panel (component 2) portion of the
# 2027 LF sample by filtering the master frame, excluding panel
# units, and allocating the remaining target with PRN sampling.
#----------------------------------------------
# Input datasets:
# - master_complete.RData (master frame with modeling outputs)
# - LF2027_panel.csv (component 1 sample to be excluded)
#----------------------------------------------
# Output datasets:
# - LF2027_nonpanel.csv (selected component 2 units w/ weights)
# - LF27_ratio_by_stratum.png (diagnostic allocation plot)
#----------------------------------------------

# ---- Setup environment ------------------------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")
# library(data.table)

# ---- Load master frame and derive LF eligibility ---------------------
# Read master
load("master_complete.RData")
# load("master_reach.RData")
# master_tot <- merge(master_tot,m[,c("POINT_ID","reach_prob")],by="POINT_ID")
# save(master_tot,file="master_complete.RData")
# Frame with LF eligibility
frame <- master_tot[(
                    (substr(master_tot$CLC18_R,1,1) == 2) |
                    (master_tot$STR25 == 1 |master_tot$STR25 == 2 | master_tot$STR25 == 3) |
                    (master_tot$LU11 == 1)) & 
                    master_tot$reach_prob > 0.5,
                    ]
nrow(frame)                    
# [1] 326890

# ---- Remove panel units and define strata ----------------------------
# Read 2022 processed LF sub-sample
LF22 <- read.csv("LF2027_panel.csv",dec=".")
nrow(LF22)
# [1] 79546
frame <- frame[!frame$POINT_ID %in% LF22$POINT_ID,]
frame$STRATUM_LF <- frame$NUTS0_24
table(frame$STRATUM_LF)

# ---- Assign PRN and allocate target counts ---------------------------
set.seed(1234)

N_total <- nrow(frame)
target_total <- 93000-nrow(LF22)

# Allocation per stratum (proportional, with largest remainder)
counts <- aggregate(list(N = rep(1L, N_total)),
                    by = list(STRATUM_LF = frame$STRATUM_LF),
                    FUN = sum)
counts <- counts[order(counts$STRATUM_LF), ]
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

# ---- Randomize within strata and select allocated units --------------
# Random priority within stratum
frame$PRN <- runif(N_total)

# Join allocation to rows
counts_sub <- counts[, c("STRATUM_LF", "N", "alloc")]
selected <- merge(frame, counts_sub, by = "STRATUM_LF", all.x = TRUE)

# Rank within stratum by PRN (descending) and select top alloc per stratum
selected$rank_in_stratum <- ave(selected$PRN, selected$STRATUM_LF,
                                FUN = function(x) rank(-x, ties.method = "first"))
selected <- selected[selected$rank_in_stratum <= selected$alloc, ]



# ---- Compute selection weights and verify ----------------------------
# Weights
selected$wgt_selection_total <- N_total / target_total
selected$wgt_selection_stratum <- selected$N / selected$alloc

# Verification
ver_sel <- aggregate(list(selected_n = rep(1L, nrow(selected))),
                     by = list(STRATUM_LF = selected$STRATUM_LF),
                     FUN = sum)
ver_check <- merge(counts[, c("STRATUM_LF", "alloc")], ver_sel,
                   by = "STRATUM_LF", all.x = TRUE)
ver_check$selected_n[is.na(ver_check$selected_n)] <- 0L

cat("Total selected:", nrow(selected), "\n")
cat("Sum allocations:", sum(counts$alloc), "\n")
cat("Allocation matches selection by stratum:", all(ver_check$alloc == ver_check$selected_n), "\n")

stopifnot(nrow(selected) == target_total)
stopifnot(sum(counts$alloc) == target_total)
stopifnot(all(ver_check$alloc == ver_check$selected_n))

# samp <- NULL
# samp$POINT_ID <- selected$POINT_ID
# samp$component <- ""
# samp$STRATUM <- selected$STRATUM_LUCAS
# samp$WGT_LUCAS <- selected$WGT_LUCAS 
# samp$WGT_comp <- selected$WGT_LF
# samp$eligibility_comp <- selected$eligibility_rate_LF
# samp$LC1 <- selected$LC1
# samp$wgt_correction <- selected$wgt_correction
# samp$wgt_selection <- selected$wgt_selection_stratum
# samp <- as.data.frame(samp)

# ---- Persist selection and diagnostic plot ---------------------------
write.table(selected, "LF2027_nonpanel.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Final barplot: selected / total per stratum (base R)
plot_df <- merge(counts[, c("STRATUM_LF", "N")], ver_sel,
                 by = "STRATUM_LF", all.x = TRUE)
plot_df$selected_n[is.na(plot_df$selected_n)] <- 0L
plot_df$ratio <- plot_df$selected_n / plot_df$N
plot_df <- plot_df[order(plot_df$STRATUM_LF), ]

png("LF27_ratio_by_stratum.png", width = 1600, height = 900)
op <- par(mar = c(10, 5, 4, 2) + 0.1)
barplot(plot_df$ratio,
        names.arg = plot_df$STRATUM_LF,
        las = 2, cex.names = 0.6,
        ylim = c(0, 1),
        ylab = "Share selected (selected / total)",
        xlab = "STRATUM_LF",
        main = "Selected / Total by STRATUM_LF")
abline(h = target_total / N_total, col = "red", lty = 2)
legend("topright", legend = sprintf("Overall share = %.3f", target_total / N_total),
       lty = 2, col = "red", bty = "n")
par(op)
dev.off()



