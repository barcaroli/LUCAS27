###############################################################
# Script: 5.Select_LF_nonpanel.R
# Purpose: Select the 2027 LF non-panel (component 2) sample by
#          filtering the frame, removing panel units, and drawing
#          allocations via PRN ranking within strata.
# Main steps:
# 1) Load master frame and compute LF eligibility and strata.
# 2) Exclude panel units and reconcile allocation with remaining strata.
# 3) Attach allocation counts, distribute remainders, and select by PRN.
# 4) Compute selection weights and eligibility corrections.
# 5) Export component-2 sample and diagnostic plot.
# Inputs:
# - master_complete.RData
# - LF2027_panel.csv
# - LF_strata_with_bethel_allocation.csv
# Outputs:
# - LF2027_nonpanel.csv
# - LF27_ratio_by_stratum.png
###############################################################

setwd("D:/Google Drive/LUCAS 2026/dati")

# ---- Load master frame and derive LF eligibility ---------------------
# Read master
load("master_complete.RData")
master_tot$STRATUM_LF <- paste(master_tot$NUTS2_24,master_tot$STR25,sep="*")
# table(master_tot$STR25)
# table(master_tot$STRATUM_LF)
xtabs(~STR25+LU11pred,data=master_tot)

master_tot$eligible <- ifelse((master_tot$LU11pred == 1) | (master_tot$STR25 %in% c(1,2,3))
                         & master_tot$reach_prob > 0.5,1,0)
master_tot$ones <- 1
LF_tot <- aggregate(ones~STRATUM_LF,data=master_tot,FUN=sum)
LF_eligibility <- aggregate(eligible~STRATUM_LF,data=master_tot,FUN=sum)
LF_eligibility_rate <- merge(LF_tot,LF_eligibility,by="STRATUM_LF")
LF_eligibility_rate$eligibility_rate_LF <- LF_eligibility_rate$eligible / LF_eligibility_rate$ones
frame <- master_tot[master_tot$eligible==1,]
nrow(frame)

# ---- Remove panel units and define strata ----------------------------
# Read 2022 processed LF sub-sample and drop those units
panel <- read.csv("LF2027_panel.csv",dec=".")
nrow(panel)

frame <- frame[!frame$POINT_ID %in% panel$POINT_ID,]
nrow(frame)

N_total <- nrow(frame)
target_total <- 93000-nrow(panel)
target_total

# ---- Load allocation and align to remaining strata -------------------
# Allocation per stratum (optimal, with largest remainder)
allocation <- read.csv("LF_strata_with_bethel_allocation.csv")
sum(allocation$nh_opt)
# Modify allocation because some strata disappeared in frame
allocation2 <- allocation[allocation$STRATO %in% unique(frame$STRATUM_LF),]
sum(allocation2$nh_opt)
set.seed(1234)
allocation2$RN <- runif(nrow(allocation2))
remainder <- target_total - sum(allocation2$nh_opt)
remainder
frame$ones <- 1
a <- aggregate(ones~STRATUM_LF,data=frame,FUN=sum)
a <- a[a$STRATUM_LF %in% allocation2$STRATO,]
allocation2 <- merge(allocation2,a,by.x="STRATO",by.y="STRATUM_LF")
allocation2$N <- allocation2$ones
allocation2$nh_opt2 <- allocation2$nh_opt + remainder * (allocation2$nh_opt / sum(allocation2$N))
sum(allocation2$nh_opt2)

allocation2$nh_opt2 <- ifelse(allocation2$RN < 0.53,floor(allocation2$nh_opt2),ceiling(allocation2$nh_opt2))
sum(allocation2$nh_opt2)
allocation2$nh_opt <- ifelse(allocation2$nh_opt2 > allocation2$N,allocation2$N,allocation2$nh_opt2)
sum(allocation2$nh_opt)
allocation2$nh_opt2 <- NULL
head(allocation2)
counts <- aggregate(list(N = rep(1L, N_total)),
                    by = list(STRATUM_LF = frame$STRATUM_LF),
                    FUN = sum)
sum(counts$N)
counts <- merge(counts,allocation2[,c("STRATO","nh_opt","nh_prop"),],by.x="STRATUM_LF",by.y="STRATO",all.y=TRUE)
counts <- counts[order(counts$STRATUM_LF), ]
counts$alloc <- counts$nh_opt
sum(counts$alloc)
rem <- target_total - sum(counts$alloc)
if (rem > 0L) {
  # order_idx <- order(-counts$frac_part, counts$STRATUM_LF)
  order_idx <- order(counts$STRATUM_LF)
  for (i in order_idx) {
    if (rem == 0L) break
    if (counts$alloc[i] < counts$N[i]) {
      counts$alloc[i] <- counts$alloc[i] + 1L
      rem <- rem - 1L
      cat("\nrem: ",rem)
    }
  }
}
if (rem < 0L) {
  # order_idx <- order(counts$frac_part, counts$STRATUM_LF)
  order_idx <- order(counts$STRATUM_LF)
  for (i in order_idx) {
    if (rem == 0L) break
    # if (counts$alloc[i] > counts$min_alloc[i]) {
      counts$alloc[i] <- counts$alloc[i] - 1L
      rem <- rem + 1L
    # }
  }
}
stopifnot(rem == 0L)
counts$alloc_raw <- NULL
counts$frac_part <- NULL
counts$min_alloc <- NULL

sum(counts$alloc)

# ---- Within strata select allocated units ----------------------------
# Join allocation to rows
counts_sub <- counts[, c("STRATUM_LF", "N", "alloc")]
sum(counts_sub$alloc)

# selected <- merge(frame, counts_sub, by = "STRATUM_LF", all.x = TRUE)
selected <- merge(frame, counts_sub, by = "STRATUM_LF")
length(unique(selected$PRN))
# Rank within stratum by PRN (descending) and select top alloc per stratum
selected$rank_in_stratum <- ave(selected$PRN, selected$STRATUM_LF,
                                FUN = function(x) rank(-x, ties.method = "first"))
selected <- selected[selected$rank_in_stratum <= selected$alloc, ]
selected <- selected[!duplicated(selected$POINT_ID),]
length(unique(selected$STRATUM_LF))
selected$ones <- 1
a <- aggregate(ones ~ STRATUM_LF,FUN=sum,data=selected)
a <- merge(a,allocation[,c("STRATO","nh_opt")],by.x="STRATUM_LF",by.y="STRATO",all.x=TRUE)
a <- merge(a,counts_sub,by="STRATUM_LF")
b <- a[a$ones != a$alloc,]
sum(a$ones)
sum(a$nh_opt)
sum(a$alloc)

sum(a$ones)
sum(a$nh_opt)

# ---- Compute selection weights and verify ----------------------------
# Weights
selected$wgt_selection_total <- N_total / target_total
selected$wgt_selection_stratum <- selected$N / selected$alloc

sum(selected$wgt_selection_total)
sum(selected$wgt_selection_stratum)

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

# stopifnot(nrow(selected) == target_total)
stopifnot(sum(counts$alloc) == target_total)
stopifnot(all(ver_check$alloc == ver_check$selected_n))

selected <- merge(selected,LF_eligibility_rate[,c("STRATUM_LF","eligibility_rate_LF")],by="STRATUM_LF")

samp <- NULL
samp$POINT_ID <- selected$POINT_ID
samp$module <- "LF"
samp$component <- "nonpanel"
samp$NUTS2 <- selected$NUTS2_24
samp$LC_pred <- selected$LC_pred
samp$STR25 <- selected$STR25
samp$WGT_LUCAS <- 1 
samp$eligibility_comp <- selected$eligibility_rate_LF
samp$wgt_module_22 <- 1
samp$wgt_correction_22 <- 1
samp$WGT_comp_27 <- selected$wgt_selection_stratum
samp <- as.data.frame(samp)

# ---- Persist selection and diagnostic plot ---------------------------
write.table(samp, "LF2027_nonpanel.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Final barplot: selected / total per stratum (base R)
plot_df <- merge(counts[, c("STRATUM_LF", "N")], ver_sel,
                 by = "STRATUM_LF", all.x = TRUE)
plot_df$selected_n[is.na(plot_df$selected_n)] <- 0L
plot_df$ratio <- plot_df$selected_n / plot_df$N
plot_df <- plot_df[order(plot_df$STRATUM_LF), ]

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

png("3.5.1.LF27_ratio_by_stratum.png", width = 1600, height = 900)
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



