# ================================================================
# Script: 1.Prepare_Copernicus.R
# Description: builds the 2027 Copernicus sub-sample context and
#              computes the allocation plan for the extra points needed
#              to reach 130,000 observations across Soil, Grassland, LF,
#              and Copernicus.
# Input: Soil2027_sample.csv, Grassland2027_sample.csv, LF2027_sample.csv,
#        master_complete.RData
# Output: points_selected_in_Soil_Grassland_LF.csv,
#         Copernicus_allocations_LC_pred.csv,
#         proportion and absolute-count comparison plots
# ================================================================

#--------------------------------------------------------------------
# Script purpose:
# - Build the 2027 Copernicus sub-sample context and compute an
#   allocation plan for 27,702 additional points to reach 130,000.
# - Land cover target proportions come from master_tot (LC_pred).
# - Constraints:
#     * Do not remove points already in tot (counts can only increase).
#     * Categories B and E are fixed (no new points added to them).
#     * Prioritize equalizing categories A, D, F, G, H using a
#       discrete water-filling, but never exceed the master proportions
#       (cap at floor(p_master * target_total) per category).
#     * Any remaining budget is assigned to C (capped by availability).
#   This script only computes allocations; it does NOT sample/select
#   specific POINT_IDs. Selection can be performed in a later script.
# - Outputs:
#     * allocations_LC_pred.csv with the per-category additions (n_add).
#     * Two plots:
#         1) Proportions: bars for Master and Initial tot, red line for
#            Final tot (projected after allocation).
#         2) Absolute counts: side-by-side bars for Initial tot (red)
#            and Final tot (blue), no reference to master_tot.
# - Assumptions:
#     * master_complete.RData provides data.frame master_tot with at
#       least: POINT_ID, STR25, LC_pred, NUTS2_21.
#     * Soil2027_sample.csv, Grassland2027_sample.csv, LF2027_sample.csv
#       are available in the working directory and share POINT_ID.

setwd("D:/Google Drive/LUCAS 2026/dati")

# ---- Load thematic selections and compute remaining budget ----
Soil <- read.csv("Soil2027_sample.csv")
Grass <- read.csv("Grassland2027_sample.csv")
LF <- read.csv("LF2027_sample.csv")

tot <- rbind(Soil,Grass,LF)

tot <- tot[!duplicated(tot$POINT_ID),]
nrow(tot)

write.table(tot,"points_selected_in_Soil_Grassland_LF.csv",sep=",",quote=F,row.names=F)

remaining <- 130000 - nrow(tot)
remaining
# [1] 9601

load("master_complete.RData")

# ---- Integrate master attributes for reference proportions ----
tot <- merge(tot,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_21")])

# round(prop.table(table(master_tot$STR25)),5)  # optional check
# round(prop.table(table(tot$STR25)),5)         # optional check

round(prop.table(table(master_tot$LC_pred)),5)
# A       B       C       D       E       F       G       H 
# 0.02989 0.23034 0.42206 0.06008 0.20519 0.01001 0.02962 0.01280 

round(prop.table(table(tot$LC_pred)),5)
# A       B       C       D       E       F       G       H 
# 0.01027 0.44795 0.09770 0.01848 0.41863 0.00472 0.00079 0.00146 

table(tot$LC_pred)





# ---------------------------
# Allocate additional points to align LC_pred master distribution 
# ---------------------------

target_total <- nrow(tot) + remaining

# Target proportions from master_tot
p_tab <- table(master_tot$LC_pred)
p <- prop.table(p_tab)
cats <- names(p)

# Fixed categories (no additions)
fixed_cats <- intersect(c("B", "E"), cats)
fixed_idx <- match(fixed_cats, cats)
fixed_idx <- fixed_idx[!is.na(fixed_idx)]

# Current counts in tot aligned to categories
cur_counts <- as.numeric(table(factor(tot$LC_pred, levels = cats)))

# Utility (non più usata per l'allocazione, mantenuta per eventuali usi)
int_quota <- function(frac, total) {
  if (sum(frac) == 0) return(rep(0L, length(frac)))
  scaled <- frac * (total / sum(frac))
  base <- floor(scaled)
  resid <- total - sum(base)
  if (resid > 0) {
    ord <- order(scaled - base, decreasing = TRUE)
    base[ord[seq_len(resid)]] <- base[ord[seq_len(resid)]] + 1L
  }
  as.integer(base)
}

# Pool disponibile (escludo già presenti in tot)
in_tot <- tot$POINT_ID
avail_idx <- !(master_tot$POINT_ID %in% in_tot)
available <- master_tot[avail_idx, c("POINT_ID","LC_pred")]

# Available counts per category
avail_counts <- as.integer(table(factor(available$LC_pred, levels = cats)))


# ---------------------------
# Allocation strategy:
# 1) Keep B and E fixed (add = 0)
# 2) Equalize A, D, F, G, H via discrete water-filling on absolute counts
#    with cap: final <= master quota per category
# 3) Any residual -> C
# ---------------------------

add_alloc <- rep(0L, length(cats))

# Category indices
eq_cats <- intersect(c("A","D","F","G","H"), cats)
eq_idx <- match(eq_cats, cats)
eq_idx <- eq_idx[!is.na(eq_idx)]

c_idx <- match("C", cats)

# Equalization with discrete water-filling
if (length(eq_idx) > 0) {
  curS <- cur_counts[eq_idx]
  cap_avail <- avail_counts[eq_idx]     # massimo incrementabile per disponibilità
  # Vincolo: non superare la quota del master in termini assoluti
  max_final_master <- floor(as.numeric(p[eq_idx]) * target_total)
  cap_master <- pmax(0L, as.integer(max_final_master - curS))
  # Capacità effettiva: limitata sia da disponibilità sia da quota master
  capS <- pmin(cap_avail, cap_master)
  addsS <- integer(length(eq_idx))
  budget <- remaining
  repeat {
    if (budget <= 0) break
    room <- capS - addsS
    eligible <- which(room > 0L)
    if (length(eligible) == 0L) break
    levels <- curS + addsS
    min_level <- min(levels[eligible])
    cand <- eligible[levels[eligible] == min_level]
    take <- min(length(cand), budget)
    if (take <= 0) break
    addsS[cand[seq_len(take)]] <- addsS[cand[seq_len(take)]] + 1L
    budget <- budget - take
  }
  add_alloc[eq_idx] <- addsS
  # Residuo a C (se presente)
  if (!is.na(c_idx)) {
    resid <- remaining - sum(add_alloc)
    if (resid > 0) {
      add_alloc[c_idx] <- min(resid, avail_counts[c_idx])
    }
  }
}

# Export allocation vector per category (new points to select)
new_points <- data.frame(LC_pred = cats, n_add = as.integer(add_alloc))
write.csv(new_points,"Copernicus_allocations_LC_pred.csv", row.names = FALSE)

# Final checks: projected proportions and total variation distance
final_counts_vec <- cur_counts
table(tot$LC_pred)
final_counts_vec <- final_counts_vec + as.integer(add_alloc)
names(final_counts_vec) <- cats
final_prop <- final_counts_vec / sum(final_counts_vec)
print(round(final_prop, 5))

tv_dist <- sum(abs(as.numeric(final_prop) - as.numeric(p))) / 2
print(tv_dist)

# Note: to materialize 'tot' with new points later you can do:
# to_add_df <- master_tot[master_tot$POINT_ID %in% points_to_add, ]
# tot <- rbind(tot, to_add_df[match(points_to_add, to_add_df$POINT_ID), intersect(names(tot), names(to_add_df))])

# ---------------------------
# Final plot: barplot (Master, Initial tot) + red line (Final)
# ---------------------------

# Proportions per category
prop_master  <- as.numeric(prop.table(table(factor(master_tot$LC_pred, levels = cats))))
prop_initial <- as.numeric(prop.table(table(factor(tot$LC_pred,         levels = cats))))
prop_final   <- as.numeric(final_prop)

# Barplot matrix: rows = series, columns = categories
M <- rbind(Master = prop_master, `Initial tot` = prop_initial)

op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)
par(mar = c(7, 4, 3, 1))

bp <- barplot(M,
              beside = TRUE,
              col = c("grey70", "skyblue"),
              border = NA,
              xaxt = "n",
              ylim = c(0, max(c(M, prop_final)) * 1.2),
              ylab = "Proportion",
              main = "LC_pred: Master vs Initial tot + Final (line)")

# Group centers to draw the line
xg <- colMeans(bp)
lines(xg, prop_final, type = "b", lwd = 3, col = "red")

# X-axis labels (categories)
axis(1, at = xg, labels = cats, las = 1, tick = FALSE)

# Combined legend (bars + red line)
legend("topright",
       legend = c("Master", "Initial tot", "Final tot"),
       fill   = c("grey70", "skyblue", NA),
       border = NA,
       lty    = c(NA, NA, 1),
       lwd    = c(NA, NA, 3),
       col    = c(NA, NA, "red"),
       bty    = "n")

# ---------------------------
# Final plot (absolute counts): side-by-side bars
# ---------------------------

# Absolute counts per category (Initial tot vs Final tot only)
count_initial <- as.integer(table(factor(tot$LC_pred, levels = cats)))
count_final   <- as.integer(final_counts_vec)

M_abs <- rbind(`Initial tot` = count_initial, `Final tot` = count_final)

op2 <- par(no.readonly = TRUE)
on.exit(par(op2), add = TRUE)
par(mar = c(7, 4, 3, 1))

bp2 <- barplot(M_abs,
               beside = TRUE,
               col = c("red", "blue"),
               border = NA,
               xaxt = "n",
               ylim = c(0, max(M_abs) * 1.2),
               ylab = "Count",
               main = "LC_pred: Initial tot (red) vs Final tot (blue)")

# X-axis labels (categories)
xg2 <- colMeans(bp2)
axis(1, at = xg2, labels = cats, las = 2, tick = FALSE)

# Legend
legend("topright",
       legend = c("Initial tot", "Final tot"),
       fill   = c("red", "blue"),
       border = NA,
       bty    = "n")

