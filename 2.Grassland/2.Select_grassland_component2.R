# ------------------------------------------------------------------------------
# Overview: calibrates and selects component 2 (Extended Grassland) of the LUCAS
# 2027 sample starting from the extended 2022 sample.
# Main inputs:
#   - Survey_2022_wgt_2nd_phase.txt (2022 land cover data and weights)
#   - effective_points_modules.xlsx (actually observed module points)
#   - grassland_extended_sample_2022.csv (Extended Grassland sample 2022)
# Outputs:
#   - Grassland2027_component2.csv (selected points with corrected weights)
#   - Grassland2027_component2_share_by_NUTS2_24.png (diagnostic plot)
# ------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Selection of 2027 Grassland sub-sample (component Extended Grassland)
#--------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# Step1 : calibrate observed 2022 Extended Grassland points
#-------------------------------------------------------------------------------------------
# --- Working directory setup and core data loads ---
setwd("D:/Google Drive/LUCAS 2026/dati")
# library(data.table)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)
library(openxlsx)
# --- Read observed module points and extended 2022 sample ---
# Read observed points in module sub-samples
points <- read.xlsx("effective_points_modules.xlsx")
# Read 2022 Extended Grassland sub-sample
extgrass22 <- read.csv2("grassland_extended_sample_2022.csv",dec=".")
load("master_complete.RData")
extgrass22$LC_pred <- NULL
extgrass22 <- merge(extgrass22,master_tot[,c("POINT_ID","PRN","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
# Select actually observed points
extgrass22obs <- extgrass22[extgrass22$ID %in% points$POINT_ID_Ext..GRASS,]
# Select actually observed points in "E" 
extgrass22obs <- merge(extgrass22obs,lucas22[,c("POINT_ID","LC1")],by.x="ID",by.y="POINT_ID")
extgrass22obs <- extgrass22obs[extgrass22obs$LC1 %in% c("E"),]
# extgrass22obs <- extgrass22obs[extgrass22obs$STR25 == 3L,]
nrow(extgrass22obs)
# [1] 26011
# Calculate calibrated weights
g22_dom <- merge(extgrass22, lucas22[, c("POINT_ID","LC1")], by.x = "ID", by.y = "POINT_ID", all.x = TRUE)
g22_dom <- g22_dom[g22_dom$LC1 %in% c("E","D"), ]
g22_dom <- g22_dom[!is.na(g22_dom$WGT_EXT_GRASSLAND),]

# N.B. : we have no "STRATUM_EXTENDED_GRASSLAND, only STRATUM_LUCAS

# --- Compute weight corrections by LUCAS stratum ---
full_sums <- as.data.table(g22_dom)[
  # , .(sum_full = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  # , .(sum_full = sum(WGT_LUCAS * WGT_EXT_GRASSLAND, na.rm = TRUE)),
  , .(sum_full = sum(WGT_EXT_GRASSLAND, na.rm = TRUE)),
  by = STRATUM_LUCAS
]

obs_sums <- as.data.table(extgrass22obs)[
  # , .(sum_obs = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  # , .(sum_obs = sum(WGT_LUCAS * WGT_EXT_GRASSLAND, na.rm = TRUE)),
  , .(sum_obs = sum(WGT_EXT_GRASSLAND, na.rm = TRUE)),
  by = STRATUM_LUCAS
]

wgts <- merge(full_sums, obs_sums, by = "STRATUM_LUCAS", all.x = TRUE)
wgts[, wgt_correction := sum_full / sum_obs]

extgrass22obs <- merge(extgrass22obs, wgts[, .(STRATUM_LUCAS, wgt_correction)], by = "STRATUM_LUCAS", all.x = TRUE)

# --- Consistency diagnostics on totals and missing strata ---
# Align filters on observed set as well (mirrors g22_dom)
extgrass22obs <- extgrass22obs[!is.na(extgrass22obs$WGT_EXT_GRASSLAND), ]

# Strati presenti nel full ma non negli osservati
missing_strata <- setdiff(full_sums$STRATUM_LUCAS, obs_sums$STRATUM_LUCAS)

# Totali
full_total <- full_sums[, sum(sum_full, na.rm = TRUE)]
corrected_obs_total <- as.data.table(extgrass22obs)[
  , sum(WGT_EXT_GRASSLAND * wgt_correction, na.rm = TRUE)
]
observed_target_total <- full_sums[
  STRATUM_LUCAS %in% obs_sums$STRATUM_LUCAS, sum(sum_full, na.rm = TRUE)
]
missing_total <- if (length(missing_strata)) {
  full_sums[STRATUM_LUCAS %in% missing_strata, sum(sum_full, na.rm = TRUE)]
} else 0

cat(sprintf(
  "Full total: %.2f\nCorrected observed total: %.2f\nTarget on observed strata: %.2f\nWeight in missing strata: %.2f\nResidual (full - corrected): %.2f\n",
  full_total, corrected_obs_total, observed_target_total, missing_total,
  full_total - corrected_obs_total
))

if (abs((full_total - corrected_obs_total) - missing_total) < 1e-6) {
  cat("Difference equals weight of missing strata (as expected).\n")
} else {
  cat("Difference does NOT match missing-strata weight; check duplicates/filters.\n")
}
#----------------------------------------------------------------------------------------

# --- Final consistency check on calibrated weights ---
# Coherence check
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_EXT_GRASSLAND / g22_dom$eligibility_rate_grassland)
# sum(extgrass22obs$WGT_LUCAS * extgrass22obs$WGT_EXT_GRASSLAND / extgrass22obs$eligibility_rate_grassland * extgrass22obs$wgt_correction)
# sum(g22_dom$WGT_LUCAS * g22_dom$WGT_EXT_GRASSLAND)
# sum(extgrass22obs$WGT_LUCAS * extgrass22obs$WGT_EXT_GRASSLAND * extgrass22obs$wgt_correction)
sum(g22_dom$WGT_EXT_GRASSLAND)
# [1] 44292.34
sum(extgrass22obs$WGT_EXT_GRASSLAND * extgrass22obs$wgt_correction)
# [1] 43424.48

#-------------------------------------------------------------------------------------------
# Step 2: selection of remaining points (20000 - n_component1) for component 2 of the 2027 Grassland sample
#-------------------------------------------------------------------------------------------
# --- Reproducibility seed and random numbers for selection ---
# load("master_complete.RData")
# extgrass22obs <- merge(extgrass22obs,master_tot[,c("POINT_ID","PRN")],by.x="ID",by.y="POINT_ID")

# --- Remove points already selected for component 1 ---
comp1_path <- "Grassland2027_component1.csv"
# component1_n <- 11099L
if (file.exists(comp1_path)) {
  comp1 <- fread(comp1_path)
  component1_n <- nrow(comp1)
  before_drop <- nrow(extgrass22obs)
  extgrass22obs <- extgrass22obs[!(extgrass22obs$ID %in% comp1$POINT_ID), ]
  cat(sprintf("Filtered %d component 1 points; %d candidates remain for component 2.\n",
              before_drop - nrow(extgrass22obs), nrow(extgrass22obs)))
} else {
  warning(sprintf("%s not found; overlap with component 1 cannot be removed.", comp1_path))
}

table(extgrass22obs$NUTS2_24)

extgrass22obs <- extgrass22obs[order(extgrass22obs$PRN, decreasing = TRUE),]

# --- Proportional allocation by stratum and point selection ---
# Base R implementation (no data.table)
df <- extgrass22obs[!is.na(extgrass22obs$NUTS2_24), , drop = FALSE]

TOTAL_TARGET <- 20000L
TARGET <- as.integer(max(0, TOTAL_TARGET - component1_n))
if (TARGET > nrow(df)) {
  stop(sprintf("Not enough points left for component 2 after excluding component 1 (need %d, have %d).",
               TARGET, nrow(df)))
}

# Allocation proportional to stratum size (Nh) by NUTS2_24 with largest‑remainder rounding
tab <- table(df$NUTS2_24)
strata <- names(tab)
Nh <- as.numeric(tab); names(Nh) <- strata
# Minimum of 2 per stratum (or 1 when only one unit exists)
nh_min <- ifelse(Nh == 1L, 1L, 2L)
nh_min <- pmin(Nh, nh_min)
if (sum(nh_min) > TARGET) {
  stop(sprintf("TARGET (%d) is smaller than the required minimum across strata (%d).", TARGET, sum(nh_min)))
}

# Allocate remaining proportionally after minimums
remaining_target <- TARGET - sum(nh_min)
allocatable <- Nh - nh_min
allocatable_total <- sum(allocatable)

nh <- nh_min
if (remaining_target > 0 && allocatable_total > 0) {
  prop_extra <- allocatable / allocatable_total
  nh_extra <- floor(prop_extra * remaining_target)
  remainder_extra <- remaining_target - sum(nh_extra)
  if (remainder_extra > 0) {
    frac <- prop_extra * remaining_target - nh_extra
    ord <- order(-frac, -allocatable)
    add_strata <- strata[ord][seq_len(remainder_extra)]
    nh_extra[add_strata] <- nh_extra[add_strata] + 1L
  }
  nh <- nh + nh_extra
}

# Safety cap (should not trigger in typical case)
nh <- pmin(nh, Nh)

# Select within each stratum by descending PRN
sel_list <- list(); idx <- 1L
for (s in strata) {
  take <- nh[s]
  if (is.na(take) || take <= 0) next
  sub <- df[df$NUTS2_24 == s, , drop = FALSE]
  if (!nrow(sub)) next
  sub <- sub[order(sub$PRN, decreasing = TRUE), , drop = FALSE]
  if (take > nrow(sub)) take <- nrow(sub)
  out <- sub[seq_len(take), , drop = FALSE]
  out$wgt_selection <- as.numeric(Nh[s] / nh[s])
  sel_list[[idx]] <- out
  idx <- idx + 1L
}

comp2 <- do.call(rbind, sel_list)

# --- Diagnostic plot: share selected vs total by NUTS2 ---
# Barplot: share selected / total by NUTS2_24 (base R)
total_tab <- table(df$NUTS2_24)
sel_tab <- table(comp2$NUTS2_24)
strata_all <- names(total_tab)
sel_counts <- rep(0L, length(strata_all)); names(sel_counts) <- strata_all
sel_counts[names(sel_tab)] <- as.integer(sel_tab)
share <- sel_counts / as.numeric(total_tab)
overall_share <- sum(sel_counts) / sum(as.numeric(total_tab))

png("Grassland2027_component2_share_by_NUTS2_24.png", width = 2000, height = 1200, res = 150)
op <- par(no.readonly = TRUE)
par(mar = c(12, 5, 4, 2) + 0.1)
mp <- barplot(share,
              ylim = c(0, 1),
              las = 2,
              cex.names = 0.6,
              col = "grey85",
              border = "grey30",
              ylab = "Share selected (selected / total)",
              main = "Selected / Total by NUTS2_24")
abline(h = overall_share, col = "red", lty = 2, lwd = 2)
usr <- par("usr")
text(x = usr[2], y = overall_share, labels = paste("Overall share =", round(overall_share, 3)), pos = 2, col = "red")
par(op)
dev.off()

# --- Export selected component ---
# comp2$LC_pred <- NULL
# comp2 <- merge(comp2,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
colnames(comp2)[2] <- "POINT_ID"

samp <- NULL
samp$POINT_ID <- comp2$POINT_ID
samp$module <- "GRASSLAND"
samp$component <- "component2"
samp$NUTS2 <- comp2$NUTS2_24
samp$LC_pred <- comp2$LC_pred
samp$STR25 <- comp2$STR25
samp$WGT_LUCAS <- comp2$WGT_LUCAS 
samp$WGT_comp <- comp2$wgt_selection
samp$eligibility_comp <- comp2$eligibility_rate_ext_grassland
samp$wgt_correction <- comp2$wgt_correction
samp$wgt_selection <- comp2$WGT_EXT_GRASSLAND
samp <- as.data.frame(samp)

write.table(samp, "Grassland2027_component2.csv", sep = ",", quote = TRUE, row.names = FALSE)

