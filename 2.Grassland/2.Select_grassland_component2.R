# ------------------------------------------------------------------------------
# Overview: calibrates and selects component 2 (Extended Grassland) of the LUCAS
# 2027 sample starting from the extended 2022 sample.
# Main inputs:
#   - Survey_2022_wgt_2nd_phase.txt (2022 land cover data and weights)
#   - effective_points_modules.xlsx (actually observed module points)
#   - grassland_extended_sample_2022.csv (Extended Grassland sample 2022)
# Outputs:
#   - Grassland2027_component2.csv (selected points with corrected weights)
#   - Grassland2027_component2_share_by_NUTS2_16.png (diagnostic plot)
# ------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Selection of 2027 Grassland sub-sample (component Extended Grassland)
#--------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# Step1 : calibrate observed 2022 Extended Grassland points
#-------------------------------------------------------------------------------------------
# --- Working directory setup and core data loads ---
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)
library(openxlsx)
# --- Read observed module points and extended 2022 sample ---
# Read observed points in module sub-samples
points <- read.xlsx("effective_points_modules.xlsx")
# Read 2022 Extended Grassland sub-sample
extgrass22 <- read.csv2("grassland_extended_sample_2022.csv",dec=".")
# --- Keep only actually observed points in class E ---
# Select actually observed points
extgrass22obs <- extgrass22[extgrass22$ID %in% points$POINT_ID_Ext..GRASS,]
# Select actually observed points in "E" and "D"
extgrass22obs <- merge(extgrass22obs,lucas22[,c("POINT_ID","LC1")],by.x="ID",by.y="POINT_ID")
extgrass22obs <- extgrass22obs[extgrass22obs$LC1 %in% c("E","D"),]
nrow(extgrass22obs)
# [1] 26012
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
load("master_complete.RData")
extgrass22obs <- merge(extgrass22obs,master_tot[,c("POINT_ID","PRN")],by.x="ID",by.y="POINT_ID")

# --- Remove points already selected for component 1 ---
comp1_path <- "Grassland2027_component1.csv"
component1_n <- 11099L
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

table(extgrass22obs$NUTS2_16)
# AT11 AT12 AT13 AT21 AT22 AT31 AT32 AT33 AT34 BE21 BE22 BE23 BE24 BE25 BE31 BE32 BE33 BE34 
# 39  196    1  112  202  222  121  125   37   41   40   71   43   60   18   62  109  142 
# BE35 BG31 BG32 BG33 BG34 BG41 BG42 CY00 CZ01 CZ02 CZ03 CZ04 CZ05 CZ06 CZ07 CZ08 DE11 DE12 
# 78  118   58   71  105  131  149   41    2   81  205   53  236   97   84   67  141   60 
# DE13 DE14 DE21 DE22 DE23 DE24 DE25 DE26 DE27 DE30 DE40 DE60 DE71 DE72 DE73 DE80 DE91 DE92 
# 141  147  187  150  168  100  153   44  220    1   91    4   77  110  110   90   29   41 
# DE93 DE94 DEA1 DEA2 DEA3 DEA4 DEA5 DEB1 DEB2 DEB3 DEC0 DED2 DED4 DED5 DEE0 DEF0 DEG0 DK01 
# 73  115   40   83   55   59   93  111  111   54   31   59   50   15   81  118   89   20 
# DK02 DK03 DK04 DK05 EE00 EL30 EL41 EL42 EL43 EL51 EL52 EL53 EL54 EL61 EL62 EL63 EL64 EL65 
# 50  100  149  100  159   17   30   36   10   89   98  150   42   94   12   77   67   62 
# ES11 ES12 ES13 ES21 ES22 ES23 ES24 ES30 ES41 ES42 ES43 ES51 ES52 ES53 ES61 ES62 FI19 FI1B 
# 297  141  138   57   53   15  137   54  279  132   90   88   84   48   55   42  113   50 
# FI1C FI1D FI20 FR10 FRB0 FRC1 FRC2 FRD1 FRD2 FRE1 FRE2 FRF1 FRF2 FRF3 FRG0 FRH0 FRI1 FRI2 
# 113  104    5   31  175  150  406  316   99   83   89   89  278  194  376  246  182  637 
# FRI3 FRJ1 FRJ2 FRK1 FRK2 FRL0 FRM0 HR03 HR04 HU11 HU12 HU21 HU22 HU23 HU31 HU32 HU33 IE04 
# 115   82  202  524  464   52   10  153  245    5   33   52   63   52   63  124  104  432 
# IE05 IE06 ITC1 ITC2 ITC3 ITC4 ITF1 ITF2 ITF3 ITF4 ITF5 ITF6 ITG1 ITG2 ITH1 ITH2 ITH3 ITH4 
# 622  308   85   22   10   80   71   47   44   97   90   63  124  137   51   51  121   47 
# ITH5 ITI1 ITI2 ITI3 ITI4 LT01 LT02 LU00 LV00 MT00 NL11 NL12 NL13 NL21 NL22 NL23 NL31 NL32 
# 131   87   43   45   66  149  453   81  241    6   31  174   43  136  147    6    9   20 
# NL33 NL34 NL41 NL42 PL21 PL22 PL41 PL42 PL43 PL51 PL52 PL61 PL62 PL63 PL71 PL72 PL81 PL82 
# 14   19   82   38  212  142  159  105   70  131   31   68  194   77  187  159  168  232 
# PL84 PL91 PL92 PT11 PT15 PT16 PT17 PT18 RO11 RO12 RO21 RO22 RO31 RO32 RO41 RO42 SE11 SE12 
# 182   73  266  108   20  109   26  114  187  238  198  101   78   11  134   74   64  194 
# SE21 SE22 SE23 SE31 SE32 SE33 SI03 SI04 SK01 SK02 SK03 SK04 
# 233  168  203  135   93  138  196  114   15  105  170  138 
extgrass22obs <- extgrass22obs[order(extgrass22obs$PRN, decreasing = TRUE),]

# --- Proportional allocation by stratum and point selection ---
# Base R implementation (no data.table)
df <- extgrass22obs[!is.na(extgrass22obs$NUTS2_16), , drop = FALSE]

TOTAL_TARGET <- 20000L
TARGET <- as.integer(max(0, TOTAL_TARGET - component1_n))
if (TARGET > nrow(df)) {
  stop(sprintf("Not enough points left for component 2 after excluding component 1 (need %d, have %d).",
               TARGET, nrow(df)))
}

# Allocation proportional to stratum size (Nh) by NUTS2_16 with largest‑remainder rounding
tab <- table(df$NUTS2_16)
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
  sub <- df[df$NUTS2_16 == s, , drop = FALSE]
  if (!nrow(sub)) next
  sub <- sub[order(sub$PRN, decreasing = TRUE), , drop = FALSE]
  if (take > nrow(sub)) take <- nrow(sub)
  out <- sub[seq_len(take), , drop = FALSE]
  out$wgt_selection <- as.numeric(Nh[s] / nh[s])
  sel_list[[idx]] <- out
  idx <- idx + 1L
}

comp2 <- do.call(rbind, sel_list)

# --- Export selected component ---
# Save CSV
write.table(comp2, "Grassland2027_component2.csv", sep = ",", quote = TRUE, row.names = FALSE)

# --- Diagnostic plot: share selected vs total by NUTS2 ---
# Barplot: share selected / total by NUTS2_16 (base R)
total_tab <- table(df$NUTS2_16)
sel_tab <- table(comp2$NUTS2_16)
strata_all <- names(total_tab)
sel_counts <- rep(0L, length(strata_all)); names(sel_counts) <- strata_all
sel_counts[names(sel_tab)] <- as.integer(sel_tab)
share <- sel_counts / as.numeric(total_tab)
overall_share <- sum(sel_counts) / sum(as.numeric(total_tab))

png("Grassland2027_component2_share_by_NUTS2_16.png", width = 2000, height = 1200, res = 150)
op <- par(no.readonly = TRUE)
par(mar = c(12, 5, 4, 2) + 0.1)
mp <- barplot(share,
              ylim = c(0, 1),
              las = 2,
              cex.names = 0.6,
              col = "grey85",
              border = "grey30",
              ylab = "Share selected (selected / total)",
              main = "Selected / Total by NUTS2_16")
abline(h = overall_share, col = "red", lty = 2, lwd = 2)
usr <- par("usr")
text(x = usr[2], y = overall_share, labels = paste("Overall share =", round(overall_share, 3)), pos = 2, col = "red")
par(op)
dev.off()



