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
options(scipen=99999)
#-------------------------------------------------------------------------------------------
# Step1 : calibrate observed 2022 Extended Grassland points
#-------------------------------------------------------------------------------------------
# --- Working directory setup and core data loads ---
setwd("D:/Google Drive/LUCAS 2026/dati")
library(data.table)
library(openxlsx)


extgrass22 <- read.csv2("grassland_extended_sample_2022.csv",dec=".")
extgrass22$LC_pred_22<-extgrass22$LC_pred
extgrass22$LC_pred <- NULL
extgrass22$STRATUM_LUCAS<-NULL

sum(extgrass22$WGT_LUCAS*extgrass22$WGT_EXT_GRASSLAND/extgrass22$eligibility_rate_ext_grassland)

load("master_complete.RData")
extgrass22 <- merge(extgrass22,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24","PRN")],by.x="ID",by.y="POINT_ID")
extgrass22$NUTS0_24<-substr(extgrass22$NUTS2_24,1,2)


# --- Load effective points and Grassland 2022 sample ---
# Read observed points in module sub-samples
points <- read.xlsx("effective_points_modules.xlsx")
extgrass22$obs<-ifelse(extgrass22$ID %in% points$POINT_ID_Ext..GRASS,1,0)

sum(extgrass22$WGT_LUCAS*extgrass22$WGT_EXT_GRASSLAND/extgrass22$eligibility_rate_ext_grassland)

sum(extgrass22$WGT_LUCAS*extgrass22$WGT_EXT_GRASSLAND/extgrass22$eligibility_rate_ext_grassland*extgrass22$obs)

lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)
extgrass22 <- merge(extgrass22,lucas22[,c("POINT_ID","LC1","STRATUM_LUCAS")],by.x="ID",by.y="POINT_ID")
aggregate(data=extgrass22,WGT_LUCAS*WGT_EXT_GRASSLAND/eligibility_rate_ext_grassland~LC_pred,FUN=sum )
aggregate(data=extgrass22,WGT_LUCAS*WGT_EXT_GRASSLAND/eligibility_rate_ext_grassland~LC_pred+obs,FUN=sum )
table(extgrass22$LC1,extgrass22$LC_pred)


addmargins(table(extgrass22$LC1,extgrass22$obs, useNA = "always"))
nrow(extgrass22)


# --- Compute weight correction by grassland stratum ---

full_sums1 <- as.data.table(extgrass22)[
  , .(sum_full = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_ext_grassland, na.rm = TRUE)),
  #by = .(NUTS2_24,LC1)
  by = .(STRATUM_LUCAS)
]

obs_sums1 <- as.data.table(extgrass22[extgrass22$obs==1,])[
  , .(sum_obs = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_ext_grassland, na.rm = TRUE)),
  #by =.(NUTS2_24,LC1)
  by = .(STRATUM_LUCAS)
]

wgts1 <- merge(full_sums1, obs_sums1, by = c("STRATUM_LUCAS"), all.x = TRUE)

wgts1[, wgt_correction1 := sum_full / sum_obs]

full_sums2 <- as.data.table(extgrass22)[
  , .(sum_full = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_ext_grassland, na.rm = TRUE)),
  by = .(NUTS0_24)
]

obs_sums2 <- as.data.table(extgrass22[extgrass22$obs==1,])[
  , .(sum_obs = sum(WGT_LUCAS * WGT_EXT_GRASSLAND / eligibility_rate_ext_grassland, na.rm = TRUE)),
  by=.(NUTS0_24)
]

wgts2 <- merge(full_sums2, obs_sums2, by = c("NUTS0_24"), all.x = TRUE)
wgts2[, wgt_correction2 := sum_full / sum_obs]


extgrass22 <- merge(extgrass22, wgts1[, .(STRATUM_LUCAS, wgt_correction1)], by =c("STRATUM_LUCAS"), all.x = TRUE)
extgrass22 <- merge(extgrass22, wgts2[, .(NUTS0_24, wgt_correction2)], by =c("NUTS0_24"), all.x = TRUE)


extgrass22$wgt_correction<-extgrass22$wgt_correction1
summary(extgrass22$wgt_correction)
extgrass22$wgt_correction<-ifelse(is.na(extgrass22$wgt_correction),extgrass22$wgt_correction2,extgrass22$wgt_correction)
sum(extgrass22$WGT_LUCAS* extgrass22$WGT_EXT_GRASSLAND/extgrass22$eligibility_rate_ext_grassland*extgrass22$wgt_correction*extgrass22$obs,na.rm =T)
extgrass22$WGT_corretto<-0
extgrass22$WGT_corretto<-ifelse(extgrass22$obs==1,extgrass22$WGT_LUCAS* extgrass22$WGT_EXT_GRASSLAND/extgrass22$eligibility_rate_ext_grassland*extgrass22$wgt_correction,0)
sum(extgrass22$WGT_corretto)
aggregate(data=extgrass22,WGT_corretto~LC_pred,FUN=sum)



# --- Consistency diagnostics on totals and missing strata ---
# Align filters on observed set as well (mirrors g22_dom)
# 
#-------------------------------------------------------------------------------------------
# Step 2: selection of remaining points (20000 - n_component1) for component 2 of the 2027 Grassland sample
#-------------------------------------------------------------------------------------------

# --- Remove points already selected for component 1 ---
comp1_path <- "Grassland2027_component1.csv"
extgrass22obs<-extgrass22[extgrass22$obs==1 & extgrass22$LC_pred %in% c("E","D"),]
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


extgrass22obs <- extgrass22obs[order(extgrass22obs$PRN, decreasing = TRUE),]

# --- Proportional allocation by stratum and point selection ---
df <- extgrass22obs[!is.na(extgrass22obs$NUTS2_24), , drop = FALSE]

TOTAL_TARGET <- 20000L
TARGET <- as.integer(max(0, TOTAL_TARGET - component1_n))
TARGET
# [1] 8901
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
  #nh_extra <- round(prop_extra * remaining_target)
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
sum(nh)
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
# sum(comp2$WGT_corretto*comp2$wgt_selection)

# --- Diagnostic plot: share selected vs total by NUTS2 ---
# Barplot: share selected / total by NUTS2_24 (base R)
total_tab <- table(df$NUTS2_24)
sel_tab <- table(comp2$NUTS2_24)
strata_all <- names(total_tab)
sel_counts <- rep(0L, length(strata_all)); names(sel_counts) <- strata_all
sel_counts[names(sel_tab)] <- as.integer(sel_tab)
share <- sel_counts / as.numeric(total_tab)
overall_share <- sum(sel_counts) / sum(as.numeric(total_tab))

png("2.2.Grassland2027_component2_share_by_NUTS2_24.png", width = 2000, height = 1200, res = 150)
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
colnames(comp2)[colnames(comp2)=="ID"] <- "POINT_ID"

comp2<-comp2[comp2$obs==1, ]
comp2<-comp2[comp2$LC1 %in% c("E","D"),]

samp <- NULL
samp$POINT_ID <- comp2$POINT_ID
samp$module <- "GRASSLAND"
samp$component <- "component2"
samp$NUTS2 <- comp2$NUTS2_24
samp$LC_pred <- comp2$LC_pred
samp$STR25 <- comp2$STR25
samp$WGT_LUCAS <- comp2$WGT_LUCAS 
samp$eligibility_comp <- comp2$eligibility_rate_ext_grassland
samp$wgt_module_22 <- comp2$WGT_EXT_GRASSLAND
samp$wgt_correction_22 <- comp2$wgt_correction
samp$WGT_comp_27 <- comp2$wgt_selection
samp$LC1<-comp2$LC1

samp <- as.data.frame(samp)
table(samp$LC_pred,comp2$obs)
table(comp1$LC_pred)


aggregate(data=comp1, WGT_LUCAS* wgt_module_22 /eligibility_comp * wgt_correction_22*WGT_comp_27~LC_pred,FUN=sum)
aggregate(data=samp,  WGT_LUCAS* wgt_module_22 /eligibility_comp * wgt_correction_22*WGT_comp_27~LC_pred,FUN=sum)


write.table(samp, "Grassland2027_component2.csv", sep = ",", quote = TRUE, row.names = FALSE)

