# ------------------------------------------------------------------------------
# Overview: builds component 1 (non-extended Grassland) of the LUCAS 2027 sample
# from the 2022 survey weights and data.
# Main inputs:
#   - Survey_2022_wgt_2nd_phase.txt (weights/attributes of 2022 points)
#   - effective_points_modules.xlsx (actually observed module points)
#   - grassland_sample_2022.csv (Grassland 2022 sample with weights)
# Output:
#   - Grassland2027_component1.csv (selected points with weight correction)
# ------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Selection of 2027 Grassland sub-sample (component Grassland not extended)
#--------------------------------------------------------------------------
# --- Environment and packages ---
library(data.table)
library(openxlsx)

# --- Working directory ---
setwd("D:/Google Drive/LUCAS 2026/dati")

# --- Load 2022 Grassland sample ---
grass22 <- read.csv2("grassland_sample_2022.csv",dec=".")
grass22$LC_pred_22<-grass22$LC_pred
grass22$LC_pred<-NULL

# --- Quick weight check against population size ---
sum(grass22$WGT_LUCAS*grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland)

# --- Enrich with LC prediction and NUTS2 info ---
load("master_complete.RData")
grass22 <- merge(grass22,master_tot[,c("POINT_ID","STR25","LC_pred","NUTS2_24")],by.x="ID",by.y="POINT_ID")
table(grass22$LC_pred_22,grass22$LC_pred)

master_tot$ones <- 1
sum(master_tot$ones[master_tot$LC_pred %in% c("D","E")])

# --- Load observed module points and flag observed Grassland sample ---
points <- read.xlsx("effective_points_modules.xlsx")
grass22$obs<-ifelse(grass22$ID %in% points$POINT_ID_Grassland,1,0)

# --- Compare totals with/without observed flag ---
sum(grass22$WGT_LUCAS*grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland)

sum(grass22$WGT_LUCAS*grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland*grass22$obs)


# --- Add observed LC1 class from survey data ---
lucas22 <- fread("Survey_2022_wgt_2nd_phase.txt")
lucas22$LC1 <- substr(lucas22$SURVEY_LC1,1,1)


grass22 <- merge(grass22,lucas22[,c("POINT_ID","LC1","STRATUM_LUCAS")],by.x="ID",by.y="POINT_ID")
addmargins(table(grass22$LC1,grass22$obs, useNA = "always"))
nrow(grass22)


# --- Compute weight correction by grassland stratum ---
# full_sums*: totals on full sample; obs_sums*: totals on observed only
full_sums1 <- as.data.table(grass22)[
   , .(sum_full = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  by = .(STRATUM_GRASSLAND,LC1)
]

obs_sums1 <- as.data.table(grass22[grass22$obs==1,])[
   , .(sum_obs = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  by =.(STRATUM_GRASSLAND,LC1)
  ]
wgts1 <- merge(full_sums1, obs_sums1, by = c("STRATUM_GRASSLAND","LC1"), all.x = TRUE)
wgts1[, wgt_correction1 := sum_full / sum_obs]
full_sums2 <- as.data.table(grass22)[
  , .(sum_full = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  by = .(STRATUM_GRASSLAND)
]
obs_sums2 <- as.data.table(grass22[grass22$obs==1,])[
  , .(sum_obs = sum(WGT_LUCAS * WGT_GRASSLAND / eligibility_rate_grassland, na.rm = TRUE)),
  by=.(STRATUM_GRASSLAND)
]
wgts2 <- merge(full_sums2, obs_sums2, by = c("STRATUM_GRASSLAND"), all.x = TRUE)
wgts2[, wgt_correction2 := sum_full / sum_obs]

# --- Attach corrections back to point-level data ---
grass22 <- merge(grass22, wgts1[, .(STRATUM_GRASSLAND, LC1, wgt_correction1)], by =c("STRATUM_GRASSLAND", "LC1"), all.x = TRUE)
grass22 <- merge(grass22, wgts2[, .(STRATUM_GRASSLAND, wgt_correction2)],      by =c("STRATUM_GRASSLAND"), all.x = TRUE)

# Prefer LC1-specific correction when available, fallback to stratum-only
grass22$wgt_correction<-grass22$wgt_correction1
summary(grass22$wgt_correction)
grass22$wgt_correction<-ifelse(is.na(grass22$wgt_correction),grass22$wgt_correction2,grass22$wgt_correction)
sum(grass22$WGT_LUCAS* grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland*grass22$wgt_correction*grass22$obs,na.rm =T)

# --- Compute corrected weights for observed points only ---
grass22$WGT_corretto<-0
grass22$WGT_corretto<-ifelse(grass22$obs==1,grass22$WGT_LUCAS* grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland*grass22$wgt_correction,0)
sum(grass22$WGT_corretto)
aggregate(data=grass22,WGT_corretto~LC_pred,FUN=sum)


# --- Consistency check: original total weight ---
sum(grass22$WGT_LUCAS* grass22$WGT_GRASSLAND/grass22$eligibility_rate_grassland)

table(grass22$LC_pred,grass22$obs)

# --- Build output for observed LC1 D/E points ---
grass22obs<-grass22[grass22$obs==1, ]
grass22obs<-grass22obs[grass22obs$LC1 %in% c("E","D"),]

# --- Diagnostic plot: share selected vs total by NUTS2 ---
total_tab <- table(grass22$NUTS2_24)
sel_tab <- table(grass22obs$NUTS2_24)
strata_all <- names(total_tab)
sel_counts <- rep(0L, length(strata_all)); names(sel_counts) <- strata_all
sel_counts[names(sel_tab)] <- as.integer(sel_tab)
share <- sel_counts / as.numeric(total_tab)
overall_share <- sum(sel_counts) / sum(as.numeric(total_tab))

png("2.1.Grassland2027_component1_share_by_NUTS2_24.png", width = 2000, height = 1200, res = 150)
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

samp <- NULL
samp$POINT_ID <- grass22obs$ID
samp$module <- "GRASSLAND"
samp$component <- "component1"
samp$NUTS2 <- grass22obs$NUTS2_24
samp$LC_pred <- grass22obs$LC_pred
samp$STR25 <- grass22obs$STR25
samp$WGT_LUCAS <-grass22obs$WGT_LUCAS 
samp$eligibility_comp <- grass22obs$eligibility_rate_grassland
samp$wgt_module_22 <- grass22obs$WGT_GRASSLAND
samp$wgt_correction_22 <- grass22obs$wgt_correction
samp$WGT_comp_27 <- 1
samp$LC1<-grass22obs$LC1
samp <- as.data.frame(samp)
addmargins(table(samp$LC_pred,samp$LC1))
sum(samp$WGT_LUCAS* samp$wgt_module_22/samp$eligibility_comp*samp$wgt_correction_22,na.rm =T)

# --- Export component 1 sample ---
write.table(samp,"Grassland2027_component1.csv",sep=",",quote=T,row.names=F)
