## Lucas LF processing
## This script:
## 1) Loads master and LF datasets and keeps only effective LF points.
## 2) Filters the LF frame by land cover/stratum conditions.
## 3) Derives the R index per point as the share of LF groups (out of 41) that contain at least one LF code (W,G,T,D,P,S,C).
## 4) Merges the LF frame with the LF sample to attach weights and stratum info.
## 5) Builds the correction factor per stratum, computes final weights, and produces weighted summaries of R by NUTS2 and by stratum.

# setwd("D:/Google Drive/LUCAS 2026/Procedure_STR25/3.LF/data")
setwd("D:/Google Drive/LUCAS 2026/dati")
library(openxlsx)

## 1. Load datasets -----------------------------------------------------------

# load("master_complete.RData")

lucas_lf <- read.csv("Lucas_LF_2022.csv",stringsAsFactors = FALSE)
# lucas_lf <- merge(lucas_lf,master_tot[,c("POINT_ID","STR25","LC_pred","LU11pred","NUTS2_24")])

# eff <- read.xlsx("effective_points_modules.xlsx")
# lucas_lf <- lucas_lf[lucas_lf$POINT_ID %in% eff$POINT_ID_LF[!is.na(eff$POINT_ID_LF)],]
# 
# lucas_lf <- lucas_lf[(lucas_lf$LU11pred == 1 & lucas_lf$STR25 == 3) | (lucas_lf$STR25 %in% c(1,2)),]
# table(lucas_lf$STR25,lucas_lf$LU11pred)

lf_samp  <- read.csv("LF_sample_2022_obs.csv",stringsAsFactors = FALSE)
lucas_lf <- merge(lf_samp[,c("POINT_ID","STR25","LC_pred","NUTS2_24","WGT_LUCAS","WGT_LF","wgt_correction")],lucas_lf,by="POINT_ID")

## 2. Transform frame: compute R index ---------------------------------------

## columns containing the 41 LF observations
lf_cols   <- grep("FEATURE", names(lucas_lf), value = TRUE)
lf_codes  <- c("W","G","T","D","P","S","C")

## for each 4-field group check if any LF code is present
group_ids  <- sub(".*_", "", lf_cols)
group_cols <- split(lf_cols, group_ids)
num_groups <- length(group_cols)

if (num_groups != 41) {
  warning("Expected 41 LF groups, found ", num_groups)
}

group_flags <- sapply(group_cols, function(cols) {
  apply(lucas_lf[, cols, drop = FALSE], 1,
        function(z) any(z %in% lf_codes, na.rm = TRUE))
})

lucas_lf$R <- rowSums(group_flags, na.rm = TRUE) / num_groups
lucas_lf$F <- ifelse(lucas_lf$R > 0, 1, 0)

# ## 3. Merge frame with sample (POINT_ID = ID) --------------------------------
# 
# # dat <- merge(lucas_lf, lf_samp,
# #              by.x = "POINT_ID",
# #              by.y = "ID",
# #              all.x = FALSE,   # keep only points present in the LF sample
# #              all.y = FALSE)
# dat <- lucas_lf
# 
# ## 4. Correction factor for WGT_LF -------------------------------------------
# ## correttore_h = N_points_stratum_h (2nd dataset) / N_total_points (1st dataset)
# 
# ## total number of points in the first dataset
# N_tot_frame <- table(lf_samp$STRATUM_LF)
# 
# ## count of LF sample points by STRATUM_LF
# N_sample_by_stratum <- table(dat$STRATUM_LF)
# 
# ## correction vector by stratum
# correttore_vec <- N_tot_frame / N_sample_by_stratum
# names(correttore_vec) <- names(N_sample_by_stratum)
# correttore_vec
# 
# ## assign the correction factor to each record (via STRATUM_LF)
# dat$CORRETT_LF <- correttore_vec[ dat$STRATUM_LF ]
# 
# ## if any stratum is missing a correction (NA), set to 0 or 1 per logic
# dat$CORRETT_LF[is.na(dat$CORRETT_LF)] <- 0

## 5. Final weight WGT_FINAL --------------------------------------------------
dat <- lucas_lf
## ensure weights are numeric
dat$WGT_LUCAS <- as.numeric(dat$WGT_LUCAS)
dat$WGT_LF    <- as.numeric(dat$WGT_LF)

# dat$WGT_FINAL <- dat$WGT_LUCAS * dat$WGT_LF * dat$wgt_correction
dat$WGT_FINAL <- 1

## 6. Weighted mean of R by NUTS2_16 -----------------------------------------

agg_R <- aggregate(cbind(R_w = R * WGT_FINAL,
                         W = WGT_FINAL) ~ NUTS2_24,
                   data = dat,
                   FUN = sum,
                   na.rm = TRUE)

agg_R$R_mean <- ifelse(agg_R$W > 0, agg_R$R_w / agg_R$W, NA_real_)

## final table with weighted mean of R per NUTS2_16
res_perc <- agg_R[, c("NUTS2_24", "R_mean")]

print(res_perc)

## ===============================================================
## 7. Output by STRATUM_LF:
##    - N = sum of WGT_FINAL
##    - weighted mean of R
##    - weighted standard deviation (SQM) of R
## ===============================================================

dat$STRATUM <- paste(dat$NUTS2_24,dat$STR25,sep="*")

strata <- sort(unique(dat$STRATUM))

stratum_stats_list <- vector("list", length(strata))

for (i in seq_along(strata)) {
  s   <- strata[i]
  sub <- dat[dat$STRATUM == s, ]
  
  w <- sub$WGT_FINAL
  N_h <- sum(w, na.rm = TRUE)   # "total number of points" (sum of weights)
  
  x <- sub$R
  x[is.na(x)] <- 0

  f <- sub$F
  f[is.na(f)] <- 0
  
  ## weighted mean
  m_R <- if (N_h > 0) sum(w * x, na.rm = TRUE) / N_h else NA
  m_F <- if (N_h > 0) sum(w * f, na.rm = TRUE) / N_h else NA
  
  ## weighted sqm
  v_R  <- if (N_h > 0) sum(w * (x - m_R)^2, na.rm = TRUE) / N_h else NA
  sd_R <- if (!is.na(v_R)) sqrt(v_R) else NA
  v_F  <- if (N_h > 0) sum(w * (f - m_F)^2, na.rm = TRUE) / N_h else NA
  sd_F <- if (!is.na(v_F)) sqrt(v_F) else NA
  
  stratum_stats_list[[i]] <- data.frame(
    STRATUM = s,
    N = N_h,
    R_mean = m_R,
    R_sqm = sd_R,
    F_mean = m_F,
    F_sqm = sd_F,
    row.names = NULL,
    check.names = FALSE
  )
}

stratum_stats <- do.call(rbind, stratum_stats_list)
sum(stratum_stats$N)

## print result
print(stratum_stats)

## save to file
write.csv(stratum_stats,
          file = "LF_stratum_stats.csv",
          row.names = FALSE)

# write.xlsx(stratum_stats,
#            file = "LF_stratum_stats.xlsx",
#            sheetName = "stats",
#            rowNames = FALSE)
