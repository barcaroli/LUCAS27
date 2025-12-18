###############################################################
# Script: 3.2.LF_process.R
# Purpose: Process the 2022 observed LF sample to build LF
#          indicators (R, F), compute final weights, and
#          summarize by LF stratum for allocation.
# Main steps:
# 1) Load observed LF data and master attributes.
# 2) Build LF presence flags across 41 groups and compute R/F.
# 3) Prepare weights for analysis (currently placeholder=1).
# 4) Compute weighted means and standard deviations by stratum.
# 5) Export stratum-level statistics for allocation.
# Inputs:
# - Lucas_LF_2022.csv
# - LF_sample_2022_obs.csv
# Outputs:
# - LF_stratum_stats.csv
###############################################################
setwd("D:/Google Drive/LUCAS 2026/dati")
library(openxlsx)

lucas_lf <- read.csv("Lucas_LF_2022.csv",stringsAsFactors = FALSE)
lf_samp  <- read.csv("LF_sample_2022_obs.csv",stringsAsFactors = FALSE)
lucas_lf <- merge(lf_samp[,c("POINT_ID","STR25","LC_pred","NUTS2_24","WGT_LUCAS","WGT_LF","wgt_correction")],lucas_lf,by="POINT_ID")

# ---- Compute LF indicators R and F -----------------------------------

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

# ---- Prepare weights -------------------------------------------------
dat <- lucas_lf
## ensure weights are numeric
dat$WGT_LUCAS <- as.numeric(dat$WGT_LUCAS)
dat$WGT_LF    <- as.numeric(dat$WGT_LF)

# dat$WGT_FINAL <- dat$WGT_LUCAS * dat$WGT_LF * dat$wgt_correction
dat$WGT_FINAL <- 1

dat$STRATUM <- paste(dat$NUTS2_24,dat$STR25,sep="*")

# ---- Weighted summaries by stratum ----------------------------------

agg_R <- aggregate(cbind(R_w = R * WGT_FINAL,
                         W = WGT_FINAL) ~ STRATUM,
                   data = dat,
                   FUN = sum,
                   na.rm = TRUE)

agg_R$R_mean <- ifelse(agg_R$W > 0, agg_R$R_w / agg_R$W, NA_real_)

res_perc <- agg_R[, c("STRATUM", "R_mean")]

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
head(stratum_stats)

# ---- Export stratum-level stats -------------------------------------
write.csv(stratum_stats,
          file = "LF_stratum_stats.csv",
          row.names = FALSE)
