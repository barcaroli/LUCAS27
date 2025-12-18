###############################################################
# Script: 3.LF_alloca.R
# Purpose: Derive optimal and proportional LF allocations per
#          stratum using SamplingStrata (Bethel) and export
#          the allocation table for sample selection.
# Main steps:
# 1) Load LF stratum statistics.
# 2) Build SamplingStrata input frame with means/SDs.
# 3) Run Bethel allocation with target CVs and adjust to totals.
# 4) Compute alternative proportional allocation for comparison.
# 5) Save allocation table with nh_opt and nh_prop.
# Inputs:
# - LF_stratum_stats.csv
# Outputs:
# - LF_strata_with_bethel_allocation.csv
###############################################################

setwd("D:/Google Drive/LUCAS 2026/dati")
library(SamplingStrata)
available <- 46500

# ---- Read LF stratum stats -------------------------------------------
stratum_stats <- read.csv("LF_stratum_stats.csv",stringsAsFactors = FALSE)

# ---- Build the `strata` dataframe for SamplingStrata -----------------
strata <- data.frame(
  STRATUM = c(1:nrow(stratum_stats)),
  STRATO = as.character(stratum_stats$STRATUM),
  N      = as.numeric(stratum_stats$N),
  COST   = rep(1,nrow(stratum_stats)),
  CENS   = rep(0,nrow(stratum_stats)),
  DOM1   = rep(1,nrow(stratum_stats))
)
strata$N <- round(strata$N)
strata$M1 <- stratum_stats$R_mean
strata$S1 <- stratum_stats$R_sqm
strata$M2 <- stratum_stats$F_mean
strata$S2 <- stratum_stats$F_sqm
set.seed(1234)
strata$RN <- runif(nrow(strata))

# ---- Target CVs (for R and F) ----------------------------------------
errors <- data.frame(
  DOM = "DOM1",
  CV1 = 0.01,
  CV2 = 0.01
)
rownames(errors) <- NULL
errors

# ---- Bethel allocation (with R2BEAT) ---------------------------------
alloc <- bethel(strata, errors, minnumstrat = 10)
sum(alloc)

strata$SOLUZ <- round(alloc)
strata <- adjustSize(available,strata,minnumstr = 10)
sum(round(strata$SOLUZ))
colnames(strata)[colnames(strata)=="SOLUZ"] <- "nh_opt"
# sum(strata$nh)

# ---- Adjust optimal allocation to totals -----------------------------
strata$nh_opt <- ifelse(strata$RN < 0.75,floor(strata$nh_opt),ceiling(strata$nh_opt))
sum(strata$nh_opt)
strata$nh_opt <- ifelse(strata$nh_opt< 10,10,strata$nh_opt)
strata$nh_opt <- ifelse(strata$nh_opt > strata$N,strata$N,strata$nh_opt)
sum(strata$nh_opt)
remainder <- available - sum(strata$nh_opt)
remainder
# strata$nh_opt2 <- round(strata$nh_opt + (strata$N - strata$nh_opt) / sum(strata$N - strata$nh_opt) * remainder)
strata$nh_opt2 <- strata$nh_opt + remainder * (strata$nh_opt / sum(strata$N))
sum(strata$nh_opt2)

strata$nh_opt2 <- ifelse(strata$RN < 0.65,floor(strata$nh_opt2),ceiling(strata$nh_opt2))
sum(strata$nh_opt2)
strata$nh_opt <- ifelse(strata$nh_opt2 > strata$N,strata$N,strata$nh_opt2)
sum(strata$nh_opt)

# ---- Proportional allocation alternative -----------------------------
strata$nh_prop <- floor(strata$N*available/sum(strata$N))
sum(strata$nh_prop)
strata$nh_prop <- ifelse(strata$nh_prop< 10,10,strata$nh_prop)
strata$nh_prop <- ifelse(strata$nh_prop > strata$N,strata$N,strata$nh_prop)
sum(strata$nh_prop)
remainder <- available - sum(strata$nh_prop)
remainder
strata$nh_prop <- round(strata$nh_prop + (strata$N - strata$nh_prop) / sum(strata$N - strata$nh_prop) * remainder)
strata$nh_prop <- ifelse(strata$nh_prop > strata$N,strata$N,strata$nh_prop)
sum(strata$nh_prop)

# ---- Quick diagnostics: allocation vs R mean -------------------------
# plot(strata$M1, strata$nh_opt,
#      xlab = "R mean by stratum",
#      ylab = "nh_opt",
#      main = "nh_opt vs R mean")
# 
# plot(strata$M1, strata$nh_opt / strata$N,
#      xlab = "R mean by stratum",
#      ylab = "nh_opt / N",
#      main = "Relative optimal allocation vs R mean")
# 
# plot(strata$M1, strata$nh_prop / strata$N,
#      xlab = "R mean by stratum",
#      ylab = "nh_prop / N",
#      main = "Relative proportional allocation vs R mean")

# ---- Show result and export ------------------------------------------
print(strata[c(1:20), c("STRATO", "N", "nh_opt", "nh_prop")])
strata$nh_opt2 <- strata$RN <- NULL
write.csv(strata,
          file = "LF_strata_with_bethel_allocation.csv",
          row.names = FALSE)
