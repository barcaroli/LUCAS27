## LF allocation via SamplingStrata
## This script:
## 1) Reads LF stratum stats (means/SDs and sizes).
## 2) Builds the `strata` input for SamplingStrata with R mean/SD.
## 3) Sets target CV for R.
## 4) Runs Bethel allocation, adjusts to available sample size, and exports results.
setwd("D:/Google Drive/LUCAS 2026/dati")
library(SamplingStrata)
available <- 46500
## read LF stratum stats
stratum_stats <- read.csv("LF_stratum_stats.csv",stringsAsFactors = FALSE)

## build the `strata` dataframe for SamplingStrata
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

## target CVs (for R)
# errors <- data.frame(
#   DOM = "DOM1",
#   CV1 = 0.01,
#   CV2 = 0.01,
#   domainvalue = 1
# )
errors <- data.frame(
  DOM = "DOM1",
  CV1 = 0.01,
  CV2 = 0.01
)
rownames(errors) <- NULL
errors

## Bethel allocation (with R2BEAT)
alloc <- bethel(strata, errors, minnumstrat = 10)
sum(alloc)

strata$SOLUZ <- round(alloc)
strata <- adjustSize(available,strata,minnumstr = 10)
sum(round(strata$SOLUZ))
colnames(strata)[ncol(strata)] <- "nh"
strata$nh_opt <- floor(strata$nh)
strata$nh_prop <- 10
strata$nh_prop <- ifelse(strata$nh_prop > strata$N,strata$N,strata$nh_prop)
remainder <- available - sum(strata$nh_prop)
remainder

strata$nh_prop <- round(strata$nh_prop + (strata$N - strata$nh_prop) / sum(strata$N - strata$nh_prop) * remainder)
strata$nh_prop <- ifelse(strata$nh_prop > strata$N,strata$N,strata$nh_prop)
sum(strata$nh_prop)

## quick diagnostics: allocation vs R mean
plot(strata$M1, strata$nh_opt,
     xlab = "R mean by stratum",
     ylab = "nh_opt",
     main = "nh_opt vs R mean")

plot(strata$M1, strata$nh_opt / strata$N,
     xlab = "R mean by stratum",
     ylab = "nh_opt / N",
     main = "Relative optimal allocation vs R mean")

plot(strata$M1, strata$nh_prop / strata$N,
     xlab = "R mean by stratum",
     ylab = "nh_prop / N",
     main = "Relative proportional allocation vs R mean")

## show result
print(strata[, c("STRATO", "N", "nh_opt", "nh_prop")])

sum(strata$nh_opt)
strata$nh_opt2 <- round(strata$nh_opt * available / sum(strata$nh_opt))
sum(strata$nh_opt2)
## save to file
write.csv(strata,
          file = "LF_strata_with_bethel_allocation.csv",
          row.names = FALSE)
