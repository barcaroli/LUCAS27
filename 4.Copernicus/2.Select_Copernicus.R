# ================================================================
# Script: 2.Select_Copernicus.R
# Description: takes the Copernicus allocation plan and performs the
#              deterministic PRN-based selection of the new points per
#              LC_pred and STRATUM (NUTS2_24*STR25), ensuring balanced totals and weights.
# Input: master_complete.RData, Copernicus_allocations_LC_pred.csv
#        (or allocations_LC_pred.csv fallback),
#        points_selected_in_Soil_Grassland_LF.csv
# Output: Copernicus2027_sample.csv
# ================================================================
#------------------------------------------------
# Points selection for 2027 Copernicus sub-sample 
#------------------------------------------------
# ---- Working directory and inputs ----
setwd("D:/Google Drive/LUCAS 2026/dati")

load("master_complete.RData")

# Read allocation table (fallback if renamed)
new_points <- tryCatch(read.csv("Copernicus_allocations_LC_pred.csv"),
                       error = function(e) read.csv("allocations_LC_pred.csv"))

already_selected <- read.csv("points_selected_in_Soil_Grassland_LF.csv")

# ---- Build the available frame and enforce exclusions ----

# Choose master data frame from loaded RData
if (exists("master_tot")) {
  master_df <- master_tot
} else if (exists("master_complete")) {
  master_df <- master_complete
} else {
  stop("Neither master_tot nor master_complete found in master_complete.RData")
}

req_cols <- c("POINT_ID", "LC_pred", "NUTS2_24", "STR25", "PRN")
missing_cols <- setdiff(req_cols, names(master_df))
if (length(missing_cols) > 0) stop(paste("Missing columns in master:", paste(missing_cols, collapse=", ")))

# Exclude points already selected in Soil/Grassland/LF if provided
exclusion_ids <- character(0)
if (exists("already_selected")) {
  idcol <- intersect(c("POINT_ID","point_id","Point_ID"), names(already_selected))
  if (length(idcol) == 0) {
    warning("'already_selected' provided but no POINT_ID column found; not excluding any IDs.")
  } else {
    exclusion_ids <- as.character(already_selected[[idcol[1]]])
  }
} else {
  # Try to read default file if present
  if (file.exists("points_selected_in_Soil_Grassland_LF.csv")) {
    already_selected <- read.csv("points_selected_in_Soil_Grassland_LF.csv")
    idcol <- intersect(c("POINT_ID","point_id","Point_ID"), names(already_selected))
    if (length(idcol) > 0) exclusion_ids <- as.character(already_selected[[idcol[1]]])
  }
}

master_avail <- master_df[!(master_df$POINT_ID %in% exclusion_ids), ]
master_avail$STRATUM <- paste(master_avail$NUTS2_24, master_avail$STR25, sep = "*")

# ---- Standardise allocation table columns ----
# Standardize new_points columns
if (!("LC_pred" %in% names(new_points))) stop("new_points must contain column LC_pred")
if (!("n_add" %in% names(new_points))) {
  # try to infer
  cand <- intersect(names(new_points), c("n","add","N_add","n_to_add","n_points"))
  if (length(cand) == 0) stop("new_points must contain column n_add (number of points to add)")
  names(new_points)[match(cand[1], names(new_points))] <- "n_add"
}

# Deterministic PRN generator from POINT_ID using base R only
# prn_from_id <- function(id) {
#   s <- charToRaw(as.character(id))
#   h <- 0L
#   for (b in as.integer(s)) h <- (h * 131L + b) %% 2147483647L
#   (h + 0.5) / 2147483647
# }

# Helper: largest remainder allocation to hit exact totals per LC_pred
largest_remainder <- function(q) {
  base <- floor(q)
  resid <- sum(q) - sum(base)
  k <- round(resid)
  if (k > 0) {
    ord <- order(q - base, decreasing = TRUE)
    base[ord[seq_len(k)]] <- base[ord[seq_len(k)]] + 1L
  } else if (k < 0) {
    k <- abs(k)
    ord <- order(q - base, decreasing = FALSE)
    base[ord[seq_len(k)]] <- pmax(0L, base[ord[seq_len(k)]] - 1L)
  }
  as.integer(base)
}

# Selection per LC_pred
cats_to_add <- as.character(new_points$LC_pred[new_points$n_add > 0])
cats_to_add <- intersect(cats_to_add, unique(master_df$LC_pred))

selected_list <- vector("list", length(cats_to_add))
names(selected_list) <- cats_to_add

# ---- Iterate by category and draw points ----
for (cat in cats_to_add) {
  n_needed <- as.integer(new_points$n_add[new_points$LC_pred == cat][1])
  pop <- master_avail[master_avail$LC_pred == cat, c("POINT_ID","LC_pred","NUTS2_24","STR25","STRATUM","PRN")]
  if (nrow(pop) == 0L || n_needed <= 0L) next
  if (n_needed > nrow(pop)) {
    warning(sprintf("LC_pred %s: requested %d exceeds available %d; capping.", cat, n_needed, nrow(pop)))
    n_needed <- nrow(pop)
  }
  
  # Assign deterministic PRN
  # pop$PRN <- prn_from_id(pop$POINT_ID)
  
  # Split by STRATUM (NUTS2_24 x STR25) and compute targets with a minimum per stratum
  g <- table(pop$STRATUM)
  strata <- names(g)
  n_g <- as.numeric(g)
  # Minimum: 2 per stratum, or 1 if only one available unit
  min_target <- pmin(ifelse(n_g == 1L, 1L, 2L), n_g)
  min_total <- sum(min_target)
  if (min_total > n_needed) {
    warning(sprintf("LC_pred %s: minimum allocation (%d) exceeds requested %d; relaxing minima.", cat, min_total, n_needed))
    # Relax to at least one per stratum when possible
    min_target <- pmin(1L, n_g)
    if (sum(min_target) > n_needed) {
      # Not enough total to place one in each stratum: prioritise larger strata
      ord <- order(n_g, decreasing = TRUE)
      keep <- ord[seq_len(n_needed)]
      min_target[] <- 0L
      min_target[keep] <- 1L
    }
  }
  target <- min_target
  remaining <- n_needed - sum(target)
  if (remaining > 0) {
    extra_cap <- pmax(0L, n_g - target)
    if (sum(extra_cap) == 0L) {
      if (remaining > 0) warning(sprintf("LC_pred %s: requested %d but only %d available after minima.", cat, n_needed, sum(target)))
    } else {
      q_extra <- remaining * (extra_cap / sum(extra_cap))
      add <- largest_remainder(q_extra)
      add <- pmin(add, extra_cap)
      target <- target + add
      # Adjust in case rounding drift remains
      diff_total <- n_needed - sum(target)
      if (diff_total != 0) {
        rem <- q_extra - floor(q_extra)
        ord <- order(rem, decreasing = diff_total > 0)
        for (j in ord) {
          if (diff_total == 0) break
          if (diff_total > 0 && target[j] < n_g[j]) {
            target[j] <- target[j] + 1L
            diff_total <- diff_total - 1L
          } else if (diff_total < 0 && target[j] > min_target[j]) {
            target[j] <- target[j] - 1L
            diff_total <- diff_total + 1L
          }
        }
      }
    }
  }
  
  # Select within each STRATUM by PRN and attach weight_copernicus = N_h / n_h
  sel_rows <- vector("list", length(strata))
  for (i in seq_along(strata)) {
    ng <- strata[i]
    N_h <- n_g[i]
    n_h <- target[i]
    if (n_h <= 0L) { sel_rows[[i]] <- NULL; next }
    sub <- pop[pop$STRATUM == ng, ]
    sub <- sub[order(sub$PRN), ]
    take <- head(sub, n_h)
    take$weight_copernicus <- N_h / n_h
    sel_rows[[i]] <- take[, c("POINT_ID","LC_pred","NUTS2_24","STR25","STRATUM","weight_copernicus")]
  }
  sel_meta <- do.call(rbind, sel_rows)
  sel_full <- master_df[match(sel_meta$POINT_ID, master_df$POINT_ID), ]
  sel_full$weight_copernicus <- sel_meta$weight_copernicus
  sel_full$STRATUM <- sel_meta$STRATUM
  selected_list[[cat]] <- sel_full
}

copernicus_selected <- do.call(rbind, selected_list)
copernicus_selected <- copernicus_selected[!duplicated(copernicus_selected$POINT_ID),]

# ---- Export selection and report diagnostics ----
# write.csv(copernicus_selected, "Copernicus2027_sample.csv", row.names = FALSE)

cat("\nRequested by LC_pred (n_add) vs selected:\n")
req <- setNames(as.integer(new_points$n_add), as.character(new_points$LC_pred))
sel <- table(copernicus_selected$LC_pred)
print(data.frame(LC_pred = names(req), requested = as.integer(req),
                 selected = as.integer(sel[names(req)]), row.names = NULL))

cat("\nSelected counts by LC_pred x STRATUM (NUTS2_24*STR25):\n")
print(table(copernicus_selected$LC_pred, copernicus_selected$STRATUM))

cat("\nSampling rate per LC_pred x STRATUM (NUTS2_24*STR25) (plot saved to sampling_rate_by_stratum.png):\n")
avail_counts <- as.data.frame(table(master_avail$LC_pred, master_avail$STRATUM))
names(avail_counts) <- c("LC_pred","STRATUM","available")
sel_counts <- as.data.frame(table(copernicus_selected$LC_pred, copernicus_selected$STRATUM))
names(sel_counts) <- c("LC_pred","STRATUM","selected")
strata_summary <- merge(avail_counts, sel_counts, by = c("LC_pred","STRATUM"), all = TRUE)
strata_summary$available[is.na(strata_summary$available)] <- 0L
strata_summary$selected[is.na(strata_summary$selected)] <- 0L
strata_summary$sampling_rate <- ifelse(strata_summary$available > 0, strata_summary$selected / strata_summary$available, 0)
strata_summary$stratum <- paste(strata_summary$LC_pred, strata_summary$STRATUM, sep = "*")
if (nrow(strata_summary) > 0) {
  strata_parts <- do.call(
    rbind,
    strsplit(
      ifelse(is.na(strata_summary$STRATUM), "NA*NA", as.character(strata_summary$STRATUM)),
      "\\*"
    )
  )
  strata_summary$NUTS2_24 <- strata_parts[, 1]
  strata_summary$STR25 <- strata_parts[, 2]
} else {
  strata_summary$NUTS2_24 <- character(0)
  strata_summary$STR25 <- character(0)
}
print(strata_summary[, c("LC_pred","NUTS2_24","STR25","available","selected","sampling_rate")])

if (nrow(strata_summary) > 0) {
  png("Copernicus_sampling_rate_by_stratum.png", width = 1600, height = 900, res = 150)
  par(mar = c(12, 5, 4, 1))
  barplot(strata_summary$sampling_rate,
          names.arg = strata_summary$stratum,
          las = 2, cex.names = 0.7,
          ylab = "Sampling rate (selected / available)",
          main = "Copernicus sampling rate by selection stratum",
          ylim = c(0, max(strata_summary$sampling_rate, na.rm = TRUE) * 1.1))
  abline(h = 0, col = "gray40")
  dev.off()
}

# copernicus_selected_b <- NULL
# copernicus_selected_b$POINT_ID <- copernicus_selected$POINT_ID
# copernicus_selected_b$component <- ""
# copernicus_selected_b$STRATUM <- paste(copernicus_selected$LC_pred,copernicus_selected$NUTS0_24,sep="*")
# copernicus_selected_b$WGT_LUCAS <- ""
# copernicus_selected_b$WGT_comp <- 1
# copernicus_selected_b$eligibility_comp <- 1
# copernicus_selected_b$LC1 <- copernicus_selected$LC_pred
# copernicus_selected_b$wgt_correction <- 1
# copernicus_selected_b$wgt_selection <- copernicus_selected$weight_copernicus
# copernicus_selected_b <- as.data.frame(copernicus_selected_b)

# Write output and print summaries
samp <- NULL
samp$POINT_ID <- copernicus_selected$POINT_ID
samp$module <- "Copernicus"
samp$component <- ""
samp$NUTS2 <- copernicus_selected$NUTS2_24
samp$LC_pred <- copernicus_selected$LC_pred
samp$STR25 <- copernicus_selected$STR25
# samp$STRATUM <- paste(samp$NUTS2, samp$STR25, sep = "*")
samp$WGT_LUCAS <- 1
samp$WGT_comp <- 1
samp$eligibility_comp <- NA
samp$wgt_correction <- 1
samp$wgt_selection <- copernicus_selected$weight_copernicus
samp <- as.data.frame(samp)

# Write the additional points needed by Copernicus
write.csv(samp, "Copernicus2027_sample_add.csv", row.names = FALSE)

cat("\nRequested by LC_pred (n_add) vs selected:\n")
req <- setNames(as.integer(new_points$n_add), as.character(new_points$LC_pred))
sel <- table(copernicus_selected$LC_pred)
print(data.frame(LC_pred = names(req), requested = as.integer(req),
                 selected = as.integer(sel[names(req)]), row.names = NULL))

cat("\nSelected counts by LC_pred x STRATUM (NUTS2_24*STR25):\n")
print(table(copernicus_selected$LC_pred, copernicus_selected$STRATUM))

