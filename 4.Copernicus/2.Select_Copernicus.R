# ================================================================
# Script: 2.Select_Copernicus.R
# Description: takes the Copernicus allocation plan and performs the
#              deterministic PRN-based selection of the new points per
#              LC_pred and NUTS0_21, ensuring balanced totals and weights.
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

req_cols <- c("POINT_ID", "LC_pred", "NUTS0_21")
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
prn_from_id <- function(id) {
  s <- charToRaw(as.character(id))
  h <- 0L
  for (b in as.integer(s)) h <- (h * 131L + b) %% 2147483647L
  (h + 0.5) / 2147483647
}

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
  pop <- master_avail[master_avail$LC_pred == cat, c("POINT_ID","LC_pred","NUTS0_21")]
  if (nrow(pop) == 0L || n_needed <= 0L) next
  if (n_needed > nrow(pop)) {
    warning(sprintf("LC_pred %s: requested %d exceeds available %d; capping.", cat, n_needed, nrow(pop)))
    n_needed <- nrow(pop)
  }
  
  # Compute common sampling rate for this LC_pred
  rate <- n_needed / nrow(pop)
  
  # Assign deterministic PRN
  pop$PRN <- prn_from_id(pop$POINT_ID)
  
  # Split by NUTS0_21 and compute targets via largest remainder
  g <- table(pop$NUTS0_21)
  nuts <- names(g)
  n_g <- as.numeric(g)
  q <- rate * n_g
  target <- largest_remainder(q)
  # Adjust in case rounding drift remains
  diff_total <- n_needed - sum(target)
  if (diff_total != 0) {
    # distribute by PRN ranks across groups
    rem <- q - floor(q)
    if (diff_total > 0) {
      ord <- order(rem, decreasing = TRUE)
      take <- seq_len(min(diff_total, length(ord)))
      target[ord[take]] <- target[ord[take]] + 1L
    } else {
      ord <- order(rem, decreasing = FALSE)
      take <- seq_len(min(-diff_total, length(ord)))
      target[ord[take]] <- pmax(0L, target[ord[take]] - 1L)
    }
  }
  
  # Select within each NUTS0_21 by PRN and attach weight_copernicus = N_h / n_h
  sel_rows <- vector("list", length(nuts))
  for (i in seq_along(nuts)) {
    ng <- nuts[i]
    N_h <- n_g[i]
    n_h <- target[i]
    if (n_h <= 0L) { sel_rows[[i]] <- NULL; next }
    sub <- pop[pop$NUTS0_21 == ng, ]
    sub <- sub[order(sub$PRN), ]
    take <- head(sub, n_h)
    take$weight_copernicus <- N_h / n_h
    sel_rows[[i]] <- take[, c("POINT_ID","LC_pred","NUTS0_21","weight_copernicus")]
  }
  sel_meta <- do.call(rbind, sel_rows)
  sel_full <- master_df[match(sel_meta$POINT_ID, master_df$POINT_ID), ]
  sel_full$weight_copernicus <- sel_meta$weight_copernicus
  selected_list[[cat]] <- sel_full
}

copernicus_selected <- do.call(rbind, selected_list)

# ---- Export selection and report diagnostics ----
write.csv(copernicus_selected, "Copernicus2027_sample.csv", row.names = FALSE)

cat("\nRequested by LC_pred (n_add) vs selected:\n")
req <- setNames(as.integer(new_points$n_add), as.character(new_points$LC_pred))
sel <- table(copernicus_selected$LC_pred)
print(data.frame(LC_pred = names(req), requested = as.integer(req),
                 selected = as.integer(sel[names(req)]), row.names = NULL))

cat("\nSelected counts by LC_pred x NUTS0_21:\n")
print(table(copernicus_selected$LC_pred, copernicus_selected$NUTS0_21))

copernicus_selected_b <- NULL
copernicus_selected_b$POINT_ID <- copernicus_selected$POINT_ID
copernicus_selected_b$component <- ""
copernicus_selected_b$STRATUM <- paste(copernicus_selected$LC_pred,copernicus_selected$NUTS0_21,sep="*")
copernicus_selected_b$WGT_LUCAS <- ""
copernicus_selected_b$WGT_comp <- 1
copernicus_selected_b$eligibility_comp <- 1
copernicus_selected_b$LC1 <- copernicus_selected$LC_pred
copernicus_selected_b$wgt_correction <- 1
copernicus_selected_b$wgt_selection <- copernicus_selected$weight_copernicus
copernicus_selected_b <- as.data.frame(copernicus_selected_b)

# Write output and print summaries
write.csv(copernicus_selected_b, "Copernicus2027_sample.csv", row.names = FALSE)

cat("\nRequested by LC_pred (n_add) vs selected:\n")
req <- setNames(as.integer(new_points$n_add), as.character(new_points$LC_pred))
sel <- table(copernicus_selected$LC_pred)
print(data.frame(LC_pred = names(req), requested = as.integer(req),
                 selected = as.integer(sel[names(req)]), row.names = NULL))

cat("\nSelected counts by LC_pred x NUTS0_21:\n")
print(table(copernicus_selected$LC_pred, copernicus_selected$NUTS0_21))
