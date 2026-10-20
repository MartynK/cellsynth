# Utility and disclosure checks: how close is the synthetic table to the
# original in distribution, and how far from it row by row.

#' Compare original and synthetic tables column by column
#'
#' For every modelled column: missing fraction in both; for numeric and date
#' columns the mean, SD and median and the Kolmogorov-Smirnov distance; for
#' categorical columns the total variation distance between label
#' distributions. `self_match` is the share of subject cells equal to the
#' same row's original cell; for categorical columns `chance_match` gives
#' what a random redraw from the marginal would produce, so a large excess
#' points at a column that copies its own row.
#'
#' @param orig,syn Raw character data.frames in the same layout.
#' @param spec The shared spec.
#' @param rows Rows to compare (default: all).
#' @return A data.frame, one row per modelled column.
#' @export
compare_tables <- function(orig, syn, spec, rows = seq_len(nrow(orig))) {
  cols <- which(spec$role %in% MODEL_ROLES)
  out <- lapply(cols, function(j) {
    o <- orig[[j]][rows]; s <- syn[[j]][rows]
    role <- spec$role[j]
    missing <- function(z) is.na(z) | trimws(z) == ""
    same <- (o == s) | (missing(o) & missing(s))
    same[is.na(same)] <- FALSE
    r <- data.frame(column = spec$column[j], role = role,
                    miss_orig = mean(missing(o)), miss_syn = mean(missing(s)),
                    mean_orig = NA_real_, mean_syn = NA_real_,
                    sd_orig = NA_real_, sd_syn = NA_real_,
                    median_orig = NA_real_, median_syn = NA_real_,
                    ks = NA_real_, tvd = NA_real_,
                    self_match = mean(same),
                    chance_match = NA_real_, stringsAsFactors = FALSE)
    if (role %in% c("numeric", "date")) {
      po <- if (role == "date") parse_date_chr(o, TRUE) else parse_number_chr(o)
      ps <- if (role == "date") parse_date_chr(s, TRUE) else parse_number_chr(s)
      po <- po[!is.na(po)]; ps <- ps[!is.na(ps)]
      if (length(po) > 1 && length(ps) > 1) {
        r$mean_orig <- mean(po); r$mean_syn <- mean(ps)
        r$sd_orig <- stats::sd(po); r$sd_syn <- stats::sd(ps)
        r$median_orig <- stats::median(po); r$median_syn <- stats::median(ps)
        r$ks <- suppressWarnings(stats::ks.test(po, ps)$statistic)
      }
    } else if (role == "categorical") {
      lo <- cat_labels(o); ls <- cat_labels(s)
      lv <- union(lo, ls)
      po <- table(factor(lo, lv)) / length(lo)
      ps <- table(factor(ls, lv)) / length(ls)
      r$tvd <- 0.5 * sum(abs(po - ps))
      r$chance_match <- sum(po * ps)
      r$self_match <- mean(lo == ls)
    }
    r
  })
  do.call(rbind, out)
}

#' Share of synthetic rows identical to their own original row
#'
#' Compares the modelled columns only (identifiers are equal by design).
#'
#' @inheritParams compare_tables
#' @param min_share Report also rows matching at least this share of cells.
#' @return list(identical = share of rows fully identical, identical_unique =
#'   share of rows identical AND unique in the original (the disclosive kind),
#'   near = share of
#'   rows matching at least `min_share` of their cells, per_row = vector).
#' @export
row_disclosure <- function(orig, syn, spec, rows = seq_len(nrow(orig)),
                           min_share = 0.9) {
  cols <- which(spec$role %in% MODEL_ROLES)
  o <- as.matrix(orig[rows, cols]); s <- as.matrix(syn[rows, cols])
  same <- (o == s) | (is.na(o) & is.na(s))
  same[is.na(same)] <- FALSE
  per_row <- rowMeans(same)
  # An identical row only discloses something if its original pattern is
  # rare: an all-empty "no hospitalisation" row matches hundreds of patients
  key <- apply(o, 1, function(r) paste(ifelse(is.na(r), "\001", r), collapse = "\002"))
  unique_orig <- !(duplicated(key) | duplicated(key, fromLast = TRUE))
  list(identical = mean(per_row == 1), near = mean(per_row >= min_share),
       identical_unique = mean(per_row == 1 & unique_orig),
       per_row = per_row)
}

#' Mean absolute difference between correlation matrices
#'
#' Over the numeric, date and binary categorical columns, Spearman
#' correlations on pairwise complete rows.
#'
#' @inheritParams compare_tables
#' @return list(mad = mean absolute difference, orig, syn matrices).
#' @export
compare_correlations <- function(orig, syn, spec) {
  # Column set and binary reference level are decided on the original only,
  # then applied identically to the synthetic table
  ref <- list()
  for (j in seq_along(orig)) {
    role <- spec$role[j]
    if (role %in% c("numeric", "date")) ref[[spec$column[j]]] <- NA
    if (role == "categorical") {
      tab <- sort(table(normalise_label(orig[[j]])), decreasing = TRUE)
      if (length(tab) == 2) ref[[spec$column[j]]] <- names(tab)[1]
    }
  }
  enc <- function(d) {
    out <- lapply(names(ref), function(nm) {
      j <- match(nm, spec$column)
      switch(spec$role[j],
             numeric = parse_number_chr(d[[j]]),
             date = parse_date_chr(d[[j]], TRUE),
             categorical = {
               lab <- normalise_label(d[[j]])
               ifelse(is.na(lab), NA_real_, as.numeric(lab == ref[[nm]]))
             })
    })
    names(out) <- names(ref)
    as.data.frame(out, check.names = FALSE)
  }
  eo <- enc(orig); es <- enc(syn)
  usable <- function(v) sum(!is.na(v)) > 20 && isTRUE(stats::sd(v, na.rm = TRUE) > 0)
  ok <- vapply(seq_along(eo), function(j) usable(eo[[j]]) && usable(es[[j]]), logical(1))
  co <- suppressWarnings(stats::cor(eo[ok], method = "spearman", use = "pairwise.complete.obs"))
  cs <- suppressWarnings(stats::cor(es[ok], method = "spearman", use = "pairwise.complete.obs"))
  d <- abs(co - cs)
  list(mad = mean(d[upper.tri(d)], na.rm = TRUE), orig = co, syn = cs)
}
