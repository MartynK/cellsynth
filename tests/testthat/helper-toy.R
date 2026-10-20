# Toy data for the test suite: built in code, never read from disk.

# A synth_source-like object with n subject rows plus one TOTAL row.
# Columns: patientNo (id), weight_kg (numeric, some decimal commas),
# response (numeric, correlated with weight_kg), code01 (0/1 code with junk),
# grp (4 labels), visit_date (mixed formats and Excel serials),
# death_flag ("1" or NA), death_date (present exactly when death_flag == "1"),
# notes (free text), unused (all NA).
toy_source <- function(n = 200, seed = 1) {
  set.seed(seed)
  ids <- sprintf("AB%03d", seq_len(n))

  x <- round(rnorm(n, 70, 12), 1)
  y <- round(0.8 * x + rnorm(n, 0, 4), 1)
  xs <- sprintf("%.1f", x)
  ys <- sprintf("%.1f", y)
  comma <- sample.int(n, round(0.1 * n))
  xs[comma] <- sub(".", ",", xs[comma], fixed = TRUE)
  xs[sample.int(n, round(0.05 * n))] <- NA
  ys[sample.int(n, round(0.05 * n))] <- NA

  code <- sample(c("0.0", "1.0", "1"), n, replace = TRUE, prob = c(.5, .3, .2))
  code[c(5, 17)] <- "0 / 1"
  code[sample(setdiff(seq_len(n), c(5, 17)), 8)] <- NA

  grp <- sample(c("alpha", "beta", "gamma", "delta"), n, replace = TRUE)
  grp[sample.int(n, 6)] <- NA

  d <- as.Date("2024-01-15") + sample(0:300, n, replace = TRUE)
  kind <- rep_len(1:3, n)
  vd <- ifelse(kind == 1, format(d, "%Y-%m-%d"),
        ifelse(kind == 2, format(d, "%Y.%m.%d."),
               as.character(as.numeric(d) + 25569)))
  vd[sample.int(n, 5)] <- NA

  dead <- runif(n) < 0.2
  flag <- ifelse(dead, "1", NA_character_)
  dd <- ifelse(dead, format(as.Date("2025-01-01") + sample(0:200, n, TRUE),
                            "%Y-%m-%d"), NA_character_)

  notes <- paste0("note ", sample(letters, n, TRUE), " about visit ", seq_len(n))
  notes[sample.int(n, 20)] <- NA

  raw <- data.frame(patientNo = ids, weight_kg = xs, response = ys,
                    code01 = code, grp = grp, visit_date = vd,
                    death_flag = flag, death_date = dd, notes = notes,
                    unused = NA_character_, stringsAsFactors = FALSE)
  total <- raw[1, ]
  total[1, ] <- NA_character_
  total$patientNo <- "TOTAL"
  total$weight_kg <- as.character(sum(x))
  raw <- rbind(raw, total)
  rownames(raw) <- NULL

  structure(list(raw = raw, orig_names = names(raw),
                 orig_class = rep("character", ncol(raw)),
                 path = "toy.csv", sheet = NULL, format = "csv"),
            class = "synth_source")
}

toy_spec <- function(src = toy_source()) {
  infer_spec(src)
}

# Silent wrapper around synthesize_table with fast settings
toy_synth <- function(src, spec, seed = 1, ...) {
  synthesize_table(src, spec, n_folds = 5, minbucket = 5, seed = seed,
                   verbose = FALSE, ...)
}
