src  <- toy_source(n = 200, seed = 21)
spec <- infer_spec(src)
res  <- toy_synth(src, spec, seed = 5)
rows <- seq_len(200)

test_that("compare_tables returns one sane row per modelled column", {
  ct <- compare_tables(src$raw, res$synthetic, spec, rows = rows)
  modelled <- spec$column[spec$role %in% c("numeric", "date", "categorical", "text")]
  expect_s3_class(ct, "data.frame")
  expect_equal(ct$column, modelled)
  expect_true(all(c("miss_orig", "miss_syn", "ks", "tvd", "self_match",
                    "chance_match") %in% names(ct)))
  expect_true(all(ct$miss_orig >= 0 & ct$miss_orig <= 1))
  expect_true(all(ct$self_match >= 0 & ct$self_match <= 1))
  num <- ct$role %in% c("numeric", "date")
  expect_true(all(ct$ks[num] >= 0 & ct$ks[num] <= 1))
  expect_true(all(is.na(ct$ks[!num])))
  cat_ <- ct$role == "categorical"
  expect_true(all(ct$tvd[cat_] >= 0 & ct$tvd[cat_] <= 1))
  expect_true(all(!is.na(ct$chance_match[cat_])))
  expect_true(all(is.na(ct$tvd[!cat_])))
})

test_that("comparing a table with itself gives zero distance and full match", {
  ct <- compare_tables(src$raw, src$raw, spec, rows = rows)
  expect_true(all(ct$self_match == 1))
  # a sample against itself: KS distance zero up to floating point
  expect_true(all(ct$ks[ct$role %in% c("numeric", "date")] < 1e-12))
  expect_true(all(ct$tvd[ct$role == "categorical"] == 0))
  expect_equal(ct$miss_orig, ct$miss_syn)
})

test_that("row_disclosure has the documented structure", {
  rd <- row_disclosure(src$raw, res$synthetic, spec, rows = rows)
  expect_named(rd, c("identical", "near", "identical_unique", "per_row"))
  expect_length(rd$per_row, 200)
  expect_true(all(rd$per_row >= 0 & rd$per_row <= 1))
  expect_true(rd$identical >= 0 && rd$identical <= 1)
  expect_gte(rd$near, rd$identical)
  expect_lte(rd$identical_unique, rd$identical)
  full <- row_disclosure(src$raw, src$raw, spec, rows = rows)
  expect_equal(full$identical, 1)
  expect_equal(full$near, 1)
  expect_true(all(full$per_row == 1))
})

test_that("identical_unique ignores identical rows whose pattern is common", {
  # Rows 1-3 share one all-missing pattern; row 4 is unique
  o <- data.frame(id = c("AB1", "AB2", "AB3", "AB4"), x = c(NA, NA, NA, "7"),
                  y = c(NA, NA, NA, "b"), stringsAsFactors = FALSE)
  sp <- data.frame(column = c("id", "x", "y"), role = c("id", "numeric", "categorical"),
                   stringsAsFactors = FALSE)
  rd <- row_disclosure(o, o, sp)
  expect_equal(rd$identical, 1)
  expect_equal(rd$identical_unique, 0.25)
})

test_that("compare_correlations returns symmetric matrices and a small mad", {
  cc <- compare_correlations(src$raw[rows, ], res$synthetic[rows, ], spec)
  expect_named(cc, c("mad", "orig", "syn"))
  expect_true(is.finite(cc$mad))
  expect_gte(cc$mad, 0)
  expect_lt(cc$mad, 0.5)
  expect_equal(dim(cc$orig), dim(cc$syn))
  expect_equal(cc$orig, t(cc$orig))
  expect_equal(unname(diag(cc$orig)), rep(1, nrow(cc$orig)))
  expect_true(all(c("weight_kg", "response") %in% colnames(cc$orig)))
  expect_gt(cc$orig["weight_kg", "response"], 0.5)
  # identity comparison
  same <- compare_correlations(src$raw[rows, ], src$raw[rows, ], spec)
  expect_equal(same$mad, 0)
})

test_that("compare_correlations survives synthesis losing a level of a binary column", {
  set.seed(8)
  n <- 120
  x <- rnorm(n)
  orig <- data.frame(id = sprintf("P%03d", seq_len(n)),
                     bin = sample(c("yes", "no"), n, TRUE),
                     x = sprintf("%.2f", x),
                     y = sprintf("%.2f", x + rnorm(n, 0, 0.5)),
                     stringsAsFactors = FALSE)
  sp <- data.frame(column = names(orig), orig_name = names(orig),
                   role = c("id", "categorical", "numeric", "numeric"),
                   stringsAsFactors = FALSE)
  syn <- orig
  syn$bin <- "yes"                      # the "no" level is lost
  cc <- compare_correlations(orig, syn, sp)
  expect_true(is.finite(cc$mad))
  expect_false("bin" %in% colnames(cc$syn))  # constant column drops out of the synthetic side
  # a level appearing only in the synthetic table is harmless as well
  syn2 <- orig
  syn2$bin[1:3] <- "maybe"
  expect_no_error(cc2 <- compare_correlations(orig, syn2, sp))
  expect_true(is.finite(cc2$mad))
  # and the table-level comparison also copes
  expect_no_error(ct <- compare_tables(orig, syn, sp))
  expect_gt(ct$tvd[ct$column == "bin"], 0.3)
})
