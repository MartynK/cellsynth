# Shared fixtures (built once per file)
src  <- toy_source(n = 200, seed = 1)
spec <- infer_spec(src)
res  <- toy_synth(src, spec, seed = 11)
res_same <- toy_synth(src, spec, seed = 11)
res_other <- toy_synth(src, spec, seed = 12)

model_cols <- spec$column[spec$role %in% c("numeric", "date", "categorical", "text")]
n_subj <- 200L   # row 201 is the TOTAL row

test_that("synthetic table has the source layout and untouched ids", {
  expect_s3_class(res, "synth_result")
  syn <- res$synthetic
  expect_equal(dim(syn), dim(src$raw))
  expect_equal(names(syn), names(src$raw))
  expect_identical(syn$patientNo, src$raw$patientNo)
  expect_true(all(vapply(syn, is.character, logical(1))))
  expect_equal(res$nonsubject, 201L)
  expect_equal(res$orig_names, src$orig_names)
  expect_equal(res$settings$n_folds, 5)
  expect_output(print(res), "synth_result")
})

test_that("every synthetic cell is a verbatim donor string from its column", {
  syn <- res$synthetic
  for (cl in model_cols) {
    pool <- unique(src$raw[[cl]][seq_len(n_subj)])
    got <- syn[[cl]][seq_len(n_subj)]
    got <- got[!is.na(got)]
    expect_true(all(got %in% pool), info = cl)
  }
  # text column is made of real strings, not generated ones
  expect_true(all(na.omit(syn$notes[1:n_subj]) %in% src$raw$notes))
  # the empty column stays empty
  expect_true(all(is.na(syn$unused)))
})

test_that("number formats and junk strings survive (raw copying)", {
  syn <- res$synthetic[seq_len(n_subj), ]
  # decimal commas and "1.0"-style strings come through unchanged in form
  expect_true(any(grepl(",", syn$weight_kg, fixed = TRUE)))
  expect_true(all(grepl("^[0-9]+([.,][0-9]+)?$", na.omit(syn$weight_kg))))
  expect_true(all(na.omit(syn$code01) %in% c("0.0", "1.0", "1", "0 / 1")))
  # Excel serials and dotted dates are both still present
  expect_true(any(grepl("^[0-9]{5}$", na.omit(syn$visit_date))))
  expect_true(any(grepl("^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.$", na.omit(syn$visit_date))))
})

test_that("non-subject rows are blanked by default, keeping identifiers", {
  syn <- res$synthetic
  tot <- syn[201, ]
  expect_equal(tot$patientNo, "TOTAL")
  others <- setdiff(names(syn), "patientNo")
  expect_true(all(is.na(unlist(tot[others]))))
  # total row is never a donor: its giant weight never appears
  expect_false(src$raw$weight_kg[201] %in% syn$weight_kg[1:n_subj])
})

test_that("nonsubject = 'keep' leaves them verbatim", {
  r <- toy_synth(src, spec, seed = 3, nonsubject = "keep")
  expect_identical(unlist(r$synthetic[201, ]), unlist(src$raw[201, ]))
  expect_equal(r$settings$nonsubject, "keep")
  # and the total is still not used as a donor
  expect_false(src$raw$weight_kg[201] %in% r$synthetic$weight_kg[1:n_subj])
})

test_that("same seed reproduces, different seed differs", {
  expect_identical(res$synthetic, res_same$synthetic)
  expect_false(identical(res$synthetic, res_other$synthetic))
})

test_that("synthesis redraws cells: no row is disclosed verbatim", {
  rd <- row_disclosure(src$raw, res$synthetic, spec, rows = seq_len(n_subj))
  expect_equal(rd$identical, 0)
  expect_lt(rd$near, 0.05)
  # and no free-standing column copies itself wholesale. (The perfectly
  # coupled death flag/date pair is excluded: each pins the other, so the flag
  # comes back unchanged; see the note in the final report.)
  ct <- compare_tables(src$raw, res$synthetic, spec, rows = seq_len(n_subj))
  free <- !ct$column %in% c("death_flag", "death_date")
  expect_true(all(ct$self_match[free] < 0.7))
})

test_that("death flag and death date stay coupled", {
  syn <- res$synthetic[seq_len(n_subj), ]
  flag <- !is.na(syn$death_flag) & syn$death_flag == "1"
  has_date <- !is.na(syn$death_date)
  expect_gte(mean(flag == has_date), 0.9)
  # a different seed too
  syn2 <- res_other$synthetic[seq_len(n_subj), ]
  flag2 <- !is.na(syn2$death_flag) & syn2$death_flag == "1"
  expect_gte(mean(flag2 == !is.na(syn2$death_date)), 0.9)
})

test_that("association between correlated numerics is kept", {
  o <- src$raw[seq_len(n_subj), ]
  s <- res$synthetic[seq_len(n_subj), ]
  r_o <- stats::cor(parse_number_chr(o$weight_kg), parse_number_chr(o$response),
                    use = "complete.obs")
  r_s <- stats::cor(parse_number_chr(s$weight_kg), parse_number_chr(s$response),
                    use = "complete.obs")
  expect_gt(r_o, 0.7)
  expect_gt(r_s, 0.4)
})

test_that("missingness fractions stay close to the original", {
  o <- src$raw[seq_len(n_subj), ]
  s <- res$synthetic[seq_len(n_subj), ]
  miss <- function(z) mean(is.na(z) | trimws(z) == "")
  for (cl in model_cols) {
    expect_lt(abs(miss(o[[cl]]) - miss(s[[cl]])), 0.12, label = cl)
  }
})

test_that("categorical marginals are roughly preserved", {
  ct <- compare_tables(src$raw, res$synthetic, spec, rows = seq_len(n_subj))
  expect_lt(ct$tvd[ct$column == "grp"], 0.3)
})

test_that("multiple cycles and inferred spec work", {
  r <- synthesize_table(src, spec = NULL, n_folds = 4, n_cycles = 2,
                        minbucket = 5, seed = 5, verbose = FALSE)
  expect_equal(r$settings$n_cycles, 2)
  expect_equal(nrow(r$log), 2 * 4 * 8)
  expect_identical(r$synthetic$patientNo, src$raw$patientNo)
})

test_that("input checks fail loudly", {
  expect_error(synthesize_table(src, spec, n_folds = 1, verbose = FALSE))
  expect_error(synthesize_table(list(), spec, verbose = FALSE))
  small <- src
  small$raw <- small$raw[1:15, ]
  expect_error(synthesize_table(small, spec, minbucket = 5, verbose = FALSE),
               "Too few subject rows")
  bad_spec <- spec[-1, ]
  expect_error(synthesize_table(src, bad_spec, verbose = FALSE), "do not match")
})

test_that("a table with no modelled columns passes through", {
  s2 <- spec
  s2$role[s2$role %in% c("numeric", "date", "categorical", "text")] <- "constant"
  r <- toy_synth(src, s2, seed = 1)
  expect_identical(r$synthetic, src$raw)
})

test_that("auxiliary predictors condition the fit but are not written out", {
  set.seed(9)
  n <- 200
  key <- toupper(sprintf("ab-%03d", seq_len(n)))
  lab <- sprintf("%.1f", parse_number_chr(src$raw$response[1:n]) * 2 + rnorm(n))
  aux_raw <- data.frame(key = c(tolower(key), "zz-999"),
                        lab = c(lab, "1.0"), stringsAsFactors = FALSE)
  aux_src <- structure(list(raw = aux_raw, orig_names = names(aux_raw),
                            orig_class = rep("character", 2), path = "aux.csv",
                            sheet = NULL, format = "csv"), class = "synth_source")
  aux_spec <- data.frame(column = c("key", "lab"), orig_name = c("key", "lab"),
                         role = c("id", "numeric"), stringsAsFactors = FALSE)
  aux <- suppressMessages(make_aux(src, "patientNo", aux_src, aux_spec, "key"))
  expect_equal(nrow(aux$raw), nrow(src$raw))
  expect_equal(names(aux$raw), "aux:lab")
  expect_true(is.na(aux$raw[[1]][201]))   # TOTAL has no key partner
  r <- toy_synth(src, spec, seed = 2, aux = aux)
  expect_equal(dim(r$synthetic), dim(src$raw))
  expect_equal(names(r$synthetic), names(src$raw))
  expect_equal(r$settings$aux, "aux:lab")
})
