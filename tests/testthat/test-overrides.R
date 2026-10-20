# User-supplied identifiers and text columns: names, patterns, options, and
# their effect on reviewed specs read back from disk

ov_df <- function(n = 60) {
  data.frame(PatKey = as.character(1000 + seq_len(n)),        # looks numeric
             hb = as.character(round(seq(110, 160, length.out = n))),
             Visit_Note = rep(c("seen", "phoned", "no show"), length.out = n),
             stringsAsFactors = FALSE)
}

test_that("id and text names are matched case-insensitively", {
  src <- structure(list(raw = ov_df(), orig_names = names(ov_df()),
                        orig_class = rep("character", 3), path = "x.csv",
                        sheet = NULL, format = "csv"), class = "synth_source")
  sp <- infer_spec(src, id = "patkey", text = "VISIT_NOTE")
  expect_equal(sp$role, c("id", "numeric", "text"))
  expect_equal(sp$reason[1], "user: id")
})

test_that("patterns and session options force roles", {
  src <- structure(list(raw = ov_df(), orig_names = names(ov_df()),
                        orig_class = rep("character", 3), path = "x.csv",
                        sheet = NULL, format = "csv"), class = "synth_source")
  sp <- infer_spec(src, id_pattern = "key$", text_pattern = "note")
  expect_equal(sp$role, c("id", "numeric", "text"))
  withr::local_options(cellsynth.id = "PatKey")
  sp2 <- suppressWarnings(infer_spec(src))
  expect_equal(sp2$role[1], "id")
})

test_that("an id named in synthesize_file() overrides a reviewed spec", {
  dir <- tempfile("ov_"); dir.create(dir)
  path <- file.path(dir, "ov.csv")
  utils::write.csv(ov_df(), path, row.names = FALSE)
  # first run without the override: PatKey is synthesised as a number
  r1 <- suppressWarnings(synthesize_file(path, out_dir = dir, n_folds = 2, verbose = FALSE))
  expect_equal(r1$results[[1]][[1]]$spec$role[1], "numeric")
  # second run reuses the spec file but names the identifier: kept as is
  r2 <- synthesize_file(path, out_dir = dir, id = "patkey", n_folds = 2, verbose = FALSE)
  expect_equal(r2$results[[1]][[1]]$spec$role[1], "id")
  out <- utils::read.csv(r2$files[1], colClasses = "character")
  expect_identical(out$PatKey, ov_df()$PatKey)
})

test_that("an id name that matches no column warns once", {
  dir <- tempfile("ov_"); dir.create(dir)
  path <- file.path(dir, "ov.csv")
  utils::write.csv(ov_df(), path, row.names = FALSE)
  expect_warning(synthesize_file(path, out_dir = dir, id = c("PatKey", "NoSuchCol"),
                                 n_folds = 2, verbose = FALSE),
                 "match no column in any sheet: NoSuchCol")
})

test_that("seeded synthesis leaves the global random stream unchanged", {
  src <- structure(list(raw = ov_df(), orig_names = names(ov_df()),
                        orig_class = rep("character", 3), path = "x.csv",
                        sheet = NULL, format = "csv"), class = "synth_source")
  sp <- infer_spec(src, id = "PatKey")
  set.seed(123); a <- stats::runif(1)
  set.seed(123); invisible(synthesize_table(src, sp, n_folds = 2, seed = 7, verbose = FALSE))
  b <- stats::runif(1)
  expect_identical(a, b)
})
