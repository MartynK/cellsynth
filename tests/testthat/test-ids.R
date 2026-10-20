# Identifier detection: trial/EDC key names and code-like values

make_src <- function(df) {
  structure(list(raw = df, orig_names = names(df),
                 orig_class = rep("character", ncol(df)), path = "x.csv",
                 sheet = NULL, format = "csv"), class = "synth_source")
}

test_that("CDISC and EDC key names are ids even when not unique", {
  n <- 60
  df <- data.frame(
    STUDYID = "STUDY01",
    USUBJID = sprintf("STUDY01-%02d-%03d", rep(1:3, each = 20), 1:n),
    SUBJID = sprintf("%03d", 1:n), SITEID = sprintf("%02d", rep(1:3, each = 20)),
    SITE = rep(c("A", "B", "C"), each = 20), PTNO = as.character(1:n),
    RANDNO = sprintf("R%04d", 1:n), odsrecid = as.character(5000 + 1:n),
    AGE = as.character(round(seq(30, 80, length.out = n))),
    stringsAsFactors = FALSE)
  sp <- infer_spec(make_src(df))
  roles <- stats::setNames(sp$role, sp$orig_name)
  expect_true(all(roles[c("STUDYID", "USUBJID", "SUBJID", "SITEID", "SITE",
                          "PTNO", "RANDNO", "odsrecid")] == "id"))
  expect_equal(unname(roles["AGE"]), "numeric")
})

test_that("code patterns are ids under neutral names, dates and decimals are not", {
  set.seed(3)
  n <- 60
  df <- data.frame(
    a = sprintf("TRIAL9-%02d-%03d", rep(1:3, each = 20), 1:n),   # multi-segment
    b = sprintf("%04d", sample(1:9999, n)),                        # zero-padded
    c = format(as.Date("2024-01-01") + 0:(n - 1)),                 # ISO dates
    d = sprintf("%.1f", seq(10, 70, length.out = n)),              # decimals
    e = as.character(1:n),                                         # plain counts
    stringsAsFactors = FALSE)
  df$b[1] <- "0042"
  sp <- infer_spec(make_src(df))
  roles <- stats::setNames(sp$role, sp$orig_name)
  expect_equal(unname(roles[c("a", "b")]), c("id", "id"))
  expect_equal(unname(roles["c"]), "date")
  expect_equal(unname(roles["d"]), "numeric")
  expect_equal(unname(roles["e"]), "numeric")
})
