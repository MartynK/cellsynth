make_src <- function(df) {
  structure(list(raw = df, orig_names = names(df),
                 orig_class = rep("character", ncol(df)), path = "x.csv",
                 sheet = NULL, format = "csv"), class = "synth_source")
}

test_that("make_aux matches keys across hyphen and case differences", {
  main <- make_src(data.frame(id = c("AB-001", "ab002", "Ab 003", "AB-004"),
                              v = c("1", "2", "3", "4"), stringsAsFactors = FALSE))
  aux <- make_src(data.frame(key = c("ab001", "AB-002", "AB003", "ZZ-9"),
                             lab = c("10", "20", "30", "40"),
                             stringsAsFactors = FALSE))
  aspec <- data.frame(column = c("key", "lab"), orig_name = c("key", "lab"),
                      role = c("id", "numeric"), stringsAsFactors = FALSE)
  expect_message(a <- make_aux(main, "id", aux, aspec, "key"), "3 of 4")
  expect_equal(a$raw[[1]], c("10", "20", "30", NA))
  expect_equal(names(a$raw), "aux:lab")
  expect_equal(a$roles, "numeric")
})

test_that("NA or blank keys never match", {
  main <- make_src(data.frame(id = c(NA, "", "  ", "AB1", "-"),
                              stringsAsFactors = FALSE))
  aux <- make_src(data.frame(key = c(NA, "", "AB1", NA, "--"),
                             lab = c("a", "b", "c", "d", "e"),
                             stringsAsFactors = FALSE))
  aspec <- data.frame(column = c("key", "lab"), orig_name = c("key", "lab"),
                      role = c("id", "categorical"), stringsAsFactors = FALSE)
  a <- suppressMessages(make_aux(main, "id", aux, aspec, "key"))
  expect_equal(a$raw[[1]], c(NA, NA, NA, "c", NA))
})

test_that("first match wins, columns may be chosen, orig names are accepted", {
  main <- make_src(data.frame(id = c("A1", "A2"), stringsAsFactors = FALSE))
  aux <- make_src(data.frame(key = c("a1", "A1", "a2"), p = c("x", "y", "z"),
                             q = c("1", "2", "3"), stringsAsFactors = FALSE))
  aux$orig_names <- c("Key", "P", "Q")
  aspec <- data.frame(column = c("key", "p", "q"), orig_name = aux$orig_names,
                      role = c("id", "categorical", "numeric"),
                      stringsAsFactors = FALSE)
  a <- suppressMessages(make_aux(main, "id", aux, aspec, "Key", cols = "Q"))
  expect_equal(names(a$raw), "aux:q")
  expect_equal(a$raw[[1]], c("1", "3"))
  # default cols = modelled columns only (id excluded)
  a2 <- suppressMessages(make_aux(main, "id", aux, aspec, "key"))
  expect_equal(names(a2$raw), c("aux:p", "aux:q"))
  expect_equal(a2$raw[["aux:p"]], c("x", "z"))
  expect_error(suppressMessages(make_aux(main, "nope", aux, aspec, "key")),
               "Column not found")
})
