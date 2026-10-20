test_that("infer_spec assigns the expected roles on the toy table", {
  src <- toy_source()
  spec <- infer_spec(src)
  role <- setNames(spec$role, spec$column)
  expect_equal(nrow(spec), ncol(src$raw))
  expect_equal(spec$column, names(src$raw))
  expect_equal(unname(role["patientNo"]), "id")
  expect_equal(unname(role["weight_kg"]), "numeric")
  expect_equal(unname(role["response"]), "numeric")
  expect_equal(unname(role["code01"]), "categorical")
  expect_equal(unname(role["grp"]), "categorical")
  expect_equal(unname(role["visit_date"]), "date")
  expect_equal(unname(role["death_flag"]), "categorical")
  expect_equal(unname(role["death_date"]), "date")
  expect_equal(unname(role["notes"]), "text")
  expect_equal(unname(role["unused"]), "empty")
  expect_true(all(c("column", "orig_name", "role", "n_obs", "n_unique",
                    "miss_frac", "reason") %in% names(spec)))
})

test_that("infer_spec overrides: id, text, roles", {
  src <- toy_source()
  s1 <- infer_spec(src, text = "grp")
  expect_equal(s1$role[s1$column == "grp"], "text")
  expect_equal(s1$reason[s1$column == "grp"], "user: text")

  s2 <- infer_spec(src, id = c("patientNo", "grp"))
  expect_equal(sum(s2$role == "id"), 2)

  s3 <- infer_spec(src, roles = c(response = "categorical", notes = "constant"))
  expect_equal(s3$role[s3$column == "response"], "categorical")
  expect_equal(s3$role[s3$column == "notes"], "constant")

  expect_warning(infer_spec(src, text = "no_such_column"), "Unknown columns")
})

test_that("Excel serials without a date-like name are not dates", {
  src <- toy_source()
  names(src$raw)[names(src$raw) == "visit_date"] <- "v"
  src$orig_names[src$orig_names == "visit_date"] <- "v"
  spec <- infer_spec(src)
  # mixed ISO/dotted/serial strings: only 2/3 parse without serial support
  expect_false(spec$role[spec$column == "v"] == "date")
})

test_that("validate_spec errors on mismatched columns and bad roles", {
  src <- toy_source()
  spec <- infer_spec(src)
  bad <- spec[-1, ]
  expect_error(cellsynth:::validate_spec(bad, src), "do not match")
  bad2 <- spec
  bad2$column[2] <- "other"
  expect_error(cellsynth:::validate_spec(bad2, src), "do not match")
  bad3 <- spec
  bad3$role[2] <- "banana"
  expect_error(cellsynth:::validate_spec(bad3, src), "Unknown roles")
  no_id <- spec
  no_id$role[no_id$role == "id"] <- "text"
  expect_warning(cellsynth:::validate_spec(no_id, src), "No identifier")
  expect_invisible(cellsynth:::validate_spec(spec, src))
})

test_that("write_spec / read_spec round trip", {
  spec <- infer_spec(toy_source())
  f <- tempfile(fileext = ".csv")
  write_spec(spec, f)
  back <- read_spec(f)
  expect_equal(back$column, spec$column)
  expect_equal(back$role, spec$role)
  unlink(f)
})

test_that("detect_nonsubject_rows flags TOTAL", {
  src <- toy_source()
  spec <- infer_spec(src)
  ns <- detect_nonsubject_rows(src$raw, spec)
  expect_type(ns, "logical")
  expect_length(ns, nrow(src$raw))
  expect_equal(which(ns), nrow(src$raw))
})

test_that("detect_nonsubject_rows flags summary labels and site-name rows", {
  src <- toy_source()
  spec <- infer_spec(src)
  raw <- src$raw
  raw$patientNo[10] <- "Budapest"       # site header: no digit
  raw$patientNo[20] <- "Összesen"
  raw$patientNo[30] <- " sum "
  raw$patientNo[40] <- "Mean"
  ns <- detect_nonsubject_rows(raw, spec)
  expect_true(all(ns[c(10, 20, 30, 40, nrow(raw))]))
  expect_equal(sum(ns), 5)
})

test_that("detect_nonsubject_rows copes with degenerate id columns", {
  src <- toy_source()
  spec <- infer_spec(src)
  raw <- src$raw
  raw$patientNo <- NA_character_
  expect_no_error(ns <- detect_nonsubject_rows(raw, spec))
  expect_false(any(ns))
  # no id column at all
  spec2 <- spec
  spec2$role[spec2$role == "id"] <- "text"
  expect_false(any(detect_nonsubject_rows(src$raw, spec2)))
  # ids without digits where others lack digits too: nothing flagged by rule 2
  raw2 <- src$raw
  raw2$patientNo <- rep_len(c("Alpha", "Beta"), nrow(raw2))
  expect_false(any(detect_nonsubject_rows(raw2, spec)))
})
