# Block-wise predictor screening, lock pairs and lock groups, and a wide-sheet
# smoke test of the whole engine.

screen <- SyntheticData:::screen_predictors
lock_groups <- SyntheticData:::lock_groups

# Modelled part of the toy table (subject rows only) with roles
toy_screen_input <- function() {
  src <- toy_source(150, seed = 7)
  sp <- infer_spec(src)
  ns <- detect_nonsubject_rows(src$raw, sp)
  tg <- which(sp$role %in% MODEL_ROLES)
  list(raw = src$raw[!ns, tg, drop = FALSE], roles = sp$role[tg])
}

test_that("a deterministic flag/date pair shows up in lock_pairs", {
  x <- toy_screen_input()
  nm <- names(x$raw)
  pf <- match("death_flag", nm)
  pd <- match("death_date", nm)
  expect_false(anyNA(c(pf, pd)))
  # the construction really is deterministic in the data
  expect_identical(!is.na(x$raw$death_flag), !is.na(x$raw$death_date))
  sel <- screen(x$raw, x$roles, seq_along(nm))
  lp <- attr(sel, "lock_pairs")
  expect_true(is.matrix(lp))
  expect_equal(ncol(lp), 2L)
  expect_true(any(lp[, 1] == pf & lp[, 2] == pd))
  # and, of course, they end up in one lock group
  lg <- lock_groups(lp, length(nm))
  expect_equal(lg[pf], lg[pd])
  # an independent column is not locked to them
  expect_false(lg[match("grp", nm)] == lg[pf])
})

test_that("pairs are reported as positions within `targets`", {
  x <- toy_screen_input()
  nm <- names(x$raw)
  pf <- match("death_flag", nm)
  pd <- match("death_date", nm)
  tg <- c(pd, match("grp", nm), pf)          # reversed, subset
  sel <- screen(x$raw, x$roles, tg)
  expect_length(sel, 3)
  lp <- attr(sel, "lock_pairs")
  expect_true(any(apply(lp, 1, function(r) setequal(r, c(1, 3)))))
  expect_true(all(lp %in% 1:3))
  # candidates are still indexes into the full raw table
  expect_true(all(unlist(sel) %in% seq_along(nm)))
})

test_that("candidate lists never contain the target itself, and are well formed", {
  x <- toy_screen_input()
  G <- ncol(x$raw)
  sel <- screen(x$raw, x$roles, seq_len(G))
  expect_length(sel, G)
  for (j in seq_len(G)) {
    expect_false(j %in% sel[[j]], info = names(x$raw)[j])
    expect_equal(anyDuplicated(sel[[j]]), 0L)
    expect_true(all(sel[[j]] >= 1 & sel[[j]] <= G))
  }
  # the true partner of the date is its flag, ranked first
  pf <- match("death_flag", names(x$raw))
  pd <- match("death_date", names(x$raw))
  expect_equal(sel[[pd]][1], pf)
  expect_equal(sel[[pf]][1], pd)
  # weight and response are strongly related
  pw <- match("weight_kg", names(x$raw))
  pr <- match("response", names(x$raw))
  expect_equal(sel[[pr]][1], pw)
})

test_that("`keep` truncates to the head of the full ranking", {
  x <- toy_screen_input()
  G <- ncol(x$raw)
  full <- screen(x$raw, x$roles, seq_len(G), keep = 100)
  short <- screen(x$raw, x$roles, seq_len(G), keep = 3)
  for (j in seq_len(G)) {
    expect_lte(length(short[[j]]), 3)
    expect_equal(short[[j]], utils::head(full[[j]], 3))
  }
  expect_equal(attr(short, "lock_pairs"), attr(full, "lock_pairs"))
})

test_that("constant and all-missing inputs give empty candidate lists", {
  raw <- data.frame(a = rep("1", 20), b = NA_character_, c = rep(c("x", "y"), 10),
                    stringsAsFactors = FALSE)
  roles <- c("categorical", "categorical", "categorical")
  sel <- screen(raw, roles, 1:3)
  expect_length(sel, 3)
  expect_equal(sel[[1]], integer(0))
  expect_equal(sel[[3]], integer(0))
  expect_equal(nrow(attr(sel, "lock_pairs")), 0L)
  # nothing varies at all: the early return still carries the attribute
  raw0 <- data.frame(a = rep("1", 10), b = rep("2", 10), stringsAsFactors = FALSE)
  sel0 <- screen(raw0, c("categorical", "categorical"), 1:2)
  expect_equal(sel0[[1]], integer(0))
  expect_equal(dim(attr(sel0, "lock_pairs")), c(0L, 2L))
})

test_that("lock_groups builds connected components", {
  expect_equal(lock_groups(NULL, 4), 1:4)
  expect_equal(lock_groups(matrix(integer(0), 0, 2), 4), 1:4)
  expect_equal(lock_groups(cbind(1L, 2L), 1), 1L)
  expect_equal(lock_groups(rbind(c(1L, 2L), c(2L, 3L), c(5L, 6L)), 6), c(1, 1, 1, 4, 5, 5))
  # components joined late, in any orientation
  expect_equal(lock_groups(rbind(c(1L, 2L), c(3L, 4L), c(2L, 4L)), 4), c(1, 1, 1, 1))
  expect_equal(lock_groups(rbind(c(4L, 3L), c(3L, 1L)), 4), c(1, 2, 1, 1))
})

test_that("blocking does not change the result (wide sparse table)", {
  raw <- sparse_binary_table(60, 800)
  # a planted coupled pair, columns 5 and 7
  raw[[7]] <- raw[[5]]
  roles <- rep("categorical", ncol(raw))
  tg <- seq_len(ncol(raw))
  many <- screen(raw, roles, tg, block_cells = 2e5)       # dozens of blocks
  one <- screen(raw, roles, tg, block_cells = 1e9)        # a single block
  expect_equal(many, one)
  expect_equal(attr(many, "lock_pairs"), attr(one, "lock_pairs"))
  for (lp in list(attr(many, "lock_pairs"), attr(one, "lock_pairs"))) {
    expect_true(any(lp[, 1] == 5 & lp[, 2] == 7))
  }
  expect_equal(many[[5]][1], 7)
  expect_equal(many[[7]][1], 5)
  for (j in c(1, 5, 400, 800)) expect_false(j %in% many[[j]])
  expect_true(all(lengths(many) <= 100))
  # block size smaller than one column's signals still works
  tiny <- screen(raw[1:40], roles[1:40], 1:40, block_cells = 1)
  expect_equal(tiny, screen(raw[1:40], roles[1:40], 1:40, block_cells = 1e9))
})

test_that("a 3,000-column sparse sheet screens in small blocks", {
  raw <- sparse_binary_table(60, 3000, seed = 9)
  raw[[2500]] <- raw[[11]]
  roles <- rep("categorical", ncol(raw))
  tm <- system.time(sel <- screen(raw, roles, seq_len(3000), block_cells = 2e5))
  expect_length(sel, 3000)
  expect_true(all(lengths(sel) <= 100))
  expect_false(any(vapply(seq_along(sel), function(j) j %in% sel[[j]], logical(1))))
  lp <- attr(sel, "lock_pairs")
  expect_true(any(lp[, 1] == 11 & lp[, 2] == 2500))
  expect_lt(tm[["elapsed"]], 120)
})

test_that("screening accepts a subset of targets with auxiliary-like extra columns", {
  # columns beyond the targets (aux in synthesize_table) can be candidates
  raw <- sparse_binary_table(60, 30, seed = 4)
  raw[[30]] <- raw[[1]]                       # column 30 mirrors target 1
  roles <- rep("categorical", 30)
  sel <- screen(raw, roles, 1:5)
  expect_length(sel, 5)
  expect_equal(sel[[1]][1], 30)
  # lock pairs only among targets
  lp <- attr(sel, "lock_pairs")
  expect_true(all(lp %in% 1:5))
})

# ---- the whole engine on a wide sheet ----------------------------------------

test_that("a 40 x 1,500 sparse mixed sheet synthesises in reasonable time", {
  skip_if(nzchar(Sys.getenv("SYNTH_SKIP_SLOW")), "SYNTH_SKIP_SLOW set")
  set.seed(41)
  n <- 40
  p <- 1500
  cols <- vector("list", p)
  for (j in seq_len(p)) {
    u <- stats::runif(n)
    cols[[j]] <- if (j %% 3 == 0) {
      # sparse categorical code: mostly missing, a few 0/1/2
      ifelse(u < 0.25, as.character(sample(0:2, n, TRUE)), NA_character_)
    } else if (j %% 3 == 1) {
      # sparse numeric: ~40% present, one decimal
      ifelse(u < 0.4, sprintf("%.1f", stats::rnorm(n, 20, 6)), NA_character_)
    } else {
      # dense numeric
      sprintf("%.1f", stats::rnorm(n, 100, 15))
    }
  }
  names(cols) <- sprintf("m%04d", seq_len(p))
  raw <- cbind(data.frame(patientNo = sprintf("W%03d", seq_len(n)),
                          stringsAsFactors = FALSE),
               as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE))
  src <- structure(list(raw = raw, orig_names = names(raw),
                        orig_class = rep("character", ncol(raw)),
                        path = "wide.csv", sheet = NULL, format = "csv"),
                   class = "synth_source")
  sp <- infer_spec(src)
  expect_true(all(c("numeric", "categorical") %in% sp$role))
  expect_equal(sp$role[1], "id")

  tm <- system.time(
    res <- synthesize_table(src, sp, n_folds = 2, seed = 3, verbose = FALSE))
  expect_s3_class(res, "synth_result")
  expect_equal(dim(res$synthetic), dim(raw))
  expect_identical(names(res$synthetic), names(raw))
  expect_identical(res$synthetic$patientNo, raw$patientNo)
  # values are borrowed from their own column
  for (j in c(2, 3, 4, 700, 1500)) {
    expect_true(all(stats::na.omit(res$synthetic[[j]]) %in% stats::na.omit(raw[[j]])),
                info = names(raw)[j])
  }
  expect_false(identical(res$synthetic, raw))
  # Target is < 60 s on an idle machine. Measured 90 to 130 s while four other
  # heavy R jobs were running (time goes to rpart fits and predict() calls),
  # so the hard bound here is loose.
  expect_lt(tm[["elapsed"]], 240)
})
