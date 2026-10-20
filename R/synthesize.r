# The synthesis engine: rolling censor-and-impute with CART leaf donors.
#
# Scheme. Every modelled column is split into `n_folds` random folds of rows.
# In round k the fold-k cells of every column are censored and redrawn, column
# by column in random order, from a model fitted on the uncensored rows. After
# all folds every cell has been redrawn once; `n_cycles` repeats the sweep
# with fresh folds. `censor_mode` sets what a redrawn cell may condition on:
# the rest of its own row ("cell") or only columns already rebuilt this round
# ("row", fully synthetic rows). See synthesize_table().
#
# Missingness is synthesised, not kept. For numeric, date and text columns a
# first-stage tree draws the cell state (value / missing / junk, where junk is
# a cell that does not parse under the column's type); a second-stage tree
# draws the value for cells that came out as values. Categorical columns treat
# "(NA)" as one more level, in a single stage.
#
# Values are never generated, only borrowed: each censored cell copies the raw
# string of a donor row drawn from its CART leaf (as in mice's "cart" method
# and synthpop). Number formats, decimal places, Excel serials and data-entry
# quirks therefore survive, and a leaf of at least `minbucket` rows means each
# synthetic value is one of at least that many plausible donors. A censored
# cell can never be its own donor.
#
# Text columns get the missingness stage; their values are drawn at random
# from the column's observed pool (a permutation in spirit). Real strings
# survive, so free text is NOT anonymised by this package.

MODEL_ROLES <- c("numeric", "date", "categorical", "text")

# ---- encoding ----------------------------------------------------------------

# Cell state of a raw column under its role: value / missing / junk
cell_state <- function(x, role) {
  miss <- is.na(x) | trimws(x) == ""
  parsed <- switch(role,
                   numeric = parse_number_chr(x),
                   date = parse_date_chr(x, allow_serial = TRUE),
                   NULL)
  st <- rep("value", length(x))
  if (!is.null(parsed)) st[!miss & is.na(parsed)] <- "junk"
  st[miss] <- "missing"
  list(state = factor(st, levels = c("value", "missing", "junk")), parsed = parsed)
}

# Modelling labels of a categorical column, "(NA)" for missing
cat_labels <- function(x) {
  lab <- normalise_label(x)
  lab[is.na(x) | trimws(x) == ""] <- "(NA)"
  lab
}

# Keep the `k` most frequent levels, lump the rest (predictor use only:
# rpart's search over a many-level factor is exponential)
lump_levels <- function(lab, k = 15) {
  tab <- sort(table(lab), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(k, length(tab)))]
  lab[!lab %in% keep] <- "(other)"
  factor(lab)
}

#' Build the predictor frame for one round
#'
#' @param raw Character data.frame holding the columns to encode.
#' @param roles Role per column of `raw`.
#' @param censor Optional list of logical vectors (one per column of `raw`,
#'   `NULL` for never censored); censored cells become `NA` (unknown), which
#'   rpart routes through surrogate splits. Observed missingness stays visible
#'   as a level of its own.
#' @return list(P = data.frame with syntactic names, group = index into `raw`
#'   columns for every column of `P`).
#' @keywords internal
build_predictors <- function(raw, roles, censor = NULL, lump = 15) {
  cols <- list()
  group <- integer(0)
  for (g in seq_along(raw)) {
    cz <- if (!is.null(censor)) censor[[g]] else NULL
    enc <- encode_group(raw[[g]], roles[g], cz, lump)
    cols <- c(cols, enc)
    group <- c(group, rep(g, length(enc)))
  }
  P <- if (length(cols)) {
    as.data.frame(cols, stringsAsFactors = FALSE)
  } else {
    data.frame(row.names = seq_len(nrow(raw)))
  }
  names(P) <- if (length(cols)) paste0("p", seq_along(cols)) else character()
  list(P = P, group = group, lump = lump)
}

# Predictor columns of one raw column: value and/or state for numeric, date
# and text; a lumped factor for categorical. Censored cells become NA.
encode_group <- function(x, role, cz = NULL, lump = 15, force_state = FALSE) {
  out <- list()
  if (role %in% c("numeric", "date", "text")) {
    cs <- cell_state(x, role)
    if (role != "text") {
      v <- cs$parsed
      if (!is.null(cz)) v[cz] <- NA
      out[[length(out) + 1]] <- v
    }
    if (force_state || length(unique(cs$state)) > 1) {
      st <- droplevels(cs$state)
      if (!is.null(cz)) st[cz] <- NA
      out[[length(out) + 1]] <- st
    }
  } else if (role == "categorical") {
    f <- lump_levels(cat_labels(x), lump)
    if (!is.null(cz)) f[cz] <- NA
    out[[length(out) + 1]] <- f
  }
  out
}

# Replace the predictor columns of group `g` after it has been updated, so
# later columns in the same round condition on the fresh synthetic values
# (sequential, synthpop-style) instead of on censored holes. The number of
# encoded columns per group can change (a state column appears or vanishes),
# so the frame is re-spliced.
refresh_group <- function(bp, g, x, role) {
  # Keep the encoded width fixed (state column always produced, then cut to
  # the original width), so the update is always in place
  enc <- encode_group(x, role, NULL, bp$lump, force_state = TRUE)
  old <- which(bp$group == g)
  if (length(enc) > length(old)) enc <- enc[seq_along(old)]
  if (length(enc) == length(old)) {
    # Usual case: same encoded width, replace in place (cheap on wide tables)
    for (i in seq_along(old)) bp$P[[old[i]]] <- enc[[i]]
    return(bp)
  }
  keep <- setdiff(seq_along(bp$group), old)
  cols <- c(as.list(bp$P[keep]), enc)
  group <- c(bp$group[keep], rep(g, length(enc)))
  P <- if (length(cols)) as.data.frame(cols, stringsAsFactors = FALSE) else
    data.frame(row.names = seq_len(nrow(bp$P)))
  names(P) <- if (length(cols)) paste0("p", seq_along(cols)) else character()
  list(P = P, group = group, lump = bp$lump)
}

# ---- predictor screening -----------------------------------------------------

# Numeric screening signals per column: values, missingness, top-level dummies
screening_signals <- function(raw, roles) {
  sig <- list()
  grp <- integer(0)
  add <- function(v, g) {
    v <- as.numeric(v)
    if (stats::sd(v, na.rm = TRUE) %in% c(0, NA)) return(invisible())
    v[is.na(v)] <- stats::median(v, na.rm = TRUE)
    sig[[length(sig) + 1]] <<- v
    grp <<- c(grp, g)
  }
  for (g in seq_along(raw)) {
    x <- raw[[g]]
    role <- roles[g]
    if (role %in% c("numeric", "date", "text")) {
      cs <- cell_state(x, role)
      if (role != "text") add(cs$parsed, g)
      add(cs$state != "value", g)
    } else if (role == "categorical") {
      lab <- cat_labels(x)
      tab <- sort(table(lab), decreasing = TRUE)
      add(match(lab, names(tab)), g)
      add(lab == "(NA)", g)
      for (lv in utils::head(setdiff(names(tab), "(NA)"), 3)) add(lab == lv, g)
    }
  }
  list(S = do.call(cbind, sig), group = grp)
}

#' Choose predictors for every target column by association screening
#'
#' Association between two columns is the largest absolute Pearson
#' correlation between any of their screening signals (values, missingness
#' indicators, level dummies). Screening runs once, on the original table.
#'
#' Memory use is bounded for very wide tables: the column x column score is
#' computed in blocks of columns and only each target's `keep` best
#' candidates and the near-deterministic pairs are retained, never the full
#' matrix (a 9,000-column sheet would need about 0.7 GB for it).
#'
#' @param raw Character data.frame of all candidate columns.
#' @param roles Roles of `raw` columns.
#' @param targets Indices of target columns.
#' @param keep Candidates kept per target, best first (truncated to
#'   `max_predictors` at use; extra ones serve row mode and lock blocking).
#' @param lock_threshold Score at or above which two targets form a lock pair.
#' @param block_cells Size budget (in matrix cells) of one block.
#' @return List, for each target, the indices of its candidate predictor
#'   columns, best first; attribute `lock_pairs` is a two-column matrix of
#'   target index pairs with score >= `lock_threshold`.
#' @keywords internal
screen_predictors <- function(raw, roles, targets, keep = 100,
                              lock_threshold = 0.95, block_cells = 2e7) {
  ss <- screening_signals(raw, roles)
  G <- ncol(raw)
  empty <- lapply(targets, function(j) integer(0))
  attr(empty, "lock_pairs") <- matrix(integer(0), 0, 2)
  if (is.null(ss$S)) return(empty)
  n <- nrow(ss$S)
  Z <- scale(ss$S)
  Z[!is.finite(Z)] <- 0
  # Signals were added column by column, so each group's signals are a
  # contiguous run: group maxima become a pmax over at most a few slices
  grp <- ss$group
  ug <- unique(grp)
  starts <- match(ug, grp)
  lens <- tabulate(match(grp, ug))
  maxlen <- max(lens)
  group_max_rows <- function(M) {          # signals x b -> groups(ug) x b
    acc <- M[starts, , drop = FALSE]
    for (k in seq_len(maxlen - 1)) {
      acc <- pmax(acc, M[starts + pmin(k, lens - 1), , drop = FALSE])
    }
    acc
  }
  is_target <- seq_len(G) %in% targets
  out <- vector("list", G)
  pairs <- list()
  # Blocks of whole groups, about block_cells / s signals wide
  width <- max(1, floor(block_cells / ncol(Z)))
  gblocks <- split(seq_along(ug), cumsum(lens) %/% width)
  for (gb in gblocks) {
    sig <- unlist(lapply(gb, function(i) starts[i] + seq_len(lens[i]) - 1L))
    C <- abs(crossprod(Z, Z[, sig, drop = FALSE])) / max(1, n - 1)
    R <- group_max_rows(C)                               # groups x signals(sig)
    # columns of the block collapse to groups too
    lstarts <- match(gb, rep(gb, lens[gb]))
    llens <- lens[gb]
    S2 <- R[, lstarts, drop = FALSE]
    for (k in seq_len(max(llens) - 1)) {
      S2 <- pmax(S2, R[, lstarts + pmin(k, llens - 1), drop = FALSE])
    }
    # S2: groups(ug) x groups(gb); one column per block group
    for (c in seq_along(gb)) {
      g <- ug[gb[c]]
      if (!is_target[g]) next
      sc <- S2[, c]
      sc[ug == g] <- -1
      ord <- order(sc, decreasing = TRUE)
      ord <- ord[sc[ord] > 0]
      out[[g]] <- ug[utils::head(ord, keep)]
      hit <- ug[sc >= lock_threshold]
      hit <- hit[is_target[hit] & hit > g]
      if (length(hit)) pairs[[length(pairs) + 1]] <- cbind(g, hit)
    }
  }
  res <- lapply(targets, function(j) if (is.null(out[[j]])) integer(0) else out[[j]])
  lp <- if (length(pairs)) do.call(rbind, pairs) else matrix(integer(0), 0, 2)
  # report pairs as target positions
  lp <- cbind(match(lp[, 1], targets), match(lp[, 2], targets))
  attr(res, "lock_pairs") <- lp[stats::complete.cases(lp), , drop = FALSE]
  res
}

# Groups of target columns that (nearly) determine each other: connected
# components of the graph "screening association >= threshold". Members of a
# group share a fold so the group is re-rolled jointly; otherwise each member
# would be redrawn given the others and could never change.
lock_groups <- function(pairs, m) {
  grp <- seq_len(m)
  if (is.null(pairs) || !nrow(pairs) || m < 2) return(grp)
  storage.mode(pairs) <- "integer"
  find <- function(i) { while (grp[i] != i) i <- grp[i]; i }
  for (r in seq_len(nrow(pairs))) {
    a <- find(pairs[r, 1]); b <- find(pairs[r, 2])
    if (a != b) grp[max(a, b)] <- min(a, b)
  }
  vapply(seq_len(m), find, integer(1))
}

# ---- leaf donors -------------------------------------------------------------

#' Draw donor rows from CART leaves
#'
#' Fits `y ~ X` on the training rows and, for every row to predict, returns a
#' training row drawn at random from the same leaf.
#'
#' @param y Target (factor or numeric), full length.
#' @param X Predictor data.frame, full length.
#' @param train,pred Logical vectors: rows to fit on and rows to draw for.
#' @param minbucket,cp rpart controls; leaves hold at least `minbucket` rows.
#' @return list(donor = row indices into `y`, leaves, fallback flag).
#' @keywords internal
leaf_donors <- function(y, X, train, pred, minbucket = 5, cp = 1e-4) {
  idx_tr <- which(train)
  idx_pr <- which(pred)
  if (!length(idx_pr)) return(list(donor = integer(0), leaves = 0L, fallback = FALSE))
  if (!length(idx_tr)) stop("No training rows to draw donors from.")
  marginal <- function() {
    list(donor = idx_tr[sample.int(length(idx_tr), length(idx_pr), replace = TRUE)],
         leaves = 1L, fallback = TRUE)
  }
  ytr <- y[idx_tr]
  if (is.factor(ytr)) ytr <- droplevels(ytr)
  if (length(idx_tr) < 2 * minbucket || ncol(X) == 0 || length(unique(ytr)) < 2)
    return(marginal())

  d <- X[idx_tr, , drop = FALSE]
  d$.y <- ytr
  method <- if (is.factor(ytr)) "class" else "anova"
  # na.pass: the default na.rpart drops rows whose predictors are all missing
  # (common when the only predictor is censored), which would misalign
  # fit$where with the training rows
  fit <- tryCatch(
    rpart::rpart(.y ~ ., data = d, method = method, na.action = stats::na.pass,
                 control = rpart::rpart.control(minbucket = minbucket, cp = cp,
                                                xval = 0, maxcompete = 0,
                                                maxsurrogate = 3)),
    error = function(e) NULL)
  if (is.null(fit)) return(marginal())

  node_ids <- as.integer(rownames(fit$frame))
  # Trick from mice.impute.cart: make predict() return the leaf number.
  # Training leaves come from the same predict() call as the new rows, so
  # both are routed identically and always aligned with idx_tr.
  fit$frame$yval <- node_ids
  # With na.pass every training row is kept, so fit$where is aligned with
  # idx_tr; fall back to predict() if rpart ever drops a row
  leaf_tr <- if (length(fit$where) == length(idx_tr)) node_ids[fit$where] else
    as.integer(stats::predict(fit, newdata = X[idx_tr, , drop = FALSE], type = "vector"))
  leaf_pr <- as.integer(stats::predict(fit, newdata = X[idx_pr, , drop = FALSE],
                                       type = "vector"))
  pools <- split(idx_tr, leaf_tr)
  donor <- vapply(as.character(leaf_pr), function(l) {
    p <- pools[[l]]
    if (is.null(p)) p <- idx_tr
    p[sample.int(length(p), 1)]
  }, integer(1))
  list(donor = unname(donor), leaves = length(pools), fallback = FALSE)
}

# ---- auxiliary predictors ----------------------------------------------------

#' Align columns of another table as auxiliary predictors
#'
#' Auxiliary columns condition the synthesis (so cross-table associations
#' survive a later join) but are never censored or written out.
#'
#' @param src The table being synthesised (`synth_source`).
#' @param by Key column of `src` (internal or on-disk name).
#' @param aux_src The other table (`synth_source`).
#' @param aux_spec Spec of `aux_src`.
#' @param aux_by Key column of `aux_src`.
#' @param cols Columns of `aux_src` to use (default: all modelled ones).
#' @param key_fun Function normalising keys before matching (e.g. dropping a
#'   hyphen so `AB-123` matches `AB123`).
#' @return list(raw, roles), rows aligned to `src$raw` (first match per key).
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60)
#' other <- data.frame(id = rev(df$id), code = rep(c(0, 1, NA), 20))
#' path <- file.path(tempdir(), "make_aux_main.csv")
#' aux_path <- file.path(tempdir(), "make_aux_other.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' utils::write.csv(other, aux_path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' aux_src <- read_source(aux_path)
#' aux_spec <- infer_spec(aux_src)
#' aux <- make_aux(src, by = "id", aux_src = aux_src,
#'                 aux_spec = aux_spec, aux_by = "id", cols = "code")
#' head(aux$raw)
#' @export
make_aux <- function(src, by, aux_src, aux_spec, aux_by, cols = NULL,
                     key_fun = function(k) toupper(gsub("[^A-Za-z0-9]", "", k))) {
  col_of <- function(s, nm) {
    j <- match(nm, names(s$raw))
    if (is.na(j)) j <- match(nm, s$orig_names)
    if (is.na(j)) stop("Column not found: ", nm)
    j
  }
  k_main <- key_fun(src$raw[[col_of(src, by)]])
  k_aux <- key_fun(aux_src$raw[[col_of(aux_src, aux_by)]])
  if (is.null(cols)) cols <- aux_spec$column[aux_spec$role %in% MODEL_ROLES]
  jj <- vapply(cols, function(nm) col_of(aux_src, nm), integer(1))
  # Missing or blank keys never match (match(NA, NA) would)
  valid_aux <- which(!is.na(k_aux) & nzchar(k_aux))
  rows <- valid_aux[match(k_main, k_aux[valid_aux])]
  rows[is.na(k_main) | !nzchar(k_main)] <- NA_integer_
  raw <- aux_src$raw[rows, jj, drop = FALSE]
  names(raw) <- paste0("aux:", names(aux_src$raw)[jj])
  rownames(raw) <- NULL
  message(sprintf("make_aux: %d of %d rows matched", sum(!is.na(rows)), length(rows)))
  list(raw = raw, roles = aux_spec$role[jj])
}

# ---- main --------------------------------------------------------------------

#' Synthesise one table
#'
#' @param src A `synth_source` from [read_source()].
#' @param spec Column spec; inferred with [infer_spec()] when `NULL`.
#' @param n_folds Number of folds; each round censors 1/`n_folds` of every
#'   column (default 10, i.e. 10%).
#' @param n_cycles Full sweeps over all folds.
#' @param censor_mode The privacy-utility dial.
#'   * `"cell"` (default): folds are drawn per column; each censored cell is
#'     redrawn given the row's other current values. Within-row and
#'     cross-table associations survive well, but a pair of columns that
#'     determine each other (death flag and death date) cannot change, so
#'     such attributes stay real per identifier.
#'   * `"row"`: whole rows are censored together and rebuilt column by
#'     column from already redrawn columns and `aux` only (sequential
#'     synthesis as in synthpop). Rows become fully synthetic; their link to
#'     the identifier survives only through `aux`.
#' @param lock_threshold Cell mode only: columns whose screening association
#'   (largest absolute correlation between value, missingness and level
#'   signals) reaches this value form a lock group, share a fold and are
#'   redrawn in sequence without seeing each other's censored cells. This
#'   re-rolls coupled pairs (flag + date) that would otherwise stay real.
#'   `NULL` switches it off.
#' @param max_predictors Predictors per target after screening.
#' @param minbucket,cp rpart controls (minimum donor pool per leaf, complexity).
#' @param aux Optional auxiliary predictors from [make_aux()].
#' @param aux_always If `TRUE` (default) every aux column is offered to every
#'   target's tree in addition to the `max_predictors` screened ones; if
#'   `FALSE` aux columns compete in the screening like any other column.
#' @param nonsubject What to do with non-subject rows (totals, site headers):
#'   `"blank"` keeps their identifiers and empties the rest, `"keep"` leaves
#'   them verbatim. They are never donors.
#' @param seed Random seed.
#' @param verbose Print progress.
#' @return A `synth_result`: `synthetic` (raw character data.frame in the
#'   source layout), `spec`, `log`, `nonsubject` rows, `settings`, and the
#'   source metadata needed to write it out.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60,
#'                  code = rep(c(0, 1, NA), 20), group = rep(c("A", "B"), 30))
#' path <- file.path(tempdir(), "synthesize_table.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' res <- synthesize_table(src, n_folds = 2, seed = 1, verbose = FALSE)
#' head(res$synthetic)
#' @export
synthesize_table <- function(src, spec = NULL, n_folds = 10, n_cycles = 1,
                             censor_mode = c("cell", "row"),
                             lock_threshold = 0.95,
                             max_predictors = 25, minbucket = 5, cp = 1e-4,
                             aux = NULL, aux_always = TRUE,
                             nonsubject = c("blank", "keep"),
                             seed = NULL, verbose = TRUE) {
  stopifnot(inherits(src, "synth_source"), n_folds >= 2, n_cycles >= 1)
  nonsubject <- match.arg(nonsubject)
  censor_mode <- match.arg(censor_mode)
  # Seeded runs must not leave the caller's random stream changed (CRAN policy)
  if (!is.null(seed)) withr::local_seed(seed)
  if (is.null(spec)) spec <- infer_spec(src)
  validate_spec(spec, src)
  t0 <- Sys.time()

  ns <- detect_nonsubject_rows(src$raw, spec)
  subj <- which(!ns)
  targets <- which(spec$role %in% MODEL_ROLES)
  cur <- src$raw[subj, targets, drop = FALSE]
  roles <- spec$role[targets]
  n <- nrow(cur)
  if (length(targets) && n < 4 * minbucket)
    stop("Too few subject rows (", n, ") for minbucket = ", minbucket,
         "; see small_sheets in synthesize_file()")

  # Candidate predictor pool: the targets plus any auxiliary columns
  n_aux <- 0L
  if (!is.null(aux)) {
    aux_raw <- aux$raw[subj, , drop = FALSE]
    n_aux <- ncol(aux_raw)
  }
  pool_raw <- if (n_aux) cbind(cur, aux_raw) else cur
  pool_roles <- c(roles, if (n_aux) aux$roles)

  if (verbose) message(sprintf(
    "synthesize_table: %d subject rows (%d non-subject), %d modelled columns, %d aux",
    n, sum(ns), length(targets), n_aux))
  # Full ranking of candidates per target; truncated to max_predictors at use
  sel <- screen_predictors(pool_raw, pool_roles, seq_along(targets),
                           keep = max(100, 4 * max_predictors),
                           lock_threshold = if (is.null(lock_threshold)) Inf else lock_threshold)
  locks <- if (censor_mode == "cell" && !is.null(lock_threshold)) {
    lock_groups(attr(sel, "lock_pairs"), length(targets))
  } else seq_along(targets)
  if (verbose && any(duplicated(locks))) message(sprintf(
    "  lock-breaking: %d columns in %d coupled groups share folds",
    sum(locks %in% locks[duplicated(locks)]), length(unique(locks[duplicated(locks)]))))

  log <- vector("list", n_cycles * n_folds * length(targets))
  li <- 0L
  if (!length(targets)) n_cycles <- 0  # nothing to synthesise
  for (cyc in seq_len(n_cycles)) {
    if (censor_mode == "cell") {
      # One fold vector per lock group (a singleton group is a plain column)
      gf <- lapply(unique(locks), function(g) sample(rep_len(seq_len(n_folds), n)))
      names(gf) <- unique(locks)
      folds <- lapply(locks, function(g) gf[[as.character(g)]])
    } else {
      f <- sample(rep_len(seq_len(n_folds), n))
      folds <- rep(list(f), length(targets))
    }
    for (k in seq_len(n_folds)) {
      # Fold k of every column is censored in this round, but only as a
      # target: each column is redrawn, in random order, from predictors that
      # are complete (original or already redrawn), and re-enters the frame at
      # once. Censoring the predictors too was tried first (2026-10-19) and
      # leaked: training rows with a censored predictor were routed by
      # surrogate splits into the wrong leaf and donated there (a deceased
      # donor in the "alive" leaf), breaking flag/date couplings.
      censor <- lapply(folds, function(f) f == k)
      pool_raw[seq_along(targets)] <- cur
      bp <- build_predictors(pool_raw, pool_roles)
      new_cur <- cur
      drawn <- integer(0)
      for (j in sample(seq_along(targets))) {
        cz <- censor[[j]]
        if (censor_mode == "cell") {
          # Group partners not yet redrawn this round are off limits (they
          # are censored in the same rows and would just copy the row back)
          blocked <- setdiff(which(locks == locks[j]), c(drawn, j))
          use <- utils::head(sel[[j]][!sel[[j]] %in% blocked], max_predictors)
        } else {
          # Row mode: the censored rows' own values of columns not yet redrawn
          # this round are off limits; only redrawn columns and aux qualify
          allowed <- c(drawn, if (n_aux) length(targets) + seq_len(n_aux))
          use <- utils::head(sel[[j]][sel[[j]] %in% allowed], max_predictors)
        }
        # Declared aux columns always condition every target, on top of the
        # screened ones: the analyst named them because the cross-table link
        # matters, and within-table predictors otherwise win the screening
        if (n_aux && aux_always) use <- union(use, length(targets) + seq_len(n_aux))
        X <- bp$P[, bp$group %in% use, drop = FALSE]
        x <- cur[[j]]
        role <- roles[j]
        st1 <- NA_integer_; st2 <- NA_integer_; fb <- FALSE
        if (role == "categorical") {
          y <- factor(cat_labels(x))
          dr <- leaf_donors(y, X, !cz, cz, minbucket, cp)
          new_cur[[j]][cz] <- x[dr$donor]
          st1 <- dr$leaves; fb <- dr$fallback
        } else {
          cs <- cell_state(x, role)
          d1 <- leaf_donors(cs$state, X, !cz, cz, minbucket, cp)
          drawn_state <- cs$state[d1$donor]
          vals <- x[d1$donor]                    # missing / junk copied as drawn
          need <- cz
          need[cz] <- drawn_state == "value"
          if (any(need)) {
            if (role == "text") {
              pool <- which(!cz & cs$state == "value")
              vals[drawn_state == "value"] <- x[pool[sample.int(length(pool), sum(need), replace = TRUE)]]
              st2 <- 1L
            } else {
              d2 <- leaf_donors(cs$parsed, X, !cz & cs$state == "value", need,
                                minbucket, cp)
              vals[drawn_state == "value"] <- x[d2$donor]
              st2 <- d2$leaves; fb <- d2$fallback
            }
          }
          new_cur[[j]][cz] <- vals
          st1 <- d1$leaves; fb <- fb || d1$fallback
        }
        if (any(cz)) bp <- refresh_group(bp, j, new_cur[[j]], role)
        drawn <- c(drawn, j)
        li <- li + 1L
        log[[li]] <- data.frame(cycle = cyc, fold = k, column = names(cur)[j],
                                role = role, n_censored = sum(cz),
                                n_predictors = ncol(X), leaves_state = st1,
                                leaves_value = st2, fallback = fb)
      }
      cur <- new_cur
      if (verbose) message(sprintf("  cycle %d fold %d/%d done (%.0f s)", cyc, k,
                                   n_folds, as.numeric(Sys.time() - t0, units = "secs")))
    }
  }

  syn <- src$raw
  syn[subj, targets] <- cur
  if (nonsubject == "blank" && any(ns)) {
    keep <- spec$role %in% c("id", "constant", "empty")
    syn[ns, !keep] <- NA_character_
  }

  structure(list(
    synthetic = syn, spec = spec, log = do.call(rbind, log[seq_len(li)]),
    nonsubject = which(ns),
    settings = list(n_folds = n_folds, n_cycles = n_cycles,
                    censor_mode = censor_mode, lock_threshold = lock_threshold,
                    lock_groups = sum(duplicated(locks)), max_predictors = max_predictors, minbucket = minbucket,
                    cp = cp, seed = seed, aux = if (n_aux) names(aux$raw),
                    aux_always = aux_always,
                    nonsubject = nonsubject, started = t0,
                    seconds = as.numeric(Sys.time() - t0, units = "secs")),
    orig_names = src$orig_names, orig_class = src$orig_class,
    format = src$format, sheet = src$sheet, path = src$path, delim = src$delim),
    class = "synth_result")
}

#' A result that is not synthesised: copied, blanked or dropped
#'
#' For sheets that are empty, have no modelled columns, or are too small to
#' synthesise (fewer than `4 * minbucket` subject rows): legends, codebooks,
#' lookup tables, tiny pilot tables.
#'
#' @param src A `synth_source`.
#' @param spec Its spec.
#' @param how `"copy"` (verbatim), `"blank"` (identifiers, constant and empty
#'   columns kept, every modelled cell emptied) or `"drop"` (left out of the
#'   output workbook).
#' @param reason Recorded in `settings$passthrough`.
#' @return A `synth_result` with `settings$passthrough` set.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:6), value = 1:6)
#' path <- file.path(tempdir(), "passthrough_result.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' spec <- infer_spec(src)
#' res <- passthrough_result(src, spec, how = "blank", reason = "Tiny table")
#' res$synthetic
#' @export
passthrough_result <- function(src, spec, how = c("blank", "copy", "drop"),
                               reason = "") {
  how <- match.arg(how)
  syn <- src$raw
  if (how == "blank" && ncol(syn)) {
    mod <- spec$role %in% MODEL_ROLES
    syn[, mod] <- NA_character_
  }
  structure(list(
    synthetic = syn, spec = spec, log = NULL, nonsubject = integer(0),
    settings = list(passthrough = how, reason = reason, seconds = 0),
    orig_names = src$orig_names, orig_class = src$orig_class,
    format = src$format, sheet = src$sheet, path = src$path,
    delim = src$delim), class = "synth_result")
}

#' Print a synth_result
#' @param x A `synth_result`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.synth_result <- function(x, ...) {
  if (!is.null(x$settings$passthrough)) {
    cat("<synth_result>", basename(x$path), "[", format(x$sheet), "] not synthesised:",
        x$settings$passthrough, "-", x$settings$reason, "\n")
    return(invisible(x))
  }
  cat("<synth_result>", basename(x$path), "\n")
  cat(nrow(x$synthetic), "rows;", sum(x$spec$role %in% MODEL_ROLES),
      "columns synthesised;", length(x$nonsubject), "non-subject rows;",
      sprintf("%.0f s\n", x$settings$seconds))
  print(table(x$spec$role))
  invisible(x)
}
