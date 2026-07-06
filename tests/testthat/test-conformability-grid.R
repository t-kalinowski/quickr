# Combinatorial enforcement of the conformability contract: every cell of
#   mode(left) x mode(right) x shape(left) x shape(right) x op
# is checked against plain R as the oracle.

skip_on_cran()

# Valid cells must match R exactly
# (values, typeof(), shape); statically invalid cells must fail to compile
# with the documented message; statically undecidable cells must compile a
# runtime guard that matches R on conformable inputs and raises the
# documented error on nonconformable ones.
#
# Compile-cost control: cells sharing a shape pair are packed into one
# compiled function with one statement per (op, mode pair, operand order),
# so one gfortran invocation covers up to ~90 cells. Ops split into two
# packed families because both operand orders appear in one function, so
# every operand is also a divisor/exponent somewhere:
#   gen: + - * < == & |   -- safe for any values (zeros, FALSE, negatives)
#   div: / ^ %% %/%       -- operands chosen zero-free and pow-safe, since
#        R answers the unsafe cells with NA/NaN (documented not-supported)
#        or traps SIGFPE in Fortran integer division
# Value edges ride along as input choices: negative dividends/divisors and
# bases (%% sign semantics, integer-exponent ^), magnitudes past 2^24 and
# 2^31 (%/% in the real domain), descending ranges, TRUE/FALSE arithmetic,
# and equal positions so == has TRUE cells.
#
# The default run compiles a fixed representative sample of shape pairs;
# set QUICKR_FULL_GRID=1 to compile every pair. Sampling is deterministic
# (a tier per pair, no randomness) so failures always reproduce.
# Compile-error cells never reach gfortran and always run.

run_full_grid <- Sys.getenv("QUICKR_FULL_GRID") %in%
  c("1", "true", "TRUE", "yes")

skip_unless_full_grid <- function() {
  if (!run_full_grid) {
    skip("representative sample only; set QUICKR_FULL_GRID=1 for the full grid")
  }
}

# --- Axes ---------------------------------------------------------------

grid_modes <- c(l = "logical", i = "integer", d = "double")

grid_shapes <- list(
  scl = list(decl = "1", kind = "scalar", n = 1L),
  vec3 = list(decl = "3", kind = "vec", len = 3L, n = 3L),
  vec4 = list(decl = "4", kind = "vec", len = 4L, n = 4L),
  mat32 = list(decl = "3, 2", kind = "mat", dims = c(3L, 2L), n = 6L),
  mat11 = list(decl = "1, 1", kind = "mat", dims = c(1L, 1L), n = 1L),
  sym = list(decl = "NA", kind = "vec", len = NA_integer_, n = 3L)
)

grid_op_families <- list(
  gen = c(
    add = "+",
    sub = "-",
    mul = "*",
    lt = "<",
    eq = "==",
    and = "&",
    or = "|"
  ),
  div = c(div = "/", pow = "^", mod = "%%", idv = "%/%")
)

# quickr requires logical operands for & and | (R would coerce numerics);
# an error divergence, pinned in its own test below.
grid_logical_only_ops <- c("and", "or")

grid_mode_pairs <- function(opname) {
  if (opname %in% grid_logical_only_ops) {
    return(list(c("l", "l")))
  }
  pairs <- expand.grid(
    names(grid_modes),
    names(grid_modes),
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(pairs)), function(i) c(pairs[i, 2L], pairs[i, 1L]))
}

# --- Values -------------------------------------------------------------

# Six values per (family, set, role, mode); shapes take a prefix (scalars
# and 1x1 matrices position 1, vec3/sym positions 1:3, mat32 all six).
grid_value_pool <- list(
  gen = list(
    primary = list(
      a = list(
        l = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
        i = c(-5L, 3L, 0L, 9L, 2L, -4L),
        d = c(1.5, 0, -100, 8.25, -2, 0.125)
      ),
      b = list(
        l = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
        i = c(2L, -3L, 4L, 0L, 2L, -7L),
        d = c(1.5, -2, 3, 0, -2, 8)
      )
    ),
    # extremes (products stay inside int32), descending runs, equal positions
    edge = list(
      a = list(
        l = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
        i = c(40000L, -40000L, 6L, 5L, 4L, 3L),
        d = c(1e10, -1e10, 2.5, 2.5, -0.5, 0)
      ),
      b = list(
        l = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
        i = c(40000L, 40000L, -6L, 5L, -4L, 0L),
        d = c(1e10, 1e10, -2.5, 2.5, 0.5, -8)
      )
    )
  ),
  div = list(
    # No zeros anywhere (either operand may be a divisor); doubles paired so
    # a negative base only ever meets a whole-valued exponent (R answers the
    # fractional case NaN, which is out of scope).
    primary = list(
      a = list(
        l = rep(TRUE, 6L),
        i = c(-5L, 3L, 7L, 9L, 2L, -4L),
        d = c(1.5, 3, 100, 8.25, 2, 2)
      ),
      b = list(
        l = rep(TRUE, 6L),
        i = c(2L, -3L, 4L, 3L, 2L, -7L),
        d = c(2, 3, 2, 4, 2, 3)
      )
    ),
    # 1e10: %/% quotient past both 2^24 and 2^31, exact in doubles, so the
    # old FLOOR()-to-int32 overflow would surface without float noise.
    edge = list(
      a = list(
        l = rep(TRUE, 6L),
        i = c(9L, 7L, 5L, -3L, 2L, 12L),
        d = c(1e10, -7.25, 4, 0.25, 9.5, 2)
      ),
      b = list(
        l = rep(TRUE, 6L),
        i = c(1L, 2L, -2L, 4L, 2L, 1L),
        d = c(1, 2, 2, 4, 3, 2)
      )
    )
  )
)

grid_operand <- function(role, family, set, mode, shape, sym_len = 3L) {
  pool <- grid_value_pool[[family]][[set]][[role]][[mode]]
  s <- grid_shapes[[shape]]
  n <- if (identical(shape, "sym")) sym_len else s$n
  x <- rep_len(pool, n)
  if (s$kind == "mat") {
    x <- matrix(x, nrow = s$dims[1L], ncol = s$dims[2L])
  }
  x
}

grid_pair_args <- function(
  sa,
  sb,
  family,
  set,
  sym_len_a = 3L,
  sym_len_b = 3L
) {
  args <- list()
  for (m in names(grid_modes)) {
    args[[paste0("a", m)]] <- grid_operand("a", family, set, m, sa, sym_len_a)
    args[[paste0("b", m)]] <- grid_operand("b", family, set, m, sb, sym_len_b)
  }
  args
}

# Length a `sym` operand must have to conform with its partner shape.
# For a matrix partner that is the vector-matrix rule's nrow -- including
# the 1x1 matrix, whose symbolic-vector cells guard on length 1.
grid_sym_ok_len <- function(partner) {
  p <- grid_shapes[[partner]]
  if (p$kind == "vec" && !is.na(p$len)) {
    p$len
  } else if (p$kind == "mat") {
    p$dims[1L]
  } else {
    3L
  }
}

# --- Generators ---------------------------------------------------------

# One function per (shape pair, op family): one statement per
# (op, mode pair, operand order), returning every result in a named list.
make_grid_pair_fn <- function(sa, sb, family) {
  decls <- c(
    vapply(
      names(grid_modes),
      function(m) {
        paste0(
          "type(a",
          m,
          " = ",
          grid_modes[[m]],
          "(",
          grid_shapes[[sa]]$decl,
          "))"
        )
      },
      ""
    ),
    vapply(
      names(grid_modes),
      function(m) {
        paste0(
          "type(b",
          m,
          " = ",
          grid_modes[[m]],
          "(",
          grid_shapes[[sb]]$decl,
          "))"
        )
      },
      ""
    )
  )
  ids <- character()
  stmts <- character()
  ops <- grid_op_families[[family]]
  pair_verdict <- grid_pair_verdict(sa, sb)
  for (opname in names(ops)) {
    if (!identical(grid_cell_verdict(sa, sb, opname), pair_verdict)) {
      next # strict-op 1x1 rows: covered by the compile-error sweep
    }
    for (p in grid_mode_pairs(opname)) {
      for (ord in c("ab", "ba")) {
        id <- paste0("r_", ord, "_", opname, "_", p[1L], p[2L])
        expr <- if (identical(ord, "ab")) {
          paste0("a", p[1L], " ", ops[[opname]], " b", p[2L])
        } else {
          paste0("b", p[2L], " ", ops[[opname]], " a", p[1L])
        }
        ids <- c(ids, id)
        stmts <- c(stmts, paste0("  ", id, " <- ", expr))
      }
    }
  }
  src <- paste0(
    "function(al, ai, ad, bl, bi, bd) {\n",
    "  declare(\n    ",
    paste(decls, collapse = ",\n    "),
    "\n  )\n",
    paste(stmts, collapse = "\n"),
    "\n  list(\n    ",
    paste(paste0(ids, " = ", ids), collapse = ",\n    "),
    "\n  )\n}"
  )
  eval(parse(text = src)[[1L]])
}

# A one-cell function, for cells whose expected outcome is a compile error.
make_grid_cell_fn <- function(sa, sb, op, ma, mb) {
  src <- paste0(
    "function(a, b) {\n",
    "  declare(type(a = ",
    grid_modes[[ma]],
    "(",
    grid_shapes[[sa]]$decl,
    ")), ",
    "type(b = ",
    grid_modes[[mb]],
    "(",
    grid_shapes[[sb]]$decl,
    ")))\n",
    "  a ",
    op,
    " b\n",
    "}"
  )
  eval(parse(text = src)[[1L]])
}

# --- The expected-outcome function --------------------------------------
# Direct transcription of the shape-contract table, per cell:
#   1. scalar op anything                    -> allow, no guard
#   2. identical known shapes                -> allow, no guard
#   3. vec(n) op mat(n, k)                   -> allow (column-major recycling)
#   4. known mismatch (incl. length 0)       -> compile error
#   5. not statically decidable (NA dims)    -> runtime guard
# One op-class split, mirroring R: *arithmetic* recycles a 1x1 matrix
# against a vector of statically known length != 1 (deprecated in R but
# still its answer, so quickr scalarizes), while comparisons and & | error
# there -- for those the 1x1 is an ordinary one-row matrix and the
# vector-matrix rule applies. A *symbolic* vector length takes the
# vector-matrix rule for every op class: the result's shape depends on the
# runtime length (R keeps the 1x1 dims only for a length-1 vector), so a
# runtime guard requires length 1 and longer vectors error where R would
# recycle (both flavors are pinned in test-recycling.R).

grid_strict_ops <- c("lt", "eq", "and", "or")

grid_cell_verdict <- function(sa, sb, opname) {
  A <- grid_shapes[[sa]]
  B <- grid_shapes[[sb]]
  is_1x1 <- function(s) s$kind == "mat" && all(s$dims == 1L)
  ok <- list(outcome = "ok")
  guard <- function(msg) list(outcome = "guard", guard_msg = msg)
  err <- function(msg) list(outcome = "error", msg = msg)

  if (A$kind == "scalar" || B$kind == "scalar") {
    return(ok)
  }
  if ((is_1x1(A) && B$kind == "vec") || (is_1x1(B) && A$kind == "vec")) {
    vec <- if (A$kind == "vec") A else B
    if (is.na(vec$len)) {
      return(guard("matrix first dimension"))
    }
    if (vec$len == 1L) {
      return(ok)
    }
    if (opname %in% grid_strict_ops) {
      return(err("matrix first dimension"))
    }
    return(ok) # scalarized 1x1: R's length-1 array recycling
  }
  if (A$kind == "vec" && B$kind == "vec") {
    if (is.na(A$len) || is.na(B$len)) {
      return(guard("equal lengths"))
    }
    if (A$len == B$len) {
      return(ok)
    }
    return(err("equal lengths"))
  }
  if (A$kind == "mat" && B$kind == "mat") {
    if (all(A$dims == B$dims)) {
      return(ok)
    }
    return(err("matching dimensions"))
  }
  # vector op matrix: vector length against nrow
  vec <- if (A$kind == "vec") A else B
  mat <- if (A$kind == "mat") A else B
  if (is.na(vec$len)) {
    return(guard("matrix first dimension"))
  }
  if (vec$len == mat$dims[1L]) {
    return(ok)
  }
  err("matrix first dimension")
}

# Shape-pair verdict for packing: the arithmetic-class verdict. Cells whose
# own verdict differs (the strict-op 1x1 rows) are excluded from the packed
# function and land in the compile-error sweep instead.
grid_pair_verdict <- function(sa, sb) {
  grid_cell_verdict(sa, sb, "add")
}

# Which shape pairs compile in the default run. Every non-error pair must
# be listed: adding a shape without deciding its tier is an error.
grid_pair_tiers <- c(
  "scl.scl" = "full",
  "scl.vec3" = "core",
  "scl.vec4" = "full",
  "scl.mat32" = "full",
  "scl.mat11" = "full",
  "scl.sym" = "full",
  "vec3.vec3" = "core",
  "vec3.mat32" = "core",
  "vec3.mat11" = "full",
  "vec3.sym" = "core",
  "vec4.vec4" = "full",
  "vec4.mat11" = "full",
  "vec4.sym" = "full",
  "mat32.mat32" = "core",
  "mat32.sym" = "core",
  "mat11.mat11" = "full",
  "mat11.sym" = "full",
  "sym.sym" = "core"
)

# --- Oracle comparison --------------------------------------------------

# suppressWarnings: R deprecation-warns on 1x1-array-vs-vector recycling
# (mat11 pairs); values still match, which is what the contract pins.
expect_grid_cells_match <- function(qfn, fn, args, context) {
  r_res <- suppressWarnings(do.call(fn, args))
  q_res <- do.call(qfn, args)
  expect_identical(names(q_res), names(r_res))
  for (nm in names(r_res)) {
    expect_equal(
      q_res[[nm]],
      r_res[[nm]],
      label = paste0(context, " ", nm, " (quickr)"),
      expected.label = "R"
    )
    expect_identical(
      typeof(q_res[[nm]]),
      typeof(r_res[[nm]]),
      label = paste0(context, " ", nm, " typeof (quickr)"),
      expected.label = "typeof (R)"
    )
  }
}

# --- Elementwise grid: valid and guarded shape pairs ---------------------

grid_pair_names <- names(grid_shapes)
for (i in seq_along(grid_pair_names)) {
  for (j in seq.int(i, length(grid_pair_names))) {
    local({
      sa <- grid_pair_names[[i]]
      sb <- grid_pair_names[[j]]
      verdict <- grid_pair_verdict(sa, sb)
      if (identical(verdict$outcome, "error")) {
        return() # handled in the compile-error section below
      }
      pair_id <- paste0(sa, ".", sb)
      tier <- grid_pair_tiers[[pair_id]]
      stopifnot(tier %in% c("core", "full"))

      for (family in names(grid_op_families)) {
        test_that(paste0("elementwise grid ", pair_id, " [", family, "]"), {
          if (identical(tier, "full")) {
            skip_unless_full_grid()
          }
          fn <- make_grid_pair_fn(sa, sb, family)
          dll_paths_before <- loaded_dll_paths()
          on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
          qfn <- quick(fn)

          sym_a <- if (identical(sa, "sym")) grid_sym_ok_len(sb) else 3L
          sym_b <- if (identical(sb, "sym")) grid_sym_ok_len(sa) else 3L
          for (set in c("primary", "edge")) {
            args <- grid_pair_args(sa, sb, family, set, sym_a, sym_b)
            expect_grid_cells_match(
              qfn,
              fn,
              args,
              context = paste0(pair_id, "/", set)
            )
          }

          if (identical(verdict$outcome, "guard")) {
            # bump one symbolic operand's length; the guard must raise the
            # documented error where R errors and BLAS-free Fortran would
            # read or write out of bounds
            bad_b <- if (identical(sb, "sym")) sym_b + 1L else sym_b
            bad_a <- if (identical(sb, "sym")) sym_a else sym_a + 1L
            args_bad <- grid_pair_args(sa, sb, family, "primary", bad_a, bad_b)
            expect_error(
              do.call(qfn, args_bad),
              verdict$guard_msg,
              fixed = TRUE
            )
          }
        })
      }
    })
  }
}

# --- Elementwise grid: statically rejected shape pairs -------------------
# Compile errors never reach gfortran, so every op and both operand orders
# are cheap enough to always run.

test_that("statically nonconformable cells are compile errors for every op", {
  for (i in seq_along(grid_pair_names)) {
    for (j in seq.int(i, length(grid_pair_names))) {
      sa <- grid_pair_names[[i]]
      sb <- grid_pair_names[[j]]
      for (family in names(grid_op_families)) {
        ops <- grid_op_families[[family]]
        for (opname in names(ops)) {
          verdict <- grid_cell_verdict(sa, sb, opname)
          if (!identical(verdict$outcome, "error")) {
            next
          }
          modes <- if (opname %in% grid_logical_only_ops) {
            c("l", "l")
          } else {
            c("d", "d")
          }
          for (ord in list(c(sa, sb), c(sb, sa))) {
            fn <- make_grid_cell_fn(
              ord[1L],
              ord[2L],
              ops[[opname]],
              modes[1L],
              modes[2L]
            )
            expect_error(
              quick(fn),
              verdict$msg,
              fixed = TRUE,
              label = paste0(
                "quick() for ",
                ord[1L],
                " ",
                ops[[opname]],
                " ",
                ord[2L]
              )
            )
          }
        }
      }
    }
  }
})

test_that("known length-0 operands are compile errors", {
  fn <- eval(parse(
    text = paste0(
      "function(a, b) {\n",
      "  declare(type(a = double(0)), type(b = double(4)))\n",
      "  a + b\n}"
    )
  )[[1L]])
  expect_error(quick(fn), "equal lengths", fixed = TRUE)
})

test_that("& and | require logical operands (R would coerce: error divergence)", {
  for (op in c("&", "|")) {
    for (ma in names(grid_modes)) {
      for (mb in names(grid_modes)) {
        if (identical(ma, "l") && identical(mb, "l")) {
          next
        }
        fn <- make_grid_cell_fn("vec3", "vec3", op, ma, mb)
        expect_error(
          quick(fn),
          "requires logical operands",
          fixed = TRUE,
          label = paste0(
            "quick() for ",
            grid_modes[[ma]],
            " ",
            op,
            " ",
            grid_modes[[mb]]
          )
        )
      }
    }
  }
})

# --- c(): mode join over all elements, constructive lengths --------------

test_that("c() grid: lattice join across modes, known and mixed lengths", {
  ids <- character()
  stmts <- character()
  for (ma in names(grid_modes)) {
    for (mb in names(grid_modes)) {
      id_ab <- paste0("r_c_", ma, mb)
      id_sb <- paste0("r_cs_", ma, mb)
      ids <- c(ids, id_ab, id_sb)
      stmts <- c(
        stmts,
        paste0("  ", id_ab, " <- c(a", ma, ", b", mb, ")"),
        paste0("  ", id_sb, " <- c(s", ma, ", b", mb, ")")
      )
    }
  }
  ids <- c(ids, "r_c3")
  stmts <- c(stmts, "  r_c3 <- c(sl, ai, bd)") # three-mode join
  src <- paste0(
    "function(al, ai, ad, bl, bi, bd, sl, si, sd) {\n",
    "  declare(\n",
    "    type(al = logical(3)), type(ai = integer(3)), type(ad = double(3)),\n",
    "    type(bl = logical(4)), type(bi = integer(4)), type(bd = double(4)),\n",
    "    type(sl = logical(1)), type(si = integer(1)), type(sd = double(1))\n",
    "  )\n",
    paste(stmts, collapse = "\n"),
    "\n  list(\n    ",
    paste(paste0(ids, " = ", ids), collapse = ",\n    "),
    "\n  )\n}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)
  for (set in c("primary", "edge")) {
    args <- c(
      grid_pair_args("vec3", "vec4", "gen", set),
      list(
        sl = grid_operand("a", "gen", set, "l", "scl"),
        si = grid_operand("a", "gen", set, "i", "scl"),
        sd = grid_operand("a", "gen", set, "d", "scl")
      )
    )
    expect_grid_cells_match(qfn, fn, args, context = paste0("c()/", set))
  }
})

test_that("c() grid: symbolic lengths are constructive (no guard)", {
  src <- paste0(
    "function(al, ai, ad, bl, bi, bd) {\n",
    "  declare(\n",
    "    type(al = logical(NA)), type(ai = integer(NA)), type(ad = double(NA)),\n",
    "    type(bl = logical(NA)), type(bi = integer(NA)), type(bd = double(NA))\n",
    "  )\n",
    "  r_ll <- c(al, bl)\n",
    "  r_id <- c(ai, bd)\n",
    "  r_dl <- c(ad, bl)\n",
    "  r_dd <- c(ad, bd)\n",
    "  list(r_ll = r_ll, r_id = r_id, r_dl = r_dl, r_dd = r_dd)\n",
    "}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)
  # unequal lengths are fine for c(): lengths add, nothing to conform
  args <- grid_pair_args(
    "sym",
    "sym",
    "gen",
    "primary",
    sym_len_a = 3L,
    sym_len_b = 4L
  )
  expect_grid_cells_match(qfn, fn, args, context = "c()/sym")
})

test_that("c() rejects rank-2 args (R would flatten: error divergence)", {
  fn <- eval(parse(
    text = paste0(
      "function(a) {\n",
      "  declare(type(a = double(3, 2)))\n",
      "  c(a, 1.0)\n}"
    )
  )[[1L]])
  expect_error(quick(fn), "scalars or 1-d arrays", fixed = TRUE)
})

# --- Multi-arg min()/max()/sum(): join across args, shapes independent ----

test_that("multi-arg min/max/sum grid: modes join, arg shapes independent", {
  ids <- character()
  stmts <- character()
  for (fname in c("min", "max", "sum")) {
    for (ma in names(grid_modes)) {
      for (mb in names(grid_modes)) {
        id <- paste0("r_", fname, "_", ma, mb)
        ids <- c(ids, id)
        stmts <- c(
          stmts,
          paste0("  ", id, " <- ", fname, "(a", ma, ", b", mb, ")")
        )
      }
    }
  }
  ids <- c(ids, "r_min3", "r_max3")
  stmts <- c(
    stmts,
    "  r_min3 <- min(al, bi, sd)",
    "  r_max3 <- max(al, bi, sd)"
  )
  src <- paste0(
    "function(al, ai, ad, bl, bi, bd, sd) {\n",
    "  declare(\n",
    "    type(al = logical(3)), type(ai = integer(3)), type(ad = double(3)),\n",
    "    type(bl = logical(4)), type(bi = integer(4)), type(bd = double(4)),\n",
    "    type(sd = double(1))\n",
    "  )\n",
    paste(stmts, collapse = "\n"),
    "\n  list(\n    ",
    paste(paste0(ids, " = ", ids), collapse = ",\n    "),
    "\n  )\n}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)
  for (set in c("primary", "edge")) {
    args <- c(
      grid_pair_args("vec3", "vec4", "gen", set),
      list(sd = grid_operand("a", "gen", set, "d", "scl"))
    )
    expect_grid_cells_match(
      qfn,
      fn,
      args,
      context = paste0("min/max/sum/", set)
    )
  }
})

# --- ifelse(): branch-mode join, shape from `test` ------------------------

test_that("ifelse grid: branch mode pairs join; scalars broadcast against vector test", {
  ids <- character()
  stmts <- character()
  for (my in names(grid_modes)) {
    for (mn in names(grid_modes)) {
      for (combo in c("vv", "sv", "vs", "ss")) {
        id <- paste0("r_", combo, "_", my, mn)
        yes <- if (substr(combo, 1L, 1L) == "v") {
          paste0("y", my)
        } else {
          paste0("p", my)
        }
        no <- if (substr(combo, 2L, 2L) == "v") {
          paste0("n", mn)
        } else {
          paste0("q", mn)
        }
        ids <- c(ids, id)
        stmts <- c(
          stmts,
          paste0("  ", id, " <- ifelse(t3, ", yes, ", ", no, ")")
        )
      }
    }
  }
  src <- paste0(
    "function(t3, yl, yi, yd, nl, ni, nd, pl, pi, pd, ql, qi, qd) {\n",
    "  declare(\n",
    "    type(t3 = logical(3)),\n",
    "    type(yl = logical(3)), type(yi = integer(3)), type(yd = double(3)),\n",
    "    type(nl = logical(3)), type(ni = integer(3)), type(nd = double(3)),\n",
    "    type(pl = logical(1)), type(pi = integer(1)), type(pd = double(1)),\n",
    "    type(ql = logical(1)), type(qi = integer(1)), type(qd = double(1))\n",
    "  )\n",
    paste(stmts, collapse = "\n"),
    "\n  list(\n    ",
    paste(paste0(ids, " = ", ids), collapse = ",\n    "),
    "\n  )\n}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)
  for (set in c("primary", "edge")) {
    # `test` must stay mixed TRUE/FALSE: with a one-sided test R's ifelse
    # never materializes the untaken branch, so its result type becomes
    # value-dependent -- not representable statically (step-9 divergence)
    args <- list(t3 = c(TRUE, FALSE, TRUE))
    for (m in names(grid_modes)) {
      args[[paste0("y", m)]] <- grid_operand("a", "gen", set, m, "vec3")
      args[[paste0("n", m)]] <- grid_operand("b", "gen", set, m, "vec3")
      args[[paste0("p", m)]] <- grid_operand("a", "gen", set, m, "scl")
      args[[paste0("q", m)]] <- grid_operand("b", "gen", set, m, "scl")
    }
    expect_grid_cells_match(qfn, fn, args, context = paste0("ifelse/", set))
  }
})

test_that("ifelse grid: symbolic branch lengths get a runtime guard", {
  ids <- character()
  stmts <- character()
  for (my in names(grid_modes)) {
    for (mn in names(grid_modes)) {
      id <- paste0("r_", my, mn)
      ids <- c(ids, id)
      stmts <- c(
        stmts,
        paste0("  ", id, " <- ifelse(t1, y", my, ", n", mn, ")")
      )
    }
  }
  src <- paste0(
    "function(t1, yl, yi, yd, nl, ni, nd) {\n",
    "  declare(\n",
    "    type(t1 = logical(NA)),\n",
    "    type(yl = logical(NA)), type(yi = integer(NA)), type(yd = double(NA)),\n",
    "    type(nl = logical(NA)), type(ni = integer(NA)), type(nd = double(NA))\n",
    "  )\n",
    paste(stmts, collapse = "\n"),
    "\n  list(\n    ",
    paste(paste0(ids, " = ", ids), collapse = ",\n    "),
    "\n  )\n}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)

  make_args <- function(len_n) {
    args <- list(t1 = c(TRUE, FALSE, TRUE))
    for (m in names(grid_modes)) {
      args[[paste0("y", m)]] <- grid_operand(
        "a",
        "gen",
        "primary",
        m,
        "sym",
        3L
      )
      args[[paste0("n", m)]] <- grid_operand(
        "b",
        "gen",
        "primary",
        m,
        "sym",
        len_n
      )
    }
    args
  }
  args_ok <- make_args(3L)
  expect_grid_cells_match(qfn, fn, args_ok, context = "ifelse/sym")
  # was: unguarded merge() reading past the shorter branch
  expect_error(
    do.call(qfn, make_args(4L)),
    "must be scalars or match the shape",
    fixed = TRUE
  )
})

test_that("ifelse grid: matrix test shapes the result", {
  skip_unless_full_grid()
  src <- paste0(
    "function(tm, ym, nm, yi, pd) {\n",
    "  declare(\n",
    "    type(tm = logical(3, 2)),\n",
    "    type(ym = double(3, 2)), type(nm = double(3, 2)),\n",
    "    type(yi = integer(3, 2)), type(pd = double(1))\n",
    "  )\n",
    "  r_dd <- ifelse(tm, ym, nm)\n",
    "  r_id <- ifelse(tm, yi, nm)\n",
    "  r_sd <- ifelse(tm, pd, nm)\n",
    "  list(r_dd = r_dd, r_id = r_id, r_sd = r_sd)\n",
    "}"
  )
  fn <- eval(parse(text = src)[[1L]])
  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn <- quick(fn)
  args <- list(
    tm = matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), 3L, 2L),
    ym = grid_operand("a", "gen", "primary", "d", "mat32"),
    nm = grid_operand("b", "gen", "primary", "d", "mat32"),
    yi = grid_operand("a", "gen", "primary", "i", "mat32"),
    pd = grid_operand("a", "gen", "primary", "d", "scl")
  )
  expect_grid_cells_match(qfn, fn, args, context = "ifelse/mat")
})

test_that("ifelse contract violations are compile errors", {
  scalar_test <- eval(parse(
    text = paste0(
      "function(t1, y, n) {\n",
      "  declare(type(t1 = logical(1)), type(y = double(3)), type(n = double(3)))\n",
      "  ifelse(t1, y, n)\n}"
    )
  )[[1L]])
  expect_error(quick(scalar_test), "scalar test is not supported", fixed = TRUE)

  known_mismatch <- eval(parse(
    text = paste0(
      "function(t3, y, n) {\n",
      "  declare(type(t3 = logical(3)), type(y = double(4)), type(n = double(3)))\n",
      "  ifelse(t3, y, n)\n}"
    )
  )[[1L]])
  expect_error(
    quick(known_mismatch),
    "must be scalars or match the shape",
    fixed = TRUE
  )

  rank_mismatch <- eval(parse(
    text = paste0(
      "function(t3, y, n) {\n",
      "  declare(type(t3 = logical(3)), type(y = double(3, 2)), type(n = double(3)))\n",
      "  ifelse(t3, y, n)\n}"
    )
  )[[1L]])
  expect_error(
    quick(rank_mismatch),
    "must be scalars or match the shape",
    fixed = TRUE
  )
})
