# Unit tests for matrix-specific internal helpers

test_that("matrix helper dimensions handle scalars and defaults", {
  expect_identical(quickr:::dim_or_one_from(NULL, 1L), 1L)
  expect_identical(quickr:::dim_or_one_from(list(3L), 1L), 3L)
  expect_identical(quickr:::dim_or_one_from(list(3L), 2L), 1L)

  expect_identical(
    quickr:::matrix_dims_from(0L, NULL, orientation = "matrix"),
    list(rows = 1L, cols = 1L)
  )
  expect_identical(
    quickr:::matrix_dims_from(1L, list(4L), orientation = "rowvec"),
    list(rows = 1L, cols = 4L)
  )
  expect_identical(
    quickr:::matrix_dims_from(1L, list(4L), orientation = "colvec"),
    list(rows = 4L, cols = 1L)
  )
})

test_that("symbol_name_or_null recognizes identifiers", {
  var <- quickr:::Variable("double", list(1L, 1L), name = "x")
  f_sym <- quickr:::Fortran("x", var, r = quote(x))
  expect_identical(quickr:::symbol_name_or_null(f_sym), "x")

  f_str <- quickr:::Fortran("x", var)
  expect_identical(quickr:::symbol_name_or_null(f_str), "x")

  f_paren <- quickr:::Fortran("(x)", var, r = quote((x)))
  expect_identical(quickr:::symbol_name_or_null(f_paren), "x")

  f_nested <- quickr:::Fortran("(((x)))", var, r = quote((((x)))))
  expect_identical(quickr:::symbol_name_or_null(f_nested), "x")

  f_expr <- quickr:::Fortran("x + 1", var, r = quote(x + 1))
  expect_null(quickr:::symbol_name_or_null(f_expr))
})

test_that("logical_arg_or_default returns NULL defaults", {
  expect_null(quickr:::logical_arg_or_default(list(), "upper.tri", NULL, "ctx"))
})

test_that("destination helpers handle NULL and mode mismatches", {
  expect_invisible(
    quickr:::assert_dest_dims_compatible(NULL, list(1L), "ctx")
  )

  dest <- quickr:::Variable("integer", list(1L), name = "out")

  expect_false(
    quickr:::can_use_output(
      dest,
      input_names = c("x", "y"),
      expected_dims = list(1L),
      context = "ctx"
    )
  )
})

test_that("bind output helpers handle type coercion edge cases", {
  make_value <- function(mode, dims = list(1L), name = "x") {
    Fortran(
      name,
      Variable(mode, dims, name = name),
      r = as.symbol(name)
    )
  }

  val_logical <- make_value("logical", name = "l")
  val_integer <- make_value("integer", name = "i")
  val_double <- make_value("double", name = "d")
  val_complex <- make_value("complex", name = "z")
  val_raw <- make_value("raw", name = "r")
  val_character <- make_value("character", name = "c")
  val_unknown <- make_value(NULL, name = "u")

  expect_identical(
    bind_output_mode(list(val_integer, val_logical), "bind"),
    "integer"
  )
  expect_identical(
    bind_output_mode(list(val_double, val_integer), "bind"),
    "double"
  )

  expect_error(
    bind_output_mode(list(val_unknown), "bind"),
    "inputs must have a known type"
  )
  expect_error(
    bind_output_mode(list(val_complex, val_double), "bind"),
    "does not support mixing complex"
  )
  expect_error(
    bind_output_mode(list(val_raw, val_character), "bind"),
    "does not support mixing raw with other types"
  )

  cast_double <- bind_cast_value(val_integer, "double", "bind")
  expect_identical(cast_double@value@mode, "double")
  expect_identical(cast_double@value@dims, val_integer@value@dims)

  cast_int <- bind_cast_value(val_logical, "integer", "bind")
  expect_identical(cast_int@value@mode, "integer")
  expect_identical(
    as.character(cast_int),
    paste0("merge(1_c_int, 0_c_int, ", as.character(val_logical), ")")
  )

  expect_error(
    bind_cast_value(val_double, "integer", "bind"),
    "does not support coercion from"
  )
})

test_that("bind dimension helpers cover scalar, symbolic, and unknown sizes", {
  expect_error(
    bind_dim_sum(list(NA_integer_), "ctx", "row"),
    "requires inputs with known row sizes"
  )

  expr <- bind_dim_sum(list(quote(n), 2L), "ctx", "column")
  expect_true(is.language(expr))
  expect_identical(expr[[1L]], quote(`+`))

  expect_identical(
    bind_common_dim(list(1L, 1L), c(TRUE, TRUE), "ctx", "row"),
    1L
  )
  expect_warning(
    common <- bind_common_dim(
      list(quote(n), quote(m)),
      c(FALSE, FALSE),
      "ctx",
      "row"
    ),
    "cannot verify conformability in ctx"
  )
  expect_identical(common, quote(n))

  expect_identical(bind_dim_string(3L), "3")
  expect_identical(
    bind_dim_string(quote(n + 1L)),
    "n + 1"
  )
  expect_identical(bind_dim_int(quote(n + 1L)), "int(n + 1)")
})

test_that("bind matrix expression helpers protect against unsupported ranks", {
  scalar <- Fortran(
    "s",
    Variable("double", name = "s"),
    r = quote(s)
  )
  mat <- Fortran(
    "A",
    Variable("double", list(2L, 2L), name = "A"),
    r = quote(A)
  )
  rank3 <- Fortran(
    "arr",
    Variable("double", list(2L, 2L, 2L), name = "arr"),
    r = quote(arr)
  )

  expect_identical(
    bind_col_matrix_expr(
      mat,
      rows = 2L,
      is_scalar = FALSE,
      context = "ctx"
    ),
    "A"
  )
  expect_identical(
    bind_row_matrix_expr(
      mat,
      cols = 2L,
      is_scalar = FALSE,
      context = "ctx"
    ),
    "A"
  )

  expect_error(
    bind_col_matrix_expr(
      rank3,
      rows = 2L,
      is_scalar = FALSE,
      context = "ctx"
    ),
    "only supports rank 0-2 inputs"
  )
  expect_error(
    bind_row_matrix_expr(
      rank3,
      cols = 2L,
      is_scalar = FALSE,
      context = "ctx"
    ),
    "only supports rank 0-2 inputs"
  )

  spread_col <- bind_col_matrix_expr(
    scalar,
    rows = quote(n),
    is_scalar = TRUE,
    context = "ctx"
  )
  expect_match(spread_col, "spread", fixed = FALSE)

  spread_row <- bind_row_matrix_expr(
    scalar,
    cols = quote(m),
    is_scalar = TRUE,
    context = "ctx"
  )
  expect_match(spread_row, "spread", fixed = FALSE)
})

test_that("maybe_reshape_vector_matrix reshapes vectors for singleton matrices", {
  vec <- quickr:::Fortran("v", quickr:::Variable("double", list(3L)))
  mat_row <- quickr:::Fortran("m", quickr:::Variable("double", list(1L, 3L)))
  mat_scalar <- quickr:::Fortran("s", quickr:::Variable("double", list(1L, 1L)))
  vec_one <- quickr:::Fortran("w", quickr:::Variable("double", list(1L)))

  reshaped <- quickr:::maybe_reshape_vector_matrix(vec, mat_row)
  expect_identical(reshaped$left@value@rank, 2L)
  expect_identical(reshaped$left@value@dims, list(1L, 3L))
  expect_identical(reshaped$right@value@dims, list(1L, 3L))

  reshaped <- quickr:::maybe_reshape_vector_matrix(vec_one, mat_scalar)
  expect_identical(reshaped$left@value@rank, 2L)
  expect_identical(reshaped$left@value@dims, list(1L, 1L))
  expect_identical(reshaped$right@value@dims, list(1L, 1L))

  reshaped <- quickr:::maybe_reshape_vector_matrix(vec, mat_scalar)
  expect_identical(reshaped$left@value@rank, 1L)
  expect_identical(reshaped$right@value@rank, 0L)

  reshaped <- quickr:::maybe_reshape_vector_matrix(mat_scalar, vec)
  expect_identical(reshaped$left@value@rank, 0L)
  expect_identical(reshaped$right@value@rank, 1L)
})

test_that("indexing matrix expressions can hoist temporaries", {
  fn <- function(x) {
    declare(type(x = double(n, k)))
    t(x)[1]
  }

  code <- as.character(r2f(fn))
  tmp <- regmatches(code, regexpr("btmp[0-9]+_", code))
  expect_true(nzchar(tmp))
  expect_match(code, paste0(tmp, " = transpose\\(x\\)"))
  expect_match(code, paste0("out_ = ", tmp, "\\("))
  expect_false(grepl("transpose\\(x\\)\\(", code))
})

test_that("unwrap_transpose_arg handles scalar inputs and rank errors", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("a", quickr:::Variable("double", name = "a"))
  scope@assign(
    "arr",
    quickr:::Variable("double", list(2L, 2L, 2L), name = "arr")
  )
  hoist <- quickr:::new_hoist(scope)

  info <- quickr:::unwrap_transpose_arg(quote(t(a)), scope, hoist = hoist)
  expect_identical(info$trans, "N")
  expect_identical(info$value@value@rank, 0L)

  expect_error(
    quickr:::unwrap_transpose_arg(quote(t(arr)), scope, hoist = hoist),
    "t\\(\\) only supports rank 0-2 inputs"
  )
})

test_that("symbol_name_or_null recognizes bare Fortran names", {
  val <- quickr:::Variable("double")
  A <- quickr:::Fortran("A", val, r = NULL)
  expect_identical(quickr:::symbol_name_or_null(A), "A")
})

test_that("assert_hoist_env validates hoist environments", {
  expect_error(
    quickr:::assert_hoist_env(NULL),
    "hoist must be a hoist environment"
  )
  expect_invisible(quickr:::assert_hoist_env(new.env(parent = emptyenv())))
})

test_that("rank assertion helpers validate matrix and vector inputs", {
  mat <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 2L), name = "A")
  )
  vec <- quickr:::Fortran(
    "v",
    quickr:::Variable("double", list(2L), name = "v")
  )
  arr <- quickr:::Fortran(
    "arr",
    quickr:::Variable("double", list(2L, 2L, 2L), name = "arr")
  )

  expect_invisible(quickr:::assert_rank2_matrix(mat, "matrix needed"))
  expect_error(
    quickr:::assert_rank2_matrix(vec, "matrix needed"),
    "matrix needed"
  )

  expect_invisible(quickr:::assert_rank_leq1(vec, "vector needed"))
  expect_error(
    quickr:::assert_rank_leq1(mat, "vector needed"),
    "vector needed"
  )

  expect_invisible(quickr:::assert_rank_leq2(mat, "rank 0-2 needed"))
  expect_error(
    quickr:::assert_rank_leq2(arr, "rank 0-2 needed"),
    "rank 0-2 needed"
  )
})

test_that("rhs and conformability helpers validate inputs", {
  expect_error(
    quickr:::assert_rhs_rank(
      0L,
      err_scalar = "scalar rhs",
      err_high = "high rhs"
    ),
    "scalar rhs"
  )
  expect_error(
    quickr:::assert_rhs_rank(
      3L,
      err_scalar = "scalar rhs",
      err_high = "high rhs"
    ),
    "high rhs"
  )
  expect_invisible(
    quickr:::assert_rhs_rank(
      1L,
      err_scalar = "scalar rhs",
      err_high = "high rhs"
    )
  )

  expect_invisible(
    quickr:::assert_conformable_dims(
      2L,
      2L,
      context = "test",
      err_msg = "non-conformable"
    )
  )
  expect_error(
    quickr:::assert_conformable_dims(
      2L,
      3L,
      context = "test",
      err_msg = "non-conformable"
    ),
    "non-conformable"
  )
  expect_warning(
    quickr:::assert_conformable_dims(
      quote(n),
      quote(m),
      context = "test",
      err_msg = "non-conformable"
    ),
    "cannot verify conformability in test"
  )
})

test_that("diag_length_expr requires known dimensions", {
  expect_error(
    quickr:::diag_length_expr(NA, 2L, "diag"),
    "requires known dimensions"
  )
  expect_error(
    quickr:::diag_length_expr(2L, NA, "diag"),
    "requires known dimensions"
  )
})

test_that("blas helpers require a hoist environment", {
  scope <- new.env(parent = emptyenv())
  var_mat <- quickr:::Variable("double", list(1L, 1L), name = "A")
  var_vec <- quickr:::Variable("double", list(1L), name = "x")
  A <- quickr:::Fortran("A", var_mat, r = quote(A))
  B <- quickr:::Fortran("B", var_mat, r = quote(B))
  x <- quickr:::Fortran("x", var_vec, r = quote(x))

  expect_error(
    quickr:::gemm(
      "N",
      "N",
      A,
      B,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::gemv(
      "N",
      A,
      x,
      1L,
      1L,
      1L,
      list(1L, 1L),
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::syrk("N", A, scope = scope, hoist = NULL),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::outer_mul(x, x, scope = scope, hoist = NULL),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::triangular_solve(
      A,
      x,
      "L",
      "N",
      "N",
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::lapack_solve(
      A,
      x,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::lapack_inverse(
      A,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::lapack_chol(
      A,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::lapack_chol2inv(
      A,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::diag_extract(
      A,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
  expect_error(
    quickr:::diag_matrix(
      x,
      nrow = 1L,
      ncol = 1L,
      scope = scope,
      hoist = NULL
    ),
    "hoist must be a hoist environment"
  )
})

test_that("blas helpers allocate temporaries when outputs are not reused", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  A <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 3L), name = "A"),
    r = quote(A)
  )
  x <- quickr:::Fortran(
    "x",
    quickr:::Variable("double", list(3L), name = "x"),
    r = quote(x)
  )

  out_gemv <- quickr:::gemv(
    "N",
    A,
    x,
    m = 2L,
    n = 3L,
    lda = 2L,
    out_dims = list(2L, 1L),
    scope = scope,
    hoist = hoist
  )
  expect_identical(out_gemv@value@dims, list(2L, 1L))

  out_syrk <- quickr:::syrk(
    trans = "T",
    X = A,
    scope = scope,
    hoist = hoist
  )
  expect_identical(out_syrk@value@dims, list(3L, 3L))

  y <- quickr:::Fortran(
    "y",
    quickr:::Variable("double", list(4L), name = "y"),
    r = quote(y)
  )
  out_outer <- quickr:::outer_mul(x, y, scope = scope, hoist = hoist)
  expect_identical(out_outer@value@dims, list(3L, 4L))

  A_tri <- quickr:::Fortran(
    "L",
    quickr:::Variable("double", list(2L, 2L), name = "L"),
    r = quote(L)
  )
  b <- quickr:::Fortran(
    "b",
    quickr:::Variable("double", list(2L), name = "b"),
    r = quote(b)
  )
  out_tri <- quickr:::triangular_solve(
    A_tri,
    b,
    uplo = "L",
    trans = "N",
    diag = "N",
    scope = scope,
    hoist = hoist
  )
  expect_identical(out_tri@value@dims, list(2L))
})

test_that("triangular_solve rejects scalar right-hand sides", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  A <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 2L), name = "A"),
    r = quote(A)
  )
  b_scalar <- quickr:::Fortran(
    "b",
    quickr:::Variable("double", name = "b"),
    r = quote(b)
  )

  expect_error(
    quickr:::triangular_solve(
      A,
      b_scalar,
      uplo = "L",
      trans = "N",
      diag = "N",
      scope = scope,
      hoist = hoist
    ),
    "expects a vector or matrix right-hand side"
  )
})

test_that("lapack_solve validates right-hand side and conformability", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  A <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 2L), name = "A"),
    r = quote(A)
  )
  b_scalar <- quickr:::Fortran("b", quickr:::Variable("double"), r = quote(b))
  b_vec <- quickr:::Fortran(
    "b",
    quickr:::Variable("double", list(2L), name = "b"),
    r = quote(b)
  )
  b_bad <- quickr:::Fortran(
    "b",
    quickr:::Variable("double", list(3L), name = "b"),
    r = quote(b)
  )

  expect_error(
    quickr:::lapack_solve(
      A,
      b_scalar,
      scope = scope,
      hoist = hoist,
      context = "solve"
    ),
    "expects a vector or matrix right-hand side"
  )
  expect_error(
    quickr:::lapack_solve(
      A,
      b_bad,
      scope = scope,
      hoist = hoist,
      context = "solve"
    ),
    "non-conformable arguments in solve"
  )
  out <- quickr:::lapack_solve(
    A,
    b_vec,
    scope = scope,
    hoist = hoist,
    context = "solve"
  )
  expect_identical(out@value@dims, list(2L))
})

test_that("lapack_solve warns on unknown conformability", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  A <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(quote(n), quote(n)), name = "A"),
    r = quote(A)
  )
  b_vec <- quickr:::Fortran(
    "b",
    quickr:::Variable("double", list(quote(m)), name = "b"),
    r = quote(b)
  )

  expect_warning(
    quickr:::lapack_solve(
      A,
      b_vec,
      scope = scope,
      hoist = hoist,
      context = "solve"
    ),
    "cannot verify conformability in solve"
  )
})

test_that("lapack_inverse and diag helpers allocate temporaries", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  A <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 2L), name = "A"),
    r = quote(A)
  )
  v <- quickr:::Fortran(
    "v",
    quickr:::Variable("double", list(3L), name = "v"),
    r = quote(v)
  )

  expect_error(
    quickr:::lapack_inverse(v, scope = scope, hoist = hoist, context = "solve"),
    "expects a matrix for `a`"
  )

  inv <- quickr:::lapack_inverse(A, scope = scope, hoist = hoist)
  expect_identical(inv@value@dims, list(2L, 2L))

  diag_out <- quickr:::diag_extract(A, scope = scope, hoist = hoist)
  expect_identical(diag_out@value@dims, list(2L))

  mat_out <- quickr:::diag_matrix(
    v,
    nrow = 3L,
    ncol = 3L,
    scope = scope,
    hoist = hoist
  )
  expect_identical(mat_out@value@dims, list(3L, 3L))
})

test_that("diag helpers validate input ranks", {
  scope <- quickr:::new_scope(NULL)
  hoist <- quickr:::new_hoist(scope)

  mat <- quickr:::Fortran(
    "A",
    quickr:::Variable("double", list(2L, 2L), name = "A"),
    r = quote(A)
  )
  vec <- quickr:::Fortran(
    "v",
    quickr:::Variable("double", list(2L), name = "v"),
    r = quote(v)
  )

  expect_error(
    quickr:::diag_extract(vec, scope = scope, hoist = hoist),
    "expects a matrix input"
  )
  expect_error(
    quickr:::diag_matrix(
      mat,
      nrow = 2L,
      ncol = 2L,
      scope = scope,
      hoist = hoist
    ),
    "expects a vector or scalar input"
  )
})

test_that("matrix inference covers triangular, solve, and diag variants", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("A", quickr:::Variable("double", list(2L, 2L), name = "A"))
  scope@assign("B", quickr:::Variable("double", list(2L, 3L), name = "B"))
  scope@assign("L", quickr:::Variable("double", list(2L, 2L), name = "L"))
  scope@assign("b", quickr:::Variable("double", list(2L), name = "b"))
  scope@assign("n", quickr:::Variable("integer", name = "n"))
  scope@assign("m", quickr:::Variable("integer", name = "m"))
  scope@assign("v", quickr:::Variable("double", list(3L), name = "v"))

  expect_null(quickr:::infer_dest_triangular(list(quote(L)), scope))
  out_tri <- quickr:::infer_dest_triangular(
    list(l = quote(L), x = quote(b)),
    scope
  )
  expect_identical(out_tri@dims, list(2L))

  out_solve <- quickr:::infer_dest_solve(list(quote(A)), scope)
  expect_identical(out_solve@dims, list(2L, 2L))

  out_solve_mat <- quickr:::infer_dest_solve(list(quote(A), quote(B)), scope)
  expect_identical(out_solve_mat@dims, list(2L, 3L))

  out_chol <- quickr:::infer_dest_chol(list(quote(A)), scope)
  expect_identical(out_chol@dims, list(2L, 2L))

  out_chol2 <- quickr:::infer_dest_chol2inv(list(quote(A)), scope)
  expect_identical(out_chol2@dims, list(2L, 2L))

  out_diag <- quickr:::infer_dest_diag(
    list(quote(v), quote(n), quote(m)),
    scope
  )
  expect_identical(out_diag@dims, list(quote(n), quote(m)))

  out_diag_named <- quickr:::infer_dest_diag(
    list(x = quote(v), nrow = quote(n), ncol = quote(m)),
    scope
  )
  expect_identical(out_diag_named@dims, list(quote(n), quote(m)))

  out_diag_nrow <- quickr:::infer_dest_diag(list(nrow = quote(n)), scope)
  expect_identical(out_diag_nrow@dims, list(quote(n), quote(n)))
})

test_that("matrix argument inference handles transposes and ranks", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("A", quickr:::Variable("double", list(2L, 3L), name = "A"))
  scope@assign("B", quickr:::Variable("double", list(3L, 4L), name = "B"))
  scope@assign("v", quickr:::Variable("double", list(3L), name = "v"))
  scope@assign("s", quickr:::Variable("double", name = "s"))
  scope@assign(
    "arr",
    quickr:::Variable("double", list(2L, 2L, 2L), name = "arr")
  )

  expect_identical(
    quickr:::infer_symbol_var(quote(A), scope),
    scope[["A"]]
  )
  expect_null(quickr:::infer_symbol_var(quote(A + 1), scope))

  info_mat <- quickr:::infer_matrix_arg(quote(t(A)), scope)
  expect_identical(info_mat$trans, "T")
  expect_identical(info_mat$var, scope[["A"]])

  info_vec <- quickr:::infer_matrix_arg(quote(t(v)), scope)
  expect_identical(info_vec$trans, "N")
  expect_identical(info_vec$var@dims, list(1L, 3L))

  info_scalar <- quickr:::infer_matrix_arg(quote(t(s)), scope)
  expect_identical(info_scalar$trans, "N")
  expect_identical(info_scalar$var, scope[["s"]])

  expect_null(quickr:::infer_matrix_arg(quote(t(arr)), scope))
  expect_null(quickr:::infer_matrix_arg(quote(t(missing)), scope))
  expect_null(quickr:::infer_matrix_arg(quote(missing), scope))

  info_plain <- quickr:::infer_matrix_arg(quote(B), scope)
  expect_identical(info_plain$trans, "N")
  expect_identical(info_plain$var, scope[["B"]])
})

test_that("matrix destination inference covers common shapes", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("A", quickr:::Variable("double", list(2L, 3L), name = "A"))
  scope@assign("B", quickr:::Variable("double", list(3L, 4L), name = "B"))
  scope@assign("v", quickr:::Variable("double", list(3L), name = "v"))
  scope@assign(
    "arr",
    quickr:::Variable("double", list(2L, 2L, 2L), name = "arr")
  )

  expect_null(quickr:::infer_dest_matmul(list(quote(A)), scope))
  expect_null(quickr:::infer_dest_matmul(list(quote(A), quote(missing)), scope))
  expect_null(quickr:::infer_dest_matmul(list(quote(A), quote(arr)), scope))

  out_mm <- quickr:::infer_dest_matmul(list(quote(A), quote(B)), scope)
  expect_identical(out_mm@dims, list(2L, 4L))

  out_mv <- quickr:::infer_dest_matmul(list(quote(A), quote(v)), scope)
  expect_identical(out_mv@dims, list(2L, 1L))

  out_vm <- quickr:::infer_dest_matmul(list(quote(v), quote(B)), scope)
  expect_identical(out_vm@dims, list(1L, 4L))

  out_t <- quickr:::infer_dest_matmul(list(quote(t(A)), quote(t(B))), scope)
  expect_identical(out_t@dims, list(3L, 3L))
})

test_that("infer_dest_solve handles NULL returns", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("A", quickr:::Variable("double", list(2L, 2L), name = "A"))
  scope@assign("v", quickr:::Variable("double", list(2L), name = "v"))
  scope@assign("s", quickr:::Variable("double", name = "s"))

  # NULL when a_arg is NULL (empty args with no names)
  expect_null(quickr:::infer_dest_solve(list(a = NULL), scope))

  # NULL when A is NULL
  expect_null(quickr:::infer_dest_solve(list(quote(missing)), scope))

  # NULL when A is not rank 2
  expect_null(quickr:::infer_dest_solve(list(quote(v)), scope))

  # NULL when b is NULL but can't infer
  expect_null(quickr:::infer_dest_solve(list(quote(A), quote(missing)), scope))

  # Returns Variable for rank 1 b
  out_vec <- quickr:::infer_dest_solve(list(quote(A), quote(v)), scope)
  expect_identical(out_vec@dims, list(2L))

  # NULL when b is scalar (rank 0)
  expect_null(quickr:::infer_dest_solve(list(quote(A), quote(s)), scope))
})

test_that("infer_size handles NULL returns", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("n", quickr:::Variable("integer", name = "n"))
  scope@assign("v", quickr:::Variable("integer", list(2L), name = "v"))
  scope@assign("s", quickr:::Variable("integer", name = "s"))

  # NULL for missing args
  expect_null(quickr:::infer_size(NULL, scope))
  expect_null(quickr:::infer_size(quote(expr), scope))

  # Returns integer literal
  expect_identical(quickr:::infer_size(3L, scope), 3L)
  expect_identical(quickr:::infer_size(3.0, scope), 3L)

  # Returns symbol for scalar variable
  expect_identical(quickr:::infer_size(quote(s), scope), quote(s))

  # NULL for non-scalar variable
  expect_null(quickr:::infer_size(quote(v), scope))

  # NULL for non-variable symbol
  expect_null(quickr:::infer_size(quote(missing), scope))
})

test_that("infer_dest_diag handles NULL returns for various cases", {
  scope <- quickr:::new_scope(NULL)
  scope@assign("A", quickr:::Variable("double", list(2L, 3L), name = "A"))
  scope@assign("v", quickr:::Variable("double", list(3L), name = "v"))
  scope@assign("s", quickr:::Variable("double", name = "s"))
  scope@assign("n", quickr:::Variable("integer", name = "n"))

  # NULL when x is missing and no nrow
  expect_null(quickr:::infer_dest_diag(list(), scope))

  # Returns vector when x is matrix (extracts diagonal)
  out_extract <- quickr:::infer_dest_diag(list(quote(A)), scope)
  expect_identical(out_extract@dims, list(2L))

  # Returns identity matrix from integer literal
  out_identity <- quickr:::infer_dest_diag(list(3L), scope)
  expect_identical(out_identity@dims, list(3L, 3L))

  # NULL when x is scalar symbol without nrow/ncol
  expect_null(quickr:::infer_dest_diag(list(quote(s)), scope))

  # Returns square matrix from vector
  out_vec <- quickr:::infer_dest_diag(list(quote(v)), scope)
  expect_identical(out_vec@dims, list(3L, 3L))

  # NULL when nrow is inferred but null
  expect_null(
    quickr:::infer_dest_diag(list(quote(v), nrow = quote(missing)), scope)
  )
})

test_that("assert_square_matrix detects non-square matrices", {
  expect_error(
    quickr:::assert_square_matrix(2L, 3L, "test"),
    "test requires a square matrix"
  )

  expect_invisible(quickr:::assert_square_matrix(2L, 2L, "test"))

  expect_warning(
    quickr:::assert_square_matrix(quote(n), quote(m), "test"),
    "cannot verify conformability in test"
  )
})
