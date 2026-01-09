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
