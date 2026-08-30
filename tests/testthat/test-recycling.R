# Elementwise conformability: R-style recycling is rejected (compile error
# for known mismatches, runtime guard for unknown), fill constructors spread
# inside c(), and matrix(scalar, m, n) materializes a real array.

test_that("known unequal vector lengths are a compile error", {
  # divisible lengths were previously blessed and silently mis-lowered
  divisible <- function(a, b) {
    declare(type(a = double(2)), type(b = double(4)))
    a + b
  }
  expect_error(quick(divisible), "equal lengths")

  ragged <- function(a, b) {
    declare(type(a = double(2)), type(b = double(3)))
    a * b
  }
  expect_error(quick(ragged), "equal lengths")

  zero_len <- function(a, b) {
    declare(type(a = double(0)), type(b = double(4)))
    a + b
  }
  expect_error(quick(zero_len), "equal lengths")
})

test_that("a known length-0 operand is rejected against an unknown length", {
  # R answers numeric(0) here; quickr has no length-0 result to return, so
  # the zero is rejected even when the other operand's length is not a
  # number the compiler can compare it to.
  fill_left <- function(x) {
    declare(type(x = double(n)))
    numeric(0) + x
  }
  expect_error(quick(fill_left), "equal lengths")

  fill_right <- function(x) {
    declare(type(x = double(n)))
    x > numeric(0)
  }
  expect_error(quick(fill_right), "equal lengths")

  declared <- function(a, b) {
    declare(type(a = double(0)), type(b = double(n)))
    a * b
  }
  expect_error(quick(declared), "equal lengths")

  # An NA dim is unknown, not "matches anything"
  unspecified <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(0)))
    a - b
  }
  expect_error(quick(unspecified), "equal lengths")
})

test_that("length checks cover comparisons, logical ops, and modulo", {
  comparison <- function(a, b) {
    declare(type(a = double(2)), type(b = double(4)))
    a < b
  }
  expect_error(quick(comparison), "equal lengths")

  logical_op <- function(a, b) {
    declare(type(a = logical(2)), type(b = logical(4)))
    a & b
  }
  expect_error(quick(logical_op), "equal lengths")

  modulo <- function(a, b) {
    declare(type(a = integer(2)), type(b = integer(4)))
    a %% b
  }
  expect_error(quick(modulo), "equal lengths")
})

test_that("symbolic differing lengths get a runtime guard", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    a + b
  }
  fsub <- as.character(r2f(fn))
  expect_match(fsub, "size(a, kind=c_ptrdiff_t)", fixed = TRUE)
  expect_match(fsub, "size(b, kind=c_ptrdiff_t)", fixed = TRUE)
  qfn <- quick(fn)
  expect_identical(qfn(c(1, 2), c(10, 20)), c(11, 22))
  # was: silent truncation to c(11, 22)
  expect_error(qfn(c(1, 2), c(10, 20, 30, 40)), "equal lengths")
  # Runtime length one does not change an assumed-shape vector into a scalar.
  expect_error(qfn(c(1, 2, 3), 10), "equal lengths")
  expect_error(qfn(10, c(1, 2, 3)), "equal lengths")
  expect_error(qfn(double(), double()), "equal lengths")
})

test_that("identical symbolic lengths guard runtime emptiness", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a - b
  }
  fsub <- as.character(r2f(fn))
  expect_match(fsub, "quickr_set_error_msg", fixed = TRUE)
  qfn <- quick(fn)
  expect_identical(qfn(c(1, 2, 3), c(10, 20, 30)), c(-9, -18, -27))
  expect_error(qfn(double(), double()), "equal lengths")
})

test_that("scalar broadcast preserves nonempty values", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(1)))
    a + b
  }
  fsub <- as.character(r2f(fn))
  expect_match(fsub, "size(a, 1, kind=c_ptrdiff_t) == 0", fixed = TRUE)
  expect_quick_identical(fn, list(c(1, 2, 3), 10))
})

test_that("scalar broadcast rejects empty array operands", {
  known_left <- function() {
    numeric(0) + 1
  }
  known_right <- function() {
    1 < numeric(0)
  }
  expect_error(quick(known_left), "equal lengths")
  expect_error(quick(known_right), "equal lengths")

  symbolic <- function(x) {
    declare(type(x = double(NA)))
    x + 1
  }
  qsymbolic <- quick(symbolic)
  expect_identical(qsymbolic(c(1, 2)), c(2, 3))
  expect_error(qsymbolic(double()), "equal lengths")
})

test_that("elementwise guards evaluate operands before reporting errors", {
  fn <- function(mat) {
    declare(type(mat = double(NA, NA)))
    mat + runif(3)
  }
  qfn <- quick(fn)
  mat <- matrix(as.double(1:4), 2, 2)

  set.seed(101)
  suppressWarnings(fn(mat))
  expected_next <- runif(1)
  set.seed(101)
  expect_error(qfn(mat), "matrix first dimension")
  expect_identical(runif(1), expected_next)

  conditional <- function(mask) {
    declare(type(mask = logical(NA)))
    ifelse(mask, runif(3), 0)
  }
  qconditional <- quick(conditional)
  mask <- c(TRUE, FALSE)
  set.seed(102)
  conditional(mask)
  expected_next <- runif(1)
  set.seed(102)
  expect_error(qconditional(mask), "shape of `test`")
  expect_identical(runif(1), expected_next)
})

test_that("nested elementwise operands preserve left-to-right evaluation", {
  mismatch <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    runif(2) + (a + b)
  }
  qmismatch <- quick(mismatch)
  set.seed(104)
  runif(2)
  expected_next <- runif(1)
  set.seed(104)
  expect_error(qmismatch(c(1, 2), c(1, 2, 3)), "equal lengths")
  expect_identical(runif(1), expected_next)

  conformable <- function() {
    runif(3) - (runif(3) * 10)
  }
  set.seed(105)
  expected <- conformable()
  set.seed(105)
  expect_identical(quick(conformable)(), expected)
})

test_that("later effects cannot change earlier elementwise operands", {
  bare <- function(x) {
    declare(type(x = double(1)))
    bump <- function() {
      x <<- x + 1
      2
    }
    x + bump()
  }

  expression <- function(x) {
    declare(type(x = double(1)))
    bump <- function() {
      x <<- x + 1
      2
    }
    (x * 2) + bump()
  }

  expect_quick_identical(bare, list(3))
  expect_quick_identical(expression, list(3))
})

test_that("c() fixes fill lengths before evaluating later arguments", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    bump <- function() {
      n <<- n + 1L
      7L
    }
    c(integer(n), bump())
  }

  expect_quick_identical(fn, list(2L))
})

test_that("subscript operands preserve left-to-right evaluation", {
  fn <- function(x) {
    declare(type(x = double(2, 2)))
    x[runif(1, 1, 3), runif(1, 1, 3) + 0]
  }
  qfn <- quick(fn)
  x <- matrix(c(11, 21, 12, 22), 2, 2)

  set.seed(106)
  expected <- fn(x)
  expected_next <- runif(1)
  set.seed(106)
  expect_identical(qfn(x), expected)
  expect_identical(runif(1), expected_next)
})

test_that("matrix-matrix elementwise ops guard unknown dims per axis", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, j)))
    a * b
  }
  qfn <- quick(fn)
  m1 <- matrix(as.double(1:6), 2, 3)
  m2 <- matrix(as.double(6:1), 2, 3)
  expect_identical(qfn(m1, m2), m1 * m2)
  expect_error(qfn(m1, t(m2)), "matching dimensions")
})

test_that("higher-rank elementwise arrays guard every axis", {
  fn <- function(a, b) {
    declare(type(a = double(NA, NA, NA)), type(b = double(NA, NA, NA)))
    a + b
  }
  qfn <- quick(fn)
  a <- array(as.double(1:8), c(2, 2, 2))
  b <- array(as.double(8:1), c(2, 2, 2))
  expect_identical(qfn(a, b), a + b)
  expect_error(
    qfn(a, array(as.double(1:12), c(2, 2, 3))),
    "matching dimensions"
  )
})

test_that("vector-matrix ops with unknown dims guard instead of rejecting", {
  fn <- function(vec, mat) {
    declare(type(vec = double(n)), type(mat = double(m, k)))
    vec + mat
  }
  qfn <- quick(fn)
  mat <- matrix(as.double(1:6), 2, 3)
  vec <- c(10, 20)
  expect_identical(qfn(vec, mat), vec + mat)
  expect_error(qfn(c(10, 20, 30), mat), "matrix first dimension")
})

test_that("vector-matrix ops reject zero-column results", {
  known <- function(vec, mat) {
    declare(type(vec = double(2)), type(mat = double(2, 0)))
    vec + mat
  }
  expect_error(quick(known), "matrix first dimension")

  symbolic <- function(vec, mat) {
    declare(type(vec = double(n)), type(mat = double(n, k)))
    vec + mat
  }
  qsymbolic <- quick(symbolic)
  expect_error(
    qsymbolic(c(1, 2), matrix(double(), 2, 0)),
    "matrix first dimension"
  )
})

test_that("expression vectors are evaluated once before matrix reshaping", {
  fn <- function(mat, n) {
    declare(type(mat = double(n, 2)), type(n = integer(1)))
    runif(n) + mat
  }
  mat <- matrix(as.double(1:6), 3, 2)
  set.seed(103)
  expected <- fn(mat, 3L)
  set.seed(103)
  expect_identical(quick(fn)(mat, 3L), expected)
})

test_that("known-shape expression operands stay fused", {
  fn <- function(a, b, c) {
    declare(
      type(a = double(3)),
      type(b = double(3)),
      type(c = double(3))
    )
    (a + b) + c
  }

  fsub <- r2f(fn)
  expect_false(grepl("btmp", as.character(fsub), fixed = TRUE))
  expect_quick_identical(
    fn,
    list(as.double(1:3), as.double(4:6), as.double(7:9))
  )
})

test_that("1x1 matrix operands follow R: arithmetic scalarizes, strict ops reject", {
  # Arithmetic: R recycles a length-1 array against a longer vector
  # (deprecated, hence suppressWarnings, but still R's answer). A 1x1
  # operand that needs a cast used to emit unindexable expression text
  # (`real(b, kind=c_double)(1, 1)`), a gfortran syntax error; it is now
  # hoisted to a temporary before subscripting.
  cast_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = logical(1, 1)))
    a + b
  }
  qfn <- quick(cast_fn)
  a <- c(1.5, 2.5, 3.5)
  b <- matrix(TRUE)
  expect_identical(qfn(a, b), suppressWarnings(cast_fn(a, b)))

  div_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = logical(1, 1)))
    a / b
  }
  qdiv <- quick(div_fn)
  expect_identical(qdiv(a, b), suppressWarnings(div_fn(a, b)))

  # Comparisons and & | do not get R's length-1 array recycling: R errors
  # ("dims [product 1] do not match the length of object"). Scalarizing
  # here would answer where R refuses, so the 1x1 is treated as an
  # ordinary one-row matrix and rejected.
  cmp_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = double(1, 1)))
    a < b
  }
  expect_error(quick(cmp_fn), "matrix first dimension")

  and_fn <- function(a, b) {
    declare(type(a = logical(3)), type(b = logical(1, 1)))
    a & b
  }
  expect_error(quick(and_fn), "matrix first dimension")

  # Unknown vector length against a 1x1: strict ops guard at runtime
  # (length 1 conforms, like R; anything longer is the R error above)
  sym_cmp <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(1, 1)))
    a < b
  }
  qcmp <- quick(sym_cmp)
  expect_identical(qcmp(3, matrix(5)), 3 < matrix(5))
  expect_error(qcmp(c(1, 2, 3), matrix(5)), "matrix first dimension")
})

test_that("1x1 matrix arithmetic rejects known empty vectors", {
  matrix_left <- function(m, x) {
    declare(type(m = double(1, 1)), type(x = double(0)))
    m + x
  }
  expect_error(quick(matrix_left), "matrix first dimension")

  matrix_right <- function(x, m) {
    declare(type(x = double(0)), type(m = double(1, 1)))
    x + m
  }
  expect_error(quick(matrix_right), "matrix first dimension")
})

test_that("constant vector dimensions participate in 1x1 scalarization", {
  fn <- function(x, m) {
    declare(type(x = double(1L + 2L)), type(m = double(1, 1)))
    x + m
  }
  x <- c(1, 2, 3)
  m <- matrix(4)
  expect_identical(quick(fn)(x, m), suppressWarnings(fn(x, m)))
})

test_that("1x1 matrix with a symbolic-length vector keeps R's shape", {
  # The result's shape depends on the runtime length: R keeps the 1x1
  # dims for a length-1 vector and drops them for any other length, so no
  # static decision can be right for both. Scalarizing regardless (the
  # old behavior) silently returned a dimensionless vector where R
  # returns a 1x1 matrix. Symbolic lengths now take the vector-matrix
  # rule instead: a runtime guard requires length 1 and the result is a
  # 1x1 matrix; longer vectors error where R would recycle (deprecated).
  fn <- function(m, x) {
    declare(type(m = double(1, 1)), type(x = double(n)))
    m + x
  }
  qfn <- quick(fn)
  expect_identical(qfn(matrix(2), 3), fn(matrix(2), 3))
  expect_error(qfn(matrix(2), c(1, 2, 3)), "matrix first dimension")

  rev_fn <- function(x, m) {
    declare(type(x = double(n)), type(m = double(1, 1)))
    x + m
  }
  qrev <- quick(rev_fn)
  expect_identical(qrev(3, matrix(2)), rev_fn(3, matrix(2)))
  expect_error(qrev(c(1, 2, 3), matrix(2)), "matrix first dimension")
})

test_that("fill constructors spread inside c()", {
  known <- function(x) {
    declare(type(x = double(3)))
    c(numeric(2), x)
  }
  expect_quick_identical(known, list(as.double(1:3)))

  symbolic <- function(x, k) {
    declare(type(x = double(3)), type(k = integer(1)))
    c(numeric(k), x)
  }
  expect_quick_identical(symbolic, list(as.double(1:3), 2L))
  expect_quick_identical(symbolic, list(as.double(1:3), 0L))
  expect_error(
    quick(symbolic)(as.double(1:3), -1L),
    "invalid 'length' argument",
    fixed = TRUE
  )

  promoted <- function(x) {
    declare(type(x = double(1)))
    c(integer(2), x)
  }
  expect_quick_identical(promoted, list(1.5))

  logical_fill <- function(x) {
    declare(type(x = logical(2)))
    c(logical(3), x)
  }
  expect_quick_identical(logical_fill, list(c(TRUE, FALSE)))

  parenthesized <- function(x) {
    declare(type(x = double(2)))
    c((numeric)(2), x)
  }
  expect_quick_identical(parenthesized, list(c(1, 2)))
})

test_that("parallel fill constructors privatize implied-do indices", {
  fn <- function(n) {
    declare(type(n = integer(1)), type(out = integer(n)))
    out <- integer(n)
    declare(parallel())
    for (i in seq_len(n)) {
      out[i] <- sum(c(integer(n), i))
    }
    out
  }

  fsub <- as.character(r2f(fn))
  expect_match(fsub, "!$omp parallel do private(tmp1_)", fixed = TRUE)
})

test_that("c() evaluates effectful arguments from left to right", {
  fn <- function() {
    c(runif(2), runif(2) + 1)
  }

  set.seed(913)
  expected <- fn()
  set.seed(913)
  expect_identical(quick(fn)(), expected)
})

test_that("symbolic fill spreading preserves pointer-sized lengths", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    c(numeric(length(x)), 1)
  }
  fsub <- as.character(r2f(fn))
  expect_match(fsub, "integer(c_ptrdiff_t) :: tmp1_", fixed = TRUE)
  expect_match(
    fsub,
    "tmp1_=1_c_ptrdiff_t, int(x__len_, kind=c_ptrdiff_t)",
    fixed = TRUE
  )
  expect_quick_identical(fn, list(c(2, 4, 6)))
})

test_that("array fill spreading preserves pointer-sized products", {
  fn <- function(k, n, m) {
    declare(
      type(k = integer(1)),
      type(n = integer(1)),
      type(m = integer(1))
    )
    array(numeric(k), dim = c(n, m))
  }
  fsub <- as.character(r2f(fn))
  expect_match(fsub, "integer(c_ptrdiff_t) :: tmp1_", fixed = TRUE)
  expect_match(
    fsub,
    paste0(
      "tmp1_=1_c_ptrdiff_t, ",
      "(int(n, kind=c_ptrdiff_t) * int(m, kind=c_ptrdiff_t))"
    ),
    fixed = TRUE
  )
  expect_quick_identical(fn, list(2L, 2L, 3L))
})

test_that("local closures can shadow fill constructors in c() and array()", {
  numeric_shadow <- function() {
    numeric <- function() c(1, 2)
    combined <- c(numeric(), 3)
    reshaped <- array(numeric(), dim = c(1L, 2L))
    list(combined = combined, reshaped = reshaped)
  }
  expect_quick_identical(numeric_shadow, list())

  integer_shadow <- function() {
    integer <- function() c(1L, 2L)
    combined <- c(integer(), 3L)
    reshaped <- array(integer(), dim = c(1L, 2L))
    list(combined = combined, reshaped = reshaped)
  }
  expect_quick_identical(integer_shadow, list())

  double_shadow <- function() {
    double <- function() c(1, 2)
    combined <- c(double(), 3)
    reshaped <- array(double(), dim = c(1L, 2L))
    list(combined = combined, reshaped = reshaped)
  }
  expect_quick_identical(double_shadow, list())

  logical_shadow <- function() {
    logical <- function() c(1L, 2L)
    combined <- c(logical(), 3L)
    reshaped <- array(logical(), dim = c(1L, 2L))
    list(combined = combined, reshaped = reshaped)
  }
  expect_quick_identical(logical_shadow, list())
})

test_that("fill constructors materialize where an array is required", {
  # A fill reaching c() through an expression is a real array, not a
  # scalar literal with claimed dims (which emitted one element where the
  # length arithmetic counted two).
  through_op <- function(x) {
    declare(type(x = double(2)))
    c(numeric(2) + 1, x)
  }
  expect_quick_identical(through_op, list(c(5, 6)))

  # Same leak as a silent wrong answer: sum() over a fill expression saw
  # one scalar instead of the filled length.
  reduced <- function() {
    sum(numeric(2) + 3)
  }
  expect_quick_identical(reduced, list())

  symbolic <- function(x, k) {
    declare(type(x = double(2)), type(k = integer(1)))
    c(integer(k) + 1L, x)
  }
  expect_quick_identical(symbolic, list(c(5, 6), 3L))
})

test_that("fill constructors materialize inside matrix()", {
  # matrix() lowers non-scalar data through reshape(), whose SOURCE must
  # be an array. Fills used to pass through as scalar literals with
  # claimed dims and relied on hoist_unless_name() to materialize them;
  # once that helper learned to skip literals, the generated
  # reshape(0.0_c_double, ...) failed to compile (and logical(k) only
  # kept working because the literal regex missed `.false.`). Fills now
  # materialize before matrix() like any other array consumer.
  numeric_fill <- function() {
    matrix(numeric(6), 3, 2)
  }
  expect_quick_identical(numeric_fill, list())

  integer_fill <- function() {
    matrix(integer(6), 3, 2)
  }
  expect_quick_identical(integer_fill, list())

  logical_fill <- function() {
    matrix(logical(6), 3, 2)
  }
  expect_quick_identical(logical_fill, list())

  assigned <- function() {
    x <- matrix(numeric(6), 3, 2)
    x
  }
  expect_quick_identical(assigned, list())
})

test_that("omitted fill lengths default to zero", {
  fn <- function() {
    c(numeric(), double(), integer(), logical(), 1, 2)
  }

  expect_quick_identical(fn, list())
})

test_that("array() rejects empty fills when the result would contain NA", {
  fn <- function() {
    array(numeric(), dim = c(1L, 2L))
  }

  qfn <- quick(fn)
  expect_error(
    qfn(),
    "NA values, which are not supported",
    fixed = TRUE
  )
})

test_that("array() recognizes parenthesized fill expressions", {
  fn <- function() {
    sum(array((numeric(2)), dim = 4))
  }

  expect_quick_identical(fn, list())
})

test_that("matrix() rejects empty fills when the result would contain NA", {
  fn <- function() {
    matrix(numeric(), nrow = 2L, ncol = 2L)
  }

  qfn <- quick(fn)
  expect_error(
    qfn(),
    "NA values, which are not supported",
    fixed = TRUE
  )
})

test_that("array() allows empty fills for empty results", {
  fn <- function() {
    array(numeric(), dim = c(0L, 2L))
  }

  expect_quick_identical(fn, list())
})

test_that("matrix() allows empty fills for empty results", {
  fn <- function() {
    matrix(numeric(), nrow = 0L, ncol = 2L)
  }

  expect_quick_identical(fn, list())
})

test_that("matrix(scalar, m, n) materializes where an array is required", {
  reduced <- function() {
    sum(matrix(2, 2, 3))
  }
  expect_quick_identical(reduced, list())

  transposed <- function() {
    t(matrix(1, 2, 3))
  }
  expect_quick_identical(transposed, list())
})

test_that("matrix() rejects negative extents before materializing", {
  static <- function() {
    sum(matrix(1, -1L, 2L))
  }
  dynamic <- function(n, k) {
    declare(type(n = integer(1)), type(k = integer(1)))
    sum(matrix(1, n, k))
  }

  expect_error(
    quick(static),
    "matrix() dimensions must be non-negative",
    fixed = TRUE
  )
  qdynamic <- quick(dynamic)
  expect_error(
    qdynamic(-1L, 2L),
    "matrix() dimensions must be non-negative",
    fixed = TRUE
  )
  expect_error(
    qdynamic(2L, -1L),
    "matrix() dimensions must be non-negative",
    fixed = TRUE
  )
  expect_quick_identical(dynamic, list(2L, 3L))
})

test_that("matrix() materializes direct non-scalar fill constructors", {
  fn <- function() {
    matrix(numeric(2), 2, 2)
  }
  expect_quick_identical(fn, list())
})

test_that("a closure's return expression materializes fills and matrix()", {
  # A local closure's return expression is compiled on its own, with no
  # enclosing call: the materialization decision sees an empty call stack,
  # so nothing is broadcasting, spreading, or padding the scalar-with-dims
  # form and it has to become a real array.
  fill <- function(x) {
    declare(type(x = double(3)))
    zeros <- function() numeric(3)
    x + zeros()
  }
  expect_quick_identical(fill, list(c(1, 2, 3)))

  mat <- function(x) {
    declare(type(x = double(2, 2)))
    ones <- function() matrix(1, 2, 2)
    x + ones()
  }
  expect_quick_identical(mat, list(matrix(as.double(1:4), 2, 2)))
})

test_that("matrix(scalar, m, n) keeps the broadcast fast path on assignment", {
  fn <- function(n, k) {
    declare(type(n = integer(1)), type(k = integer(1)))
    m <- matrix(0, n, k)
    m
  }
  fsub <- r2f(fn)
  # no hoisted temp: the scalar broadcasts straight into the target
  expect_match(fsub, "m = 0.0_c_double", fixed = TRUE)
  expect_quick_identical(fn, list(2L, 3L))
})

test_that("matrix(scalar, m, n) broadcasts natively in elementwise ops", {
  # Against a genuine rank-2 array the fill compiles to its scalar --
  # no O(m*n) temporary is materialized.
  broadcast <- function(x, n) {
    declare(type(x = double(n, n)), type(n = integer(1)))
    x + matrix(1, n, n)
  }
  expect_false(grepl("allocate", r2f(broadcast), fixed = TRUE))
  expect_quick_identical(broadcast, list(matrix(as.double(1:4), 2, 2), 2L))

  scalar_var_data <- function(x, s, n) {
    declare(type(x = double(n, n)), type(s = double(1)), type(n = integer(1)))
    x * matrix(s, n, n)
  }
  expect_quick_identical(
    scalar_var_data,
    list(matrix(as.double(1:4), 2, 2), 3, 2L)
  )

  # The claimed dims still participate in the conformability contract.
  static_mismatch <- function(x) {
    declare(type(x = double(2, 2)))
    x + matrix(1, 3, 3)
  }
  expect_error(quick(static_mismatch), "matching dimensions")

  symbolic <- function(x, k) {
    declare(type(x = double(2, 2)), type(k = integer(1)))
    x + matrix(1, k, k)
  }
  q_symbolic <- quick(symbolic)
  expect_error(
    q_symbolic(matrix(as.double(1:4), 2, 2), 3L),
    "matching dimensions"
  )
  expect_identical(
    q_symbolic(matrix(as.double(1:4), 2, 2), 2L),
    symbolic(matrix(as.double(1:4), 2, 2), 2L)
  )

  numeric_dim <- function(x, n) {
    declare(type(x = double(2, 2)), type(n = double(1)))
    x + matrix(1, n, 2)
  }
  expect_quick_identical(
    numeric_dim,
    list(matrix(as.double(1:4), 2, 2), 2.5)
  )

  # Two fills meeting each other still materialize (no scalar result
  # with claimed array dims may escape).
  both_fills <- function(n) {
    declare(type(n = integer(1)))
    sum(matrix(2, n, n) + matrix(3, n, n))
  }
  expect_quick_identical(both_fills, list(2L))

  # A vector operand keeps the vector-matrix reshape rule.
  vec_operand <- function(v) {
    declare(type(v = double(2)))
    v + matrix(1, 2, 3)
  }
  expect_quick_identical(vec_operand, list(c(1, 2)))
})

test_that("elementwise matrix fills respect a local matrix closure", {
  fn <- function(x) {
    declare(type(x = double(2, 2)))
    matrix <- function(data, nrow, ncol) data + 1
    matrix(1, 2, 2) + x
  }

  expect_quick_identical(fn, list(matrix(as.double(1:4), 2, 2)))
})

test_that("a left matrix fill is evaluated before its right operand", {
  fn <- function(x, s, n) {
    declare(
      type(x = double(2, 2)),
      type(s = double(1)),
      type(n = integer(1))
    )
    bump <- function() {
      s <<- s + 1
      n <<- n + 1L
      x
    }
    out <- matrix(s, n, n) + bump()
    out
  }

  expect_quick_identical(fn, list(matrix(as.double(1:4), 2, 2), 1, 2L))
})

test_that("a left matrix fill keeps dimensions captured before a scalar RHS", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    bump <- function() {
      n <<- n + 1L
      1
    }
    matrix(1, n, 1) + bump()
  }

  expect_quick_identical(fn, list(2L))
})

test_that("scalar-backed array expressions materialize before shape guards", {
  fn <- function(mat, n, k) {
    declare(
      type(mat = double(n, k)),
      type(n = integer(1)),
      type(k = integer(1))
    )
    array(0, dim = c(n, k)) + mat
  }
  mat <- matrix(as.double(1:6), 2, 3)
  expect_quick_identical(fn, list(mat, 2L, 3L))
})

test_that("constructor lengths follow current local bindings", {
  fn <- function() {
    n <- 2L
    n <- 3L
    double(n)
  }

  expect_identical(quick(fn)(), double(3))
})

test_that("matrix and array reject negative dimensions before use", {
  matrix_static <- function() {
    sum(matrix(1, -1L, 2L))
  }
  array_static <- function() {
    sum(array(1, dim = c(-1L, 2L)))
  }
  matrix_dynamic <- function(n) {
    declare(type(n = integer(1)))
    sum(matrix(1, n, 2L))
  }
  array_dynamic <- function(n) {
    declare(type(n = integer(1)))
    sum(array(1, dim = c(n, 2L)))
  }

  expect_error(quick(matrix_static), "dimensions must be non-negative")
  expect_error(quick(array_static), "dimensions must be non-negative")

  qmatrix <- quick(matrix_dynamic)
  qarray <- quick(array_dynamic)
  expect_identical(qmatrix(2L), 4.0)
  expect_identical(qarray(2L), 4.0)
  expect_error(qmatrix(-1L), "dimensions must be non-negative")
  expect_error(qarray(-1L), "dimensions must be non-negative")
})

test_that("matrix and array reject non-finite runtime dimensions", {
  expect_error(
    quick(function() {
      sum(matrix(1, Inf, 2L))
    }),
    "size must be finite",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(array(1, dim = c(Inf, 2L)))
    }),
    "size must be finite",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(matrix(1, NA_integer_, 2L))
    }),
    "dimensions must not be NA",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(array(1, dim = c(NA_real_, 2L)))
    }),
    "size must be an integer, found: NA",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(matrix(1, as.integer(Inf), 2L))
    }),
    "size must be finite",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(array(1, dim = c(as.integer(NaN), 2L)))
    }),
    "size must be finite",
    fixed = TRUE
  )
  expect_error(
    quick(function() {
      sum(array(1, dim = c(as.integer(2147483648), 2L)))
    }),
    "representable as an R integer",
    fixed = TRUE
  )

  matrix_dynamic <- function(n) {
    declare(type(n = double(1)))
    sum(matrix(1, n, 2L))
  }
  array_dynamic <- function(n) {
    declare(type(n = double(1)))
    sum(array(1, dim = c(n, 2L)))
  }

  matrix_code <- suppressWarnings(as.character(r2f(matrix_dynamic)))
  array_code <- suppressWarnings(as.character(r2f(array_dynamic)))
  expect_match(matrix_code, "huge(0_c_int)", fixed = TRUE)
  expect_match(array_code, "huge(0_c_int)", fixed = TRUE)
  expect_lt(
    regexpr("huge(0_c_int)", matrix_code, fixed = TRUE)[[1L]],
    regexpr("allocate(", matrix_code, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("huge(0_c_int)", array_code, fixed = TRUE)[[1L]],
    regexpr("allocate(", array_code, fixed = TRUE)[[1L]]
  )

  qmatrix <- suppressWarnings(quick(matrix_dynamic))
  qarray <- suppressWarnings(quick(array_dynamic))
  for (bad_dim in c(Inf, -Inf, NaN, 2147483648)) {
    expect_error(
      qmatrix(bad_dim),
      "representable as an R integer",
      fixed = TRUE
    )
    expect_error(
      qarray(bad_dim),
      "representable as an R integer",
      fixed = TRUE
    )
  }
  expect_identical(qmatrix(2), 4.0)
  expect_identical(qarray(2), 4.0)

  matrix_fill_dynamic <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(numeric(n), n, 1L)
    sum(out)
  }
  matrix_wrapped_fill_dynamic <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(c(numeric(n)), n, 1L)
    sum(out)
  }
  array_fill_dynamic <- function(n) {
    declare(type(n = double(1)))
    out <- array(numeric(n) + 0, dim = c(n))
    sum(out)
  }
  matrix_fill_dynamic_code <- suppressWarnings(
    as.character(r2f(matrix_fill_dynamic))
  )
  matrix_wrapped_fill_dynamic_code <- suppressWarnings(
    as.character(r2f(matrix_wrapped_fill_dynamic))
  )
  array_fill_dynamic_code <- suppressWarnings(
    as.character(r2f(array_fill_dynamic))
  )
  for (code in list(
    matrix_fill_dynamic_code,
    matrix_wrapped_fill_dynamic_code,
    array_fill_dynamic_code
  )) {
    expect_lt(
      regexpr("representable as an R integer", code, fixed = TRUE)[[1L]],
      regexpr("allocate(", code, fixed = TRUE)[[1L]]
    )
  }
  qmatrix_fill_dynamic <- suppressWarnings(quick(matrix_fill_dynamic))
  qmatrix_wrapped_fill_dynamic <- suppressWarnings(
    quick(matrix_wrapped_fill_dynamic)
  )
  qarray_fill_dynamic <- suppressWarnings(quick(array_fill_dynamic))
  expect_error(qmatrix_fill_dynamic(Inf), "representable as an R integer")
  expect_error(
    qmatrix_wrapped_fill_dynamic(Inf),
    "representable as an R integer"
  )
  expect_error(qarray_fill_dynamic(Inf), "representable as an R integer")
  expect_identical(qmatrix_fill_dynamic(2), 0.0)
  expect_identical(qmatrix_wrapped_fill_dynamic(2), 0.0)
  expect_identical(qarray_fill_dynamic(2), 0.0)

  fill_dynamic <- function(n) {
    declare(type(n = double(1)))
    out <- numeric(n)
    sum(out)
  }
  fill_dynamic_code <- suppressWarnings(as.character(r2f(fill_dynamic)))
  expect_lt(
    regexpr(
      "fill constructor dimensions must be finite",
      fill_dynamic_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(out", fill_dynamic_code, fixed = TRUE)[[1L]]
  )
  qfill_dynamic <- suppressWarnings(quick(fill_dynamic))
  expect_error(qfill_dynamic(Inf), "representable as an R integer")
  expect_identical(qfill_dynamic(2), 0.0)

  matrix_assigned <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(1, n, 2L)
    sum(out)
  }
  array_assigned <- function(n) {
    declare(type(n = double(1)))
    out <- array(1, dim = c(n, 2L))
    sum(out)
  }
  qmatrix_assigned <- suppressWarnings(quick(matrix_assigned))
  qarray_assigned <- suppressWarnings(quick(array_assigned))
  expect_error(qmatrix_assigned(Inf), "representable as an R integer")
  expect_error(qarray_assigned(Inf), "representable as an R integer")
  expect_error(qmatrix_assigned(-1), "dimensions must be non-negative")
  expect_error(qarray_assigned(-1), "dimensions must be non-negative")
  expect_identical(qmatrix_assigned(2), 4.0)
  expect_identical(qarray_assigned(2), 4.0)

  matrix_declared <- function(n) {
    declare(type(n = double(1)), type(out = double(n, 2L)))
    out <- matrix(1, n, 2L)
    sum(out)
  }
  array_declared <- function(n) {
    declare(type(n = double(1)), type(out = double(n, 2L, 1L)))
    out <- array(1, dim = c(n, 2L, 1L))
    sum(out)
  }
  matrix_declared_code <- suppressWarnings(as.character(r2f(matrix_declared)))
  array_declared_code <- suppressWarnings(as.character(r2f(array_declared)))
  expect_lt(
    regexpr(
      "representable as an R integer",
      matrix_declared_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(out", matrix_declared_code, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr(
      "representable as an R integer",
      array_declared_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(out", array_declared_code, fixed = TRUE)[[1L]]
  )
  qmatrix_declared <- suppressWarnings(quick(matrix_declared))
  qarray_declared <- suppressWarnings(quick(array_declared))
  expect_error(qmatrix_declared(Inf), "representable as an R integer")
  expect_error(qarray_declared(Inf), "representable as an R integer")
  expect_identical(qmatrix_declared(2), 4.0)
  expect_identical(qarray_declared(2), 4.0)

  matrix_assigned_integer <- function(n) {
    declare(type(n = integer(1)))
    out <- matrix(1, n, 2L)
    sum(out)
  }
  array_assigned_integer <- function(n) {
    declare(type(n = integer(1)))
    out <- array(1, dim = c(n, 2L))
    sum(out)
  }
  qmatrix_assigned_integer <- quick(matrix_assigned_integer)
  qarray_assigned_integer <- quick(array_assigned_integer)
  expect_error(
    qmatrix_assigned_integer(-1L),
    "dimensions must be non-negative"
  )
  expect_error(
    qarray_assigned_integer(-1L),
    "dimensions must be non-negative"
  )
  expect_identical(qmatrix_assigned_integer(2L), 4.0)
  expect_identical(qarray_assigned_integer(2L), 4.0)

  matrix_coerced <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(1, as.integer(n), 2L)
    sum(out)
  }
  array_coerced <- function(n) {
    declare(type(n = double(1)))
    out <- array(1, dim = c(as.integer(n), 2L))
    sum(out)
  }
  qmatrix_coerced <- suppressWarnings(quick(matrix_coerced))
  qarray_coerced <- suppressWarnings(quick(array_coerced))
  expect_error(qmatrix_coerced(Inf), "representable as an R integer")
  expect_error(qarray_coerced(Inf), "representable as an R integer")
  expect_error(qmatrix_coerced(-1), "dimensions must be non-negative")
  expect_error(qarray_coerced(-1), "dimensions must be non-negative")
  expect_identical(qmatrix_coerced(2), 4.0)
  expect_identical(qarray_coerced(2), 4.0)

  matrix_nested <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(1, n, 2L) + 0
    sum(out)
  }
  nested_code <- suppressWarnings(as.character(r2f(matrix_nested)))
  expect_lt(
    regexpr("representable as an R integer", nested_code, fixed = TRUE)[[1L]],
    regexpr("allocate(", nested_code, fixed = TRUE)[[1L]]
  )
  qmatrix_nested <- suppressWarnings(quick(matrix_nested))
  expect_error(qmatrix_nested(Inf), "representable as an R integer")
  expect_error(qmatrix_nested(-1), "dimensions must be non-negative")
  expect_identical(qmatrix_nested(2), 4.0)

  matrix_returned <- function(n) {
    declare(type(n = double(1)))
    out <- matrix(1, n, 2L)
    out
  }
  array_returned <- function(n) {
    declare(type(n = double(1)))
    out <- array(1, dim = c(n, 2L))
    out
  }
  array_vector_returned <- function(n) {
    declare(type(n = double(1)))
    out <- array(1, dim = c(n))
    out
  }
  array_vector_nested_returned <- function(n) {
    declare(type(n = double(1)))
    out <- array(1, dim = c(n)) + 0
    out
  }
  qmatrix_returned <- suppressWarnings(quick(matrix_returned))
  qarray_returned <- suppressWarnings(quick(array_returned))
  qarray_vector_returned <- suppressWarnings(quick(array_vector_returned))
  qarray_vector_nested_returned <- suppressWarnings(
    quick(array_vector_nested_returned)
  )
  expect_error(qmatrix_returned(Inf), "representable as an R integer")
  expect_error(qarray_returned(Inf), "representable as an R integer")
  expect_error(
    qarray_vector_returned(Inf),
    "representable as an R integer"
  )
  expect_error(
    qarray_vector_nested_returned(Inf),
    "representable as an R integer"
  )
  expect_identical(qmatrix_returned(2.5), matrix(1, 2.5, 2L))
  expect_identical(qarray_returned(2.5), array(1, dim = c(2.5, 2L)))
  expect_identical(qmatrix_returned(-0.5), matrix(1, -0.5, 2L))

  closure_matrix <- function(n) {
    declare(type(n = double(1)))
    make <- function() matrix(1, n, 2L)
    out <- make()
    sum(out)
  }
  closure_matrix_inline <- function(n) {
    declare(type(n = double(1)))
    make <- function() matrix(1, n, 2L)
    sum(make())
  }
  qclosure_matrix <- suppressWarnings(quick(closure_matrix))
  qclosure_matrix_inline <- suppressWarnings(quick(closure_matrix_inline))
  closure_code <- suppressWarnings(as.character(r2f(closure_matrix)))
  closure_inline_code <- suppressWarnings(
    as.character(r2f(closure_matrix_inline))
  )
  expect_lt(
    regexpr(
      "local closure result dimensions must be finite",
      closure_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(", closure_code, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr(
      "local closure result dimensions must be finite",
      closure_inline_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(", closure_inline_code, fixed = TRUE)[[1L]]
  )
  expect_error(qclosure_matrix(Inf), "representable as an R integer")
  expect_error(
    qclosure_matrix_inline(Inf),
    "representable as an R integer"
  )
  expect_identical(qclosure_matrix(2), 4.0)
  expect_identical(qclosure_matrix_inline(2), 4.0)

  closure_matrix_integer <- function(n) {
    declare(type(n = integer(1)))
    make <- function() matrix(1, n, 2L)
    out <- make()
    sum(out)
  }
  closure_matrix_integer_code <- as.character(r2f(closure_matrix_integer))
  expect_lt(
    regexpr(
      "local closure result dimensions must be non-negative",
      closure_matrix_integer_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(out", closure_matrix_integer_code, fixed = TRUE)[[1L]]
  )
  qclosure_matrix_integer <- quick(closure_matrix_integer)
  expect_error(qclosure_matrix_integer(-1L), "non-negative")
  expect_identical(qclosure_matrix_integer(2L), 4.0)

  closure_self_read <- function(n, x) {
    declare(type(n = double(1)), type(x = double(n, 2L)))
    make <- function(y) y + 1
    x <- make(x)
    sum(x)
  }
  closure_self_read_code <- suppressWarnings(
    as.character(r2f(closure_self_read))
  )
  expect_lt(
    regexpr(
      "local closure result dimensions must be finite",
      closure_self_read_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(btmp", closure_self_read_code, fixed = TRUE)[[1L]]
  )
  qclosure_self_read <- suppressWarnings(quick(closure_self_read))
  expect_identical(qclosure_self_read(2, matrix(1, 2L, 2L)), 8.0)

  sapply_matrix <- function(n) {
    declare(type(n = double(1)))
    out <- sapply(
      seq_len(1L),
      function(i) matrix(1, n, 2L),
      simplify = "array"
    )
    sum(out)
  }
  qsapply_matrix <- suppressWarnings(quick(sapply_matrix))
  sapply_code <- suppressWarnings(as.character(r2f(sapply_matrix)))
  expect_lt(
    regexpr(
      "sapply() output dimensions must be finite",
      sapply_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(", sapply_code, fixed = TRUE)[[1L]]
  )
  expect_error(qsapply_matrix(Inf), "representable as an R integer")
  expect_identical(qsapply_matrix(2), 4.0)

  sapply_integer <- function(n) {
    declare(type(n = integer(1)))
    out <- sapply(seq_len(n), function(i) i)
    sum(out)
  }
  sapply_integer_code <- as.character(r2f(sapply_integer))
  expect_lt(
    regexpr("must be non-negative", sapply_integer_code, fixed = TRUE)[[1L]],
    regexpr("allocate(out", sapply_integer_code, fixed = TRUE)[[1L]]
  )
  qsapply_integer <- quick(sapply_integer)
  expect_error(qsapply_integer(-1L), "non-negative")
  expect_identical(qsapply_integer(3L), 6L)

  sapply_self_read <- function(n, out) {
    declare(type(n = double(1)), type(out = double(n, 1L)))
    out <- sapply(
      seq_len(1L),
      function(i) out[, 1L],
      simplify = "array"
    )
    sum(out)
  }
  sapply_self_read_code <- suppressWarnings(
    as.character(r2f(sapply_self_read))
  )
  expect_lt(
    regexpr(
      "sapply() output dimensions must be finite",
      sapply_self_read_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(btmp", sapply_self_read_code, fixed = TRUE)[[1L]]
  )
  qsapply_self_read <- suppressWarnings(quick(sapply_self_read))
  expect_identical(qsapply_self_read(2, matrix(1, 2L, 1L)), 2.0)

  sapply_dynamic_iterable <- function(n) {
    declare(type(n = double(1)))
    x <- matrix(1, n, 2L)
    out <- sapply(x, function(i) i)
    sum(out)
  }
  sapply_dynamic_iterable_code <- suppressWarnings(
    as.character(r2f(sapply_dynamic_iterable))
  )
  expect_lt(
    regexpr(
      "representable as an R integer",
      sapply_dynamic_iterable_code,
      fixed = TRUE
    )[[1L]],
    regexpr("allocate(", sapply_dynamic_iterable_code, fixed = TRUE)[[1L]]
  )
  qsapply_dynamic_iterable <- suppressWarnings(
    quick(sapply_dynamic_iterable)
  )
  expect_error(
    qsapply_dynamic_iterable(Inf),
    "representable as an R integer"
  )
  expect_identical(qsapply_dynamic_iterable(2), 4.0)

  closure_formal_matrix <- function(n) {
    declare(type(n = double(1)))
    make <- function(k) matrix(1, k, 2L)
    sum(make(n))
  }
  expect_error(
    suppressWarnings(r2f(closure_formal_matrix)),
    "matrix() dimensions must not be NA",
    fixed = TRUE
  )

  integer_return <- function(n, m) {
    declare(type(n = integer(1)), type(m = integer(1)))
    out <- matrix(1, n, m)
    out
  }
  integer_return_code <- suppressWarnings(r2f(integer_return))
  expect_match(
    integer_return_code@c_bridge,
    "(?s)return dimensions must be non-negative.*const R_xlen_t out__len_",
    perl = TRUE
  )
  expect_match(
    integer_return_code@c_bridge,
    paste0(
      "\\(\\(R_xlen_t\\)\\(_as_int_n\\)\\).*",
      "\\(\\(R_xlen_t\\)\\(_as_int_m\\)\\)"
    ),
    perl = TRUE
  )
  qinteger_return <- quick(integer_return)
  expect_error(
    qinteger_return(-1L, -1L),
    "return dimensions must be non-negative",
    fixed = TRUE
  )
  expect_identical(qinteger_return(2L, 3L), matrix(1, 2L, 3L))
})

test_that("integer-backed logical matrix fills materialize portably", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    !matrix(rev(flag), 2, 2)
  }

  code <- as.character(r2f(fn))
  expect_match(
    code,
    "integer(c_int) :: btmp1_(2, 2) ! logical",
    fixed = TRUE
  )
  expect_quick_identical(fn, list(TRUE))
})

test_that("local closure arguments reject effectful expressions", {
  fn <- function() {
    subtract <- function(a, b) a - b
    subtract(runif(1), runif(1))
  }

  expect_error(
    quick(fn),
    "local closure calls only support pure argument expressions",
    fixed = TRUE
  )
})
