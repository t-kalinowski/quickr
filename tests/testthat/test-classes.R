# Unit tests for internal S7 helpers/classes

test_that("prop helpers implement coercion, validation and set-once behavior", {
  Test <- S7::new_class(
    name = "Test",
    properties = list(
      s = quickr:::prop_string(
        default = NULL,
        allow_null = TRUE,
        coerce = TRUE,
        set_once = TRUE
      ),
      n = quickr:::prop_wholenumber(
        default = 1,
        allow_null = TRUE,
        coerce = TRUE
      ),
      e = quickr:::prop_enum(
        values = c("alpha", "beta"),
        nullable = TRUE,
        exact = FALSE
      )
    )
  )

  obj <- Test()

  obj@s <- 123
  expect_identical(obj@s, "123")
  expect_error(obj@s <- "again", "can only be set once")

  obj@n <- 2
  expect_identical(obj@n, 2L)
  expect_error(obj@n <- 2.5, "must be a whole number")

  obj@e <- "alp"
  expect_identical(obj@e, "alpha")
  obj@e <- "gamma"
  expect_error(S7::validate(obj), "must be either")
})

test_that("Variable dims setter accepts common forms", {
  v <- quickr:::Variable(name = "x", mode = "integer", dims = c(2, NA))
  expect_identical(v@dims, list(2L, NA_integer_))
  expect_identical(v@rank, 2L)

  v2 <- quickr:::Variable(name = quote(foo), mode = "double", dims = quote(n))
  expect_identical(v2@name, "foo")
  expect_true(is.symbol(v2@dims[[1L]]))

  v3 <- quickr:::Variable(name = "y", mode = "double", dims = quote(n + 1L))
  expect_true(is.call(v3@dims[[1L]]))

  expect_error(
    quickr:::Variable(name = "bad", mode = "integer", dims = list("x")),
    "@dims must be a list"
  )
})

test_that("Variable validator rejects non-logical logical_as_int", {
  expect_error(
    quickr:::Variable(mode = "integer", logical_as_int = TRUE),
    "logical_as_int"
  )
})

test_that("Fortran validates length and prints non-null properties", {
  expect_error(quickr:::Fortran(c("a", "b")), "length 1")

  v <- quickr:::Variable(name = "x", mode = "integer", dims = 1L)
  f <- quickr:::Fortran("x", value = v, r = quote(x))

  out <- capture.output(print(f))
  expect_true(any(grepl("@value:", out, fixed = TRUE)))
  expect_true(any(grepl("@r:", out, fixed = TRUE)))
})

test_that("new_setter returns NULL when no coercion or set_once", {
  result <- quickr:::new_setter(coerce = FALSE, set_once = FALSE)
  expect_null(result)
})

test_that("new_setter handles coerce = NULL correctly", {
  result <- quickr:::new_setter(coerce = NULL, set_once = FALSE)
  expect_null(result)
})

test_that("new_setter handles TRUE coercion", {
  Test <- S7::new_class(
    name = "TestCoerce",
    properties = list(
      x = quickr:::prop_string(
        default = NULL,
        allow_null = TRUE,
        coerce = TRUE
      )
    )
  )
  obj <- Test()
  obj@x <- 123
  expect_identical(obj@x, "123")
})

test_that("new_setter errors for invalid coerce argument", {
  expect_error(
    quickr:::new_setter(coerce = list()),
    "coerce must be TRUE, FALSE, NULL"
  )
})
