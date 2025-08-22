
This project is an R package that transpiles R functions to Fortran.

- To run the full test suite:
```sh
R -q -e 'testthat::test_local()'
```

- To run a single test file:
```sh
R -q -e 'devtools::test_active_file("tests/testthat/test-dims2f.R")'
```

- To see the generated C and Fortran code for an R function, use `r2f()`:
```sh
R --no-save -q <<'EOF'
devtools::load_all()
r2f(function(x) {
  declare(type(x = double(NA)))
  x + 1
})
EOF
```

- Never disable or skip tests.
