# case-sensitive variable name clashes

    Code
      quick(function(j) {
        declare(type(j = integer(1)))
        J <- double(length = j)
        J
      })
    Condition
      Error in `check_all_var_names_valid()`:
      ! Fortran is case-insensitive; these names conflict when mapped: `j -> j` and `J -> J`

# reserved or underscored names are rejected

    Code
      quick(function(x) {
        `_bad` <- x + 1L
        `_bad`
      })
    Condition
      Error in `check_all_var_names_valid()`:
      ! symbols cannot start or end with '_', but found: _bad

---

    Code
      quick(function(x) {
        bad_ <- x + 1L
        bad_
      })
    Condition
      Error in `check_all_var_names_valid()`:
      ! symbols cannot start or end with '_', but found: bad_

---

    Code
      quick(function(int) {
        int
      })
    Condition
      Error in `check_all_var_names_valid()`:
      ! symbols map to reserved Fortran names: int -> int

---

    Code
      quick(function(foo.bar, foo_bar) {
        1
      })
    Condition
      Error in `check_all_var_names_valid()`:
      ! Fortran is case-insensitive; these names conflict when mapped: `foo.bar -> foo_bar` and `foo_bar -> foo_bar`

