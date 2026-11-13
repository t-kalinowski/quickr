# scalar printers accept integer, double, and both logical storage forms

    Code
      fn
    Output
      function(i, x, flag) {
          declare(type(i = integer(1)), type(x = double(1)), type(flag = logical(1)))
          local_flag <- x > 0
          print(i)
          print(x)
          print(flag)
          print(local_flag)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(i, x, flag) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: i
        real(c_double), intent(in out) :: x
        integer(c_int), intent(in) :: flag ! logical
      
        ! locals
        logical :: local_flag ! logical
        ! manifest end
      
      
        local_flag = (x > 0.0_c_double)
        call intpr1("", 0, i)
        call dblepr1("", 0, x)
        call intpr1("", 0, flag)
        call intpr1("", 0, merge(1_c_int, 0_c_int, local_flag))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const i__,
        const double* const x__,
        const int* const flag__);
      
      SEXP fn_(SEXP _args) {
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (i__len_ != 1)
          Rf_error("length(i) must be 1, not %0.f",
                    (double)i__len_);
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        
        fn(i__, x__, flag__);
        
        return x;
      }

# array printers accept integer, double, and both logical storage forms

    Code
      fn
    Output
      function(i, x, flag) {
          declare(type(i = integer(n)), type(x = double(n)), type(flag = logical(n)))
          local_flag <- x > 0
          print(i)
          print(x)
          print(flag)
          print(local_flag)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(i, x, flag, i__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: i__len_
      
        ! args
        integer(c_int), intent(in) :: i(i__len_)
        real(c_double), intent(in out) :: x(i__len_)
        integer(c_int), intent(in) :: flag(i__len_) ! logical
      
        ! locals
        logical, allocatable :: local_flag(:) ! logical
        ! manifest end
      
        allocate(local_flag(i__len_))
      
      
        local_flag = (x > 0.0_c_double)
        call intpr("", 0, i, size(i))
        call dblepr("", 0, x, size(x))
        call intpr("", 0, flag, size(flag))
        call intpr("", 0, merge(1_c_int, 0_c_int, local_flag), size(merge(1_c_int, 0_c_int, local_flag)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const i__,
        const double* const x__,
        const int* const flag__,
        const R_xlen_t i__len_);
      
      SEXP fn_(SEXP _args) {
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (i__len_ != x__len_)
          Rf_error("length(x) must equal length(i),"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)i__len_);
        if (i__len_ != flag__len_)
          Rf_error("length(flag) must equal length(i),"
                   " but are %0.f and %0.f",
                    (double)flag__len_, (double)i__len_);
        
        fn(
          i__,
          x__,
          flag__,
          i__len_);
        
        return x;
      }

# cat prints literal labels with embedded quotes

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          cat("ready\n")
          cat("say \"hi\"\n")
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x
        ! manifest end
      
      
        call labelpr("ready", 5)
        call labelpr("say ""hi""", 8)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

