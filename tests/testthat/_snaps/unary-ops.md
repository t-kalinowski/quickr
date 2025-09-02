# logical not

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          lgl <- x > 1L
          not_lgl <- !lgl
          not_lgl
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, not_lgl, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: not_lgl(x__len_) ! logical
      
        ! locals
        logical :: lgl(x__len_) ! logical
        ! manifest end
      
      
        lgl = (x > 1_c_int)
        not_lgl = (.not. lgl)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__, 
        int* const not_lgl__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t not_lgl__len_ = x__len_;
        SEXP not_lgl = PROTECT(Rf_allocVector(LGLSXP, not_lgl__len_));
        int* not_lgl__ = LOGICAL(not_lgl);
        
        fn(x__, not_lgl__, x__len_);
        
        UNPROTECT(1);
        return not_lgl;
      }

# unary minus

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- -x
          y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (-x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__, 
        int* const y__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        UNPROTECT(1);
        return y;
      }

# unary plus

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- +x
          y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = x
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__, 
        int* const y__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        UNPROTECT(1);
        return y;
      }

