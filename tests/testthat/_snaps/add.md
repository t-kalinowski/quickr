# add1

    Code
      slow_add1
    Output
      function(x) {
          declare(type(x = double(NA)))
          x <- x + 1
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_add1(x, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        ! manifest end
      
      
        x = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_add1(double* const x__, const R_xlen_t x__len_);
      
      SEXP slow_add1_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        
        slow_add1(x__, x__len_);
        
        return x;
      }

# add2

    Code
      slow_add2
    Output
      function(x, y) {
          declare(type(x = integer(n)), type(y = integer(n)))
          out <- x + y
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_add2(x, y, out, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: y(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = (x + y)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_add2(
        const int* const x__, 
        const int* const y__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP slow_add2_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != INTSXP) {
          Rf_error("typeof(y) must be 'integer', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const int* const y__ = INTEGER(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != y__len_)
          Rf_error("length(y) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        slow_add2(
          x__,
          y__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

