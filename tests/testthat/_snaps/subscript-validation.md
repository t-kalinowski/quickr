# dynamic x[a:b] bounds compile without runtime bounds guards

    Code
      fn
    Output
      function(x, n) {
          declare(type(x = double(NA)), type(n = integer(1)))
          x[1:n]
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, n, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_((abs((n - 1)) + 1))
        ! manifest end
      
      
        out_ = x(1_c_int:n:sign(1, n-1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const n__,
        double* const out___,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = ((((_as_int_n - 1)) < 0 ? -((_as_int_n - 1)) : ((_as_int_n - 1))) + 1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          n__,
          out___,
          x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# literal in-range bounds emit no guard; bad literals error at compile time

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(5)))
          x[2:4]
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(5)
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
        out_ = x(2_c_int:4_c_int:sign(1, 4_c_int-2_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 5)
          Rf_error("length(x) must be 5, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

