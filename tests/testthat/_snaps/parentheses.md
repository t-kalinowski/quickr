# parentheses preserve arithmetic precedence for scalars

    Code
      fn
    Output
      function(a, b, c, d) {
          declare(
            type(a = double(1)),
            type(b = double(1)),
            type(c = double(1)),
            type(d = double(1))
          )
      
          (a + b) / (c + d)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, c, d, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        real(c_double), intent(in) :: c
        real(c_double), intent(in) :: d
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = (((a + b)) / ((c + d)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        const double* const c__, 
        const double* const d__, 
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        // c
        _args = CDR(_args);
        SEXP c = CAR(_args);
        if (TYPEOF(c) != REALSXP) {
          Rf_error("typeof(c) must be 'double', not '%s'", Rf_type2char(TYPEOF(c)));
        }
        const double* const c__ = REAL(c);
        const R_xlen_t c__len_ = Rf_xlength(c);
        
        // d
        _args = CDR(_args);
        SEXP d = CAR(_args);
        if (TYPEOF(d) != REALSXP) {
          Rf_error("typeof(d) must be 'double', not '%s'", Rf_type2char(TYPEOF(d)));
        }
        const double* const d__ = REAL(d);
        const R_xlen_t d__len_ = Rf_xlength(d);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        if (c__len_ != 1)
          Rf_error("length(c) must be 1, not %0.f",
                    (double)c__len_);
        if (d__len_ != 1)
          Rf_error("length(d) must be 1, not %0.f",
                    (double)d__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          c__,
          d__,
          out___);
        
        UNPROTECT(1);
        return out_;
      }

# parentheses preserve precedence in vectorized expressions

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(n)))
          (x + 1) * (y - 1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(x__len_)
        real(c_double), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = (((x + 1.0_c_double)) * ((y - 1.0_c_double)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const double* const y__, 
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != y__len_)
          Rf_error("length(y) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__len_);
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          y__,
          out___,
          x__len_);
        
        UNPROTECT(1);
        return out_;
      }

