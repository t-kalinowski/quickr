# ifelse promotes branches and shapes like test

    Code
      fn
    Output
      function(c, a) {
          declare(type(c = logical(n)), type(a = double(n)))
          ifelse(c, 1L, a)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(c, a, out_, c__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: c__len_
      
        ! args
        integer(c_int), intent(in) :: c(c__len_) ! logical
        real(c_double), intent(in) :: a(c__len_)
        real(c_double), intent(out) :: out_(c__len_)
        ! manifest end
      
      
        out_ = merge(real(1_c_int, kind=c_double), a, (c/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const c__, 
        const double* const a__, 
        double* const out___, 
        const R_xlen_t c__len_);
      
      SEXP fn_(SEXP _args) {
        // c
        _args = CDR(_args);
        SEXP c = CAR(_args);
        if (TYPEOF(c) != LGLSXP) {
          Rf_error("typeof(c) must be 'logical', not '%s'", Rf_type2char(TYPEOF(c)));
        }
        const int* const c__ = LOGICAL(c);
        const R_xlen_t c__len_ = Rf_xlength(c);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        if (c__len_ != a__len_)
          Rf_error("length(a) must equal length(c),"
                   " but are %0.f and %0.f",
                    (double)a__len_, (double)c__len_);
        const R_xlen_t out___len_ = c__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          c__,
          a__,
          out___,
          c__len_);
        
        UNPROTECT(1);
        return out_;
      }

