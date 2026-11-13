# quick() supports mixed-case compiled names

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          x + 1
        }
      <environment: 0x0>
      attr(,"name")
      [1] "qSim"
    Code
      cat(fsub)
    Output
      subroutine qSim(x, out_, x__len_) bind(c, name = "qSim")
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void qSim(
        const double* const x__,
        double* const out___,
        const R_xlen_t x__len_);
      
      SEXP qSim_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        qSim(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

