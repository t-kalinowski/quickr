# roll_mean

    Code
      fn
    Output
      function(x, weights, normalize = TRUE) {
          declare(
            type(x = double(NA)),
            type(weights = double(NA)),
            type(normalize = logical(1))
          )
          out <- double(length(x) - length(weights) + 1)
          n <- length(weights)
          if (normalize) {
            weights <- weights / sum(weights) * length(weights)
          }
      
          for (i in seq_along(out)) {
            out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, weights, normalize, out, weights__len_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: weights__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in out) :: weights(weights__len_)
        integer(c_int), intent(in) :: normalize ! logical
        real(c_double), intent(out) :: out(((x__len_ - weights__len_) + 1))
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: n
        ! manifest end
      
      
        out = 0
        n = size(weights)
        if ((normalize/=0)) then
          weights = ((weights / sum(weights)) * size(weights))
        end if
        do i = 1, size(out)
          out(i) = (sum((x(i:((i + n) - 1_c_int):sign(1, ((i + n) - 1_c_int)-i)) * weights)) / real(size(weights), kind=c_double))
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        double* const weights__, 
        const int* const normalize__, 
        double* const out__, 
        const R_xlen_t weights__len_, 
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
        
        // weights
        _args = CDR(_args);
        SEXP weights = CAR(_args);
        if (TYPEOF(weights) != REALSXP) {
          Rf_error("typeof(weights) must be 'double', not '%s'", Rf_type2char(TYPEOF(weights)));
        }
        weights = Rf_duplicate(weights);
        SETCAR(_args, weights);
        double* const weights__ = REAL(weights);
        const R_xlen_t weights__len_ = Rf_xlength(weights);
        
        // normalize
        _args = CDR(_args);
        SEXP normalize = CAR(_args);
        if (TYPEOF(normalize) != LGLSXP) {
          Rf_error("typeof(normalize) must be 'logical', not '%s'", Rf_type2char(TYPEOF(normalize)));
        }
        const int* const normalize__ = LOGICAL(normalize);
        const R_xlen_t normalize__len_ = Rf_xlength(normalize);
        
        if (normalize__len_ != 1)
          Rf_error("length(normalize) must be 1, not %0.f",
                    (double)normalize__len_);
        const R_xlen_t out__len_ = ((x__len_ - weights__len_) + 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          weights__,
          normalize__,
          out__,
          weights__len_,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

