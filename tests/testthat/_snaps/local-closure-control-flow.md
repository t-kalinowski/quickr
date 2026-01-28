# local closures support NULL defaults via optional args

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          f <- function(a = NULL) {
            if (is.null(a)) {
              a <- x + 1
            }
            a * 2
          }
          f()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
        call f(res = out_)
      
        contains
          subroutine f(a, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in), optional :: a
            real(c_double), intent(out) :: res
            real(c_double) :: a__local_
      
            if (present(a)) then
              a__local_ = a
            end if
            if ((.not. present(a))) then
              a__local_ = (x + 1.0_c_double)
            end if
            res = (a__local_ * 2.0_c_double)
          end subroutine
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

