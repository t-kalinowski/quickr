# floor() hoists non-name arguments to a temporary

    Code
      fn
    Output
      function() {
          out <- floor(runif(1L) * 10)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
        block
          real(c_double) :: btmp1_
      
          btmp1_ = (unif_rand() * 10.0_c_double)
          out = (aint(btmp1_) - merge(1.0_c_double, 0.0_c_double, (btmp1_ < aint(btmp1_))))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(double* const out__);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        GetRNGstate();
        fn(out__);
        PutRNGstate();
        
        UNPROTECT(1);
        return out;
      }

