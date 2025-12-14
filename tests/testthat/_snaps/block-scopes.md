# array-expression subscripting hoists into a block-scoped temp

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Fortran disallows (expr)(i,j); quickr uses a block-local temp array.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 4)))
          out <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(3, 4)
        real(c_double), intent(out) :: out
        ! manifest end
      
      
        block
          logical :: btmp1_(3, 4) ! logical
      
          btmp1_ = ((x > 0.0_c_double))
          out = merge(1.0_c_double, 0.0_c_double, btmp1_(2_c_int, 3_c_int))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        if (x__dim_1_ != 3)
          Rf_error("dim(x)[1] must be 3, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 4)
          Rf_error("dim(x)[2] must be 4, not %0.f",
                    (double)x__dim_2_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

