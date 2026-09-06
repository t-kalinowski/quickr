# drop() translation snapshots

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(1L, 3L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(1, 3)
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
        out_ = A(1, :)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const A__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        if (A__dim_1_ != 1)
          Rf_error("dim(A)[1] must be 1, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 3)
          Rf_error("dim(A)[2] must be 3, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(A__, out___);
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(3L, 1L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(3, 1)
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
        out_ = A(:, 1)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const A__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        if (A__dim_1_ != 3)
          Rf_error("dim(A)[1] must be 3, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 1)
          Rf_error("dim(A)[2] must be 1, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(A__, out___);
        
        UNPROTECT(1);
        return out_;
      }

