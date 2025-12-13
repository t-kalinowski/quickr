# scalar logical indexing works in ifelse()

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Regression: indexing external logical arrays must compile (no invalid subscripting of booleanized expressions).
    Code
      fn
    Output
      function(pred) {
          declare(type(pred = logical(3, 4)))
          out <- double(1)
          out[1] <- ifelse(pred[2, 3], 1.0, 0.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(pred, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: pred(3, 4) ! logical
        real(c_double), intent(out) :: out(1)
        ! manifest end
      
      
        out = 0
        out(1_c_int) = merge(1.0_c_double, 0.0_c_double, (pred(2_c_int, 3_c_int) /= 0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const pred__, double* const out__);
      
      SEXP fn_(SEXP _args) {
        // pred
        _args = CDR(_args);
        SEXP pred = CAR(_args);
        if (TYPEOF(pred) != LGLSXP) {
          Rf_error("typeof(pred) must be 'logical', not '%s'", Rf_type2char(TYPEOF(pred)));
        }
        const int* const pred__ = LOGICAL(pred);
        const int* const pred__dim_ = ({
        SEXP dim_ = Rf_getAttrib(pred, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "pred must be a 2D-array, but length(dim(pred)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int pred__dim_1_ = pred__dim_[0];
        const int pred__dim_2_ = pred__dim_[1];
        
        if (pred__dim_1_ != 3)
          Rf_error("dim(pred)[1] must be 3, not %0.f",
                    (double)pred__dim_1_);
        if (pred__dim_2_ != 4)
          Rf_error("dim(pred)[2] must be 4, not %0.f",
                    (double)pred__dim_2_);
        const R_xlen_t out__len_ = 1;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(pred__, out__);
        
        UNPROTECT(1);
        return out;
      }

# indexing a logical expression works (comparison)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Ensures indexing a logical expression compiles by hoisting the array expression to a temporary before subscripting (no invalid (expr)(i,j)).
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 4)))
          out <- double(1)
          out[1] <- ifelse(((((x > 0.0))))[2, 3], 1.0, 0.0)
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
        real(c_double), intent(out) :: out(1)
        ! manifest end
      
      
        out = 0
        block
          logical :: tmp1_(3, 4) ! logical
      
          tmp1_ = (((((x > 0.0_c_double)))))
          out(1_c_int) = merge(1.0_c_double, 0.0_c_double, tmp1_(2_c_int, 3_c_int))
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
        const R_xlen_t out__len_ = 1;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# indexing a logical expression works (boolean ops)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Ensures indexing a boolean expression compiles by hoisting to a temporary before subscripting (no invalid (expr)(i,j)).
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 4)))
          out <- double(1)
          out[1] <- ifelse(((((x > 0.0) & (x < 0.5))))[2, 3], 1.0, 0.0)
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
        real(c_double), intent(out) :: out(1)
        ! manifest end
      
      
        out = 0
        block
          logical :: tmp1_(3, 4) ! logical
      
          tmp1_ = (((((x > 0.0_c_double)) .and. ((x < 0.5_c_double)))))
          out(1_c_int) = merge(1.0_c_double, 0.0_c_double, tmp1_(2_c_int, 3_c_int))
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
        const R_xlen_t out__len_ = 1;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# expression indexing with logical mask avoids extra materialization

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Mask subsetting should stay as a single pack-like operation (avoid pushing mask into operands and duplicating mask evaluation).
    Code
      fn
    Output
      function(x, y, z, a) {
          declare(
            type(x = double(3, 4)),
            type(y = double(3, 4)),
            type(z = double(3, 4)),
            type(a = double(1))
          )
          out <- double(1)
          out[1] <- sum((x + y)[z > a])
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, a, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(3, 4)
        real(c_double), intent(in) :: y(3, 4)
        real(c_double), intent(in) :: z(3, 4)
        real(c_double), intent(in) :: a
        real(c_double), intent(out) :: out(1)
        ! manifest end
      
      
        out = 0
        out(1_c_int) = sum(((x + y)), mask = (z > a))
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
        const double* const z__, 
        const double* const a__, 
        double* const out__);
      
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const int* const y__dim_ = ({
        SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "y must be a 2D-array, but length(dim(y)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int y__dim_1_ = y__dim_[0];
        const int y__dim_2_ = y__dim_[1];
        
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != REALSXP) {
          Rf_error("typeof(z) must be 'double', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const double* const z__ = REAL(z);
        const int* const z__dim_ = ({
        SEXP dim_ = Rf_getAttrib(z, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "z must be a 2D-array, but length(dim(z)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int z__dim_1_ = z__dim_[0];
        const int z__dim_2_ = z__dim_[1];
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        if (x__dim_1_ != 3)
          Rf_error("dim(x)[1] must be 3, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 4)
          Rf_error("dim(x)[2] must be 4, not %0.f",
                    (double)x__dim_2_);
        if (y__dim_1_ != 3)
          Rf_error("dim(y)[1] must be 3, not %0.f",
                    (double)y__dim_1_);
        if (y__dim_2_ != 4)
          Rf_error("dim(y)[2] must be 4, not %0.f",
                    (double)y__dim_2_);
        if (z__dim_1_ != 3)
          Rf_error("dim(z)[1] must be 3, not %0.f",
                    (double)z__dim_1_);
        if (z__dim_2_ != 4)
          Rf_error("dim(z)[2] must be 4, not %0.f",
                    (double)z__dim_2_);
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        const R_xlen_t out__len_ = 1;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          y__,
          z__,
          a__,
          out__);
        
        UNPROTECT(1);
        return out;
      }

