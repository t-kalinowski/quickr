# matrix

    Code
      r2f(fn)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: a
        integer(c_int), intent(in) :: b
        real(c_double), intent(out) :: out(a, b)
        ! manifest end
      
      
      
        out = 0.0_c_double
      end subroutine
      
      @r: function (a, b)
        {
            declare(type(a = integer(1)))
            declare(type(b = integer(1)))
            out <- matrix(0, a, b)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const int* const a__,
          const int* const b__,
          double* const out__);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != INTSXP) {
            Rf_error("typeof(a) must be 'integer', not '%s'", R_typeToChar(a));
          }
          const int* const a__ = INTEGER(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          // b
          _args = CDR(_args);
          SEXP b = CAR(_args);
          if (TYPEOF(b) != INTSXP) {
            Rf_error("typeof(b) must be 'integer', not '%s'", R_typeToChar(b));
          }
          const int* const b__ = INTEGER(b);
          const R_xlen_t b__len_ = Rf_xlength(b);
        
          if (a__len_ != 1)
            Rf_error("length(a) must be 1, not %0.f",
                      (double)a__len_);
          if (b__len_ != 1)
            Rf_error("length(b) must be 1, not %0.f",
                      (double)b__len_);
          const R_xlen_t out__len_ = (Rf_asInteger(a)) * (Rf_asInteger(b));
          SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
          double* out__ = REAL(out);
          {
            const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
            int* const _dim = INTEGER(_dim_sexp);
            _dim[0] = Rf_asInteger(a);
            _dim[1] = Rf_asInteger(b);
            Rf_dimgets(out, _dim_sexp);
          }
        
          fn(a__, b__, out__);
        
          UNPROTECT(2);
          return out;
        }

# reuse implicit size

    Code
      print(fsub)
    Output
      subroutine fn(a1, a2, out, a1__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a1__len_
      
        ! args
        real(c_double), intent(in) :: a1(a1__len_)
        real(c_double), intent(in) :: a2(a1__len_, a1__len_)
        real(c_double), intent(out) :: out(a1__len_)
        ! manifest end
      
      
      
        out = (a1 + a2(1_c_int, :))
      end subroutine
      
      @r: function (a1, a2)
        {
            declare(type(a1 = double(n)))
            declare(type(a2 = double(n, n)))
            out <- a1 + a2[1, ]
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const double* const a1__,
          const double* const a2__,
          double* const out__,
          const R_xlen_t a1__len_);
        
        SEXP fn_(SEXP _args) {
          // a1
          _args = CDR(_args);
          SEXP a1 = CAR(_args);
          if (TYPEOF(a1) != REALSXP) {
            Rf_error("typeof(a1) must be 'double', not '%s'", R_typeToChar(a1));
          }
          const double* const a1__ = REAL(a1);
          const R_xlen_t a1__len_ = Rf_xlength(a1);
        
          // a2
          _args = CDR(_args);
          SEXP a2 = CAR(_args);
          if (TYPEOF(a2) != REALSXP) {
            Rf_error("typeof(a2) must be 'double', not '%s'", R_typeToChar(a2));
          }
          const double* const a2__ = REAL(a2);
          const int* const a2__dim_ = ({
          SEXP dim_ = Rf_getAttrib(a2, R_DimSymbol);
          if (Rf_length(dim_) != 2) Rf_error(
            "a2 must be a 2D-array, but length(dim(a2)) is %i",
            (int) Rf_length(dim_));
          INTEGER(dim_);});
          const int a2__dim_1_ = a2__dim_[0];
          const int a2__dim_2_ = a2__dim_[1];
        
          if (a1__len_ != a2__dim_1_)
            Rf_error("dim(a2)[1] must equal length(a1),"
                     " but are %0.f and %0.f",
                      (double)a2__dim_1_, (double)a1__len_);
          if (a1__len_ != a2__dim_2_)
            Rf_error("dim(a2)[2] must equal length(a1),"
                     " but are %0.f and %0.f",
                      (double)a2__dim_2_, (double)a1__len_);
          const R_xlen_t out__len_ = a1__len_;
          SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
          double* out__ = REAL(out);
        
          fn(
            a1__,
            a2__,
            out__,
            a1__len_);
        
          UNPROTECT(1);
          return out;
        }
    Code
      cat(c_wrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a1__, 
        const double* const a2__, 
        double* const out__, 
        const R_xlen_t a1__len_);
      
      SEXP fn_(SEXP _args) {
        // a1
        _args = CDR(_args);
        SEXP a1 = CAR(_args);
        if (TYPEOF(a1) != REALSXP) {
          Rf_error("typeof(a1) must be 'double', not '%s'", R_typeToChar(a1));
        }
        const double* const a1__ = REAL(a1);
        const R_xlen_t a1__len_ = Rf_xlength(a1);
        
        // a2
        _args = CDR(_args);
        SEXP a2 = CAR(_args);
        if (TYPEOF(a2) != REALSXP) {
          Rf_error("typeof(a2) must be 'double', not '%s'", R_typeToChar(a2));
        }
        const double* const a2__ = REAL(a2);
        const int* const a2__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a2, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a2 must be a 2D-array, but length(dim(a2)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a2__dim_1_ = a2__dim_[0];
        const int a2__dim_2_ = a2__dim_[1];
        
        if (a1__len_ != a2__dim_1_)
          Rf_error("dim(a2)[1] must equal length(a1),"
                   " but are %0.f and %0.f",
                    (double)a2__dim_1_, (double)a1__len_);
        if (a1__len_ != a2__dim_2_)
          Rf_error("dim(a2)[2] must equal length(a1),"
                   " but are %0.f and %0.f",
                    (double)a2__dim_2_, (double)a1__len_);
        const R_xlen_t out__len_ = a1__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          a1__,
          a2__,
          out__,
          a1__len_);
        
        UNPROTECT(1);
        return out;
      }

