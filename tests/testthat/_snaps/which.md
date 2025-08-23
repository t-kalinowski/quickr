# which.max

    Code
      r2f(fn)
    Output
      subroutine fn(a, out, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        integer(c_int), intent(out) :: out
        ! manifest end
      
      
        out = maxloc(a, 1)
      end subroutine
      
      @r: function (a)
        {
            declare(type(a = double(NA)))
            out <- which.max(a)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const double* const a__,
          int* const out__,
          const R_xlen_t a__len_);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != REALSXP) {
            Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
          }
          const double* const a__ = REAL(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          const R_xlen_t out__len_ = (1);
          SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
          int* out__ = INTEGER(out);
        
          fn(a__, out__, a__len_);
        
          UNPROTECT(1);
          return out;
        }

---

    Code
      r2f(fn)
    Output
      subroutine fn(a, out, a__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        integer(c_int), intent(in) :: a(a__len_) ! logical
        integer(c_int), intent(out) :: out
        ! manifest end
      
      
        out = findloc((a/=0), .true., 1)
      end subroutine
      
      @r: function (a)
        {
            declare(type(a = logical(NA)))
            out <- which.max(a)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const int* const a__,
          int* const out__,
          const R_xlen_t a__len_);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != LGLSXP) {
            Rf_error("typeof(a) must be 'logical', not '%s'", R_typeToChar(a));
          }
          const int* const a__ = LOGICAL(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          const R_xlen_t out__len_ = (1);
          SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
          int* out__ = INTEGER(out);
        
          fn(a__, out__, a__len_);
        
          UNPROTECT(1);
          return out;
        }

# which.max/which.min

    Code
      fn
    Output
      function(lgl1, int1, dbl1) {
          declare(type(lgl1 = logical(NA)))
          declare(type(int1 = integer(NA)))
          declare(type(dbl1 = double(NA)))
          out <- c(
            which.min(lgl1),
            which.min(int1),
            which.min(dbl1),
            which.max(lgl1),
            which.max(int1),
            which.max(dbl1),
            which.max(dbl1[dbl1 < 0])
          )
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(lgl1, int1, dbl1, out, dbl1__len_, int1__len_, lgl1__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: lgl1__len_
        integer(c_ptrdiff_t), intent(in), value :: int1__len_
        integer(c_ptrdiff_t), intent(in), value :: dbl1__len_
      
        ! args
        integer(c_int), intent(in) :: lgl1(lgl1__len_) ! logical
        integer(c_int), intent(in) :: int1(int1__len_)
        real(c_double), intent(in) :: dbl1(dbl1__len_)
        integer(c_int), intent(out) :: out(7)
        ! manifest end
      
      
      
      
      out = [ findloc((lgl1/=0), .false., 1), minloc(int1, 1), minloc(dbl1, 1), findloc((lgl1/=0), .true., 1), maxloc(int1, 1), &
      maxloc(dbl1, 1), maxloc(pack(dbl1, (dbl1 < 0_c_int)), 1) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const lgl1__, 
        const int* const int1__, 
        const double* const dbl1__, 
        int* const out__, 
        const R_xlen_t dbl1__len_, 
        const R_xlen_t int1__len_, 
        const R_xlen_t lgl1__len_);
      
      SEXP fn_(SEXP _args) {
        // lgl1
        _args = CDR(_args);
        SEXP lgl1 = CAR(_args);
        if (TYPEOF(lgl1) != LGLSXP) {
          Rf_error("typeof(lgl1) must be 'logical', not '%s'", R_typeToChar(lgl1));
        }
        const int* const lgl1__ = LOGICAL(lgl1);
        const R_xlen_t lgl1__len_ = Rf_xlength(lgl1);
        
        // int1
        _args = CDR(_args);
        SEXP int1 = CAR(_args);
        if (TYPEOF(int1) != INTSXP) {
          Rf_error("typeof(int1) must be 'integer', not '%s'", R_typeToChar(int1));
        }
        const int* const int1__ = INTEGER(int1);
        const R_xlen_t int1__len_ = Rf_xlength(int1);
        
        // dbl1
        _args = CDR(_args);
        SEXP dbl1 = CAR(_args);
        if (TYPEOF(dbl1) != REALSXP) {
          Rf_error("typeof(dbl1) must be 'double', not '%s'", R_typeToChar(dbl1));
        }
        const double* const dbl1__ = REAL(dbl1);
        const R_xlen_t dbl1__len_ = Rf_xlength(dbl1);
        
        const R_xlen_t out__len_ = 7;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(
          lgl1__,
          int1__,
          dbl1__,
          out__,
          dbl1__len_,
          int1__len_,
          lgl1__len_);
        
        UNPROTECT(1);
        return out;
      }

