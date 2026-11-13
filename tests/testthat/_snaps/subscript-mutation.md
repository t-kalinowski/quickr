# subscript replacement preserves inputs that are not returned

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3)))
          x[2L] <- 9
          sum(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x(3)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        x(2_c_int) = 9.0_c_double
        out_ = sum(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const x__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 3)
          Rf_error("length(x) must be 3, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# logical replacement preserves the caller's vector and mask

    Code
      fn
    Output
      function(x, mask) {
          declare(type(x = double(n)), type(mask = logical(n)))
          x[mask] <- 0
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        integer(c_int), intent(in) :: mask(x__len_) ! logical
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        x(pack([(tmp1_, tmp1_=1, size((mask/=0)))], (mask/=0))) = 0.0_c_double
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const x__,
        const int* const mask__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != mask__len_)
          Rf_error("length(mask) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)mask__len_, (double)x__len_);
        
        fn(x__, mask__, x__len_);
        
        return x;
      }

# subscript replacement copies a modified argument before aliases read it

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(3)), type(y = double(3)))
          x[1L] <- x[1L] + 1
          list(x = x, y = y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x(3)
        real(c_double), intent(in out) :: y(3)
        ! manifest end
      
      
        x(1_c_int) = (x(1_c_int) + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const x__, const double* const y__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != 3)
          Rf_error("length(x) must be 3, not %0.f",
                    (double)x__len_);
        if (y__len_ != 3)
          Rf_error("length(y) must be 3, not %0.f",
                    (double)y__len_);
        
        fn(x__, y__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, x);
        SET_VECTOR_ELT(_ans, 1, y);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("x"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("y"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(2);
        return _ans;
      }

