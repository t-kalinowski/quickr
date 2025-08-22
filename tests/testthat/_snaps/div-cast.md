# division casts integers

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = integer(n)), type(b = integer(n)))
          a / b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        integer(c_int), intent(in) :: a(a__len_)
        integer(c_int), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = (real(a, kind=c_double) / real(b, kind=c_double))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const a__, 
        const int* const b__, 
        double* const out___, 
        const R_xlen_t a__len_);
      
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
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

# division casts double and integer

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = integer(n)))
          a / b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        integer(c_int), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = (a / real(b, kind=c_double))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const int* const b__, 
        double* const out___, 
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
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != INTSXP) {
          Rf_error("typeof(b) must be 'integer', not '%s'", R_typeToChar(b));
        }
        const int* const b__ = INTEGER(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

# division casts logical

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = logical(n)))
          a / b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        integer(c_int), intent(in) :: b(a__len_) ! logical
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = (a / merge(1_c_double, 0_c_double, (b/=0)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const int* const b__, 
        double* const out___, 
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
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != LGLSXP) {
          Rf_error("typeof(b) must be 'logical', not '%s'", R_typeToChar(b));
        }
        const int* const b__ = LOGICAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

# division casts complex

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = complex(n)), type(b = complex(n)))
          a / b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        complex(c_double_complex), intent(in) :: a(a__len_)
        complex(c_double_complex), intent(in) :: b(a__len_)
        complex(c_double_complex), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = (a / b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const Rcomplex* const a__, 
        const Rcomplex* const b__, 
        Rcomplex* const out___, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != CPLXSXP) {
          Rf_error("typeof(a) must be 'complex', not '%s'", R_typeToChar(a));
        }
        const Rcomplex* const a__ = COMPLEX(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != CPLXSXP) {
          Rf_error("typeof(b) must be 'complex', not '%s'", R_typeToChar(b));
        }
        const Rcomplex* const b__ = COMPLEX(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(CPLXSXP, out___len_));
        Rcomplex* out___ = COMPLEX(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

