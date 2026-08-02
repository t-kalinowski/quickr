# %% and %/%

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          a %% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = modulo(a, b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        const double* const b__,
        double* const out___,
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
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

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          a %/% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:)
      
          allocate(btmp1_(a__len_))
          btmp1_ = (a / b)
          out_ = (aint(btmp1_) - merge(1.0_c_double, 0.0_c_double, (btmp1_ < aint(btmp1_))))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        const double* const b__,
        double* const out___,
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
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

# integer %/% is exact beyond 2^24

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = integer(1)), type(b = integer(1)))
          a %/% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: a
        integer(c_int), intent(in) :: b
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
        out_ = int(floor(real(a, kind=c_double) / real(b, kind=c_double)), kind=c_int)
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
        int* const out___);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != INTSXP) {
          Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = INTEGER(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != INTSXP) {
          Rf_error("typeof(b) must be 'integer', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const int* const b__ = INTEGER(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(a__, b__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# double %/% stays exact beyond integer range

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(1)), type(b = double(1)))
          a %/% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
      
          btmp1_ = (a / b)
          out_ = (aint(btmp1_) - merge(1.0_c_double, 0.0_c_double, (btmp1_ < aint(btmp1_))))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        const double* const b__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(a__, b__, out___);
        
        UNPROTECT(1);
        return out_;
      }

