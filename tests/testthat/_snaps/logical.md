# between

    Code
      fn
    Output
      function(x, left, right) {
          declare({
            type(x = double(n))
            type(left = double(1))
            type(right = double(1))
          })
          out <- x >= left & x <= right
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, left, right, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: left
        real(c_double), intent(in) :: right
        integer(c_int), intent(out) :: out(x__len_) ! logical
        ! manifest end
      
      
        out = (x >= left) .and. (x <= right)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const double* const left__, 
        const double* const right__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // left
        _args = CDR(_args);
        SEXP left = CAR(_args);
        if (TYPEOF(left) != REALSXP) {
          Rf_error("typeof(left) must be 'double', not '%s'", R_typeToChar(left));
        }
        const double* const left__ = REAL(left);
        const R_xlen_t left__len_ = Rf_xlength(left);
        
        // right
        _args = CDR(_args);
        SEXP right = CAR(_args);
        if (TYPEOF(right) != REALSXP) {
          Rf_error("typeof(right) must be 'double', not '%s'", R_typeToChar(right));
        }
        const double* const right__ = REAL(right);
        const R_xlen_t right__len_ = Rf_xlength(right);
        
        if (left__len_ != 1)
          Rf_error("length(left) must be 1, not %0.f",
                    (double)left__len_);
        if (right__len_ != 1)
          Rf_error("length(right) must be 1, not %0.f",
                    (double)right__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(
          x__,
          left__,
          right__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# logical ops

    Code
      fn
    Output
      function(a, b) {
          declare(
            type(a = double(1)),
            type(b = double(1))
          )
      
          delta <- a - b
          if (delta < 0) {
            delta <- (-1) * delta
          }
      
          a_gt_b <- a > b
          b_gt_a <- b > a
          delta_lt_3 <- delta <= 3
      
          out <- (a_gt_b || b_gt_a) && delta_lt_3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
      
        ! locals
        logical :: b_gt_a ! logical
        real(c_double) :: delta
        logical :: a_gt_b ! logical
        logical :: delta_lt_3 ! logical
        ! manifest end
      
      
        delta = (a - b)
        if ((delta < 0.0_c_double)) then
          delta = (-1.0_c_double * delta)
        end if
        a_gt_b = (a > b)
        b_gt_a = (b > a)
        delta_lt_3 = (delta <= 3.0_c_double)
        out = a_gt_b .or. b_gt_a .and. delta_lt_3
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
        int* const out__);
      
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
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare({
            type(a = double(1))
            type(b = double(1))
          })
      
          delta <- abs(a - b)
          out <- (a != b) & (delta <= 3)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
      
        ! locals
        real(c_double) :: delta
        ! manifest end
      
      
        delta = abs((a - b))
        out = (a /= b) .and. (delta <= 3.0_c_double)
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
        int* const out__);
      
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
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(1)), type(b = double(1)))
          out <- (a != b) && abs(a - b) <= 3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
        ! manifest end
      
      
        out = (a /= b) .and. (abs((a - b)) <= 3.0_c_double)
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
        int* const out__);
      
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
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          out <- (a != b) & abs(a - b) <= 3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        integer(c_int), intent(out) :: out(a__len_) ! logical
        ! manifest end
      
      
        out = (a /= b) .and. (abs((a - b)) <= 3.0_c_double)
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
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out__len_ = a__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(
          a__,
          b__,
          out__,
          a__len_);
        
        UNPROTECT(1);
        return out;
      }

