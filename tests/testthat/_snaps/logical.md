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
      subroutine fn(x, left, right, out, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: left
        real(c_double), intent(in) :: right
        integer(c_int), intent(out) :: out(x__len_) ! logical
        ! manifest end
      
      
        if (size(x, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        if (size(x, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
      if (size((x >= left), kind=c_ptrdiff_t) == 0 .or. size((x >= left), kind=c_ptrdiff_t) /= size((x <= right), kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        out = (x >= left) .and. (x <= right)
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
          end subroutine quickr_set_error_msg
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
        const R_xlen_t x__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // left
        _args = CDR(_args);
        SEXP left = CAR(_args);
        if (TYPEOF(left) != REALSXP) {
          Rf_error("typeof(left) must be 'double', not '%s'", Rf_type2char(TYPEOF(left)));
        }
        const double* const left__ = REAL(left);
        const R_xlen_t left__len_ = Rf_xlength(left);
        
        // right
        _args = CDR(_args);
        SEXP right = CAR(_args);
        if (TYPEOF(right) != REALSXP) {
          Rf_error("typeof(right) must be 'double', not '%s'", Rf_type2char(TYPEOF(right)));
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
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          left__,
          right__,
          out__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
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
        real(c_double) :: delta
        logical :: a_gt_b ! logical
        logical :: b_gt_a ! logical
        logical :: delta_lt_3 ! logical
        ! manifest end
      
      
        delta = (a - b)
        if ((delta < 0.0_c_double)) then
          delta = ((-1.0_c_double) * delta)
        end if
        a_gt_b = (a > b)
        b_gt_a = (b > a)
        delta_lt_3 = (delta <= 3.0_c_double)
        out = (a_gt_b .or. b_gt_a) .and. delta_lt_3
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
        out = ((a /= b)) .and. ((delta <= 3.0_c_double))
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
      
      
        out = ((a /= b)) .and. (abs((a - b)) <= 3.0_c_double)
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
      subroutine fn(a, b, out, a__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        integer(c_int), intent(out) :: out(a__len_) ! logical
        ! manifest end
      
      
        if (size(a, kind=c_ptrdiff_t) == 0 .or. size(a, kind=c_ptrdiff_t) /= size(b, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        if (size(a, kind=c_ptrdiff_t) == 0 .or. size(a, kind=c_ptrdiff_t) /= size(b, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        if (size(abs((a - b)), 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
      if (size(((a /= b)), kind=c_ptrdiff_t) == 0 .or. size(((a /= b)), kind=c_ptrdiff_t) /= size((abs((a - b)) <= 3.0_c_double),&
      & kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        out = ((a /= b)) .and. (abs((a - b)) <= 3.0_c_double)
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
          end subroutine quickr_set_error_msg
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
        const R_xlen_t a__len_, 
        char* quickr_err_msg);
      
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
        const R_xlen_t out__len_ = a__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out__,
          a__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# parentheses preserve logical precedence

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = integer(1)), type(y = integer(1)))
          cond <- (x > 8L || x <= 0L) && (y > 8L || y <= 0L)
          cond
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, cond) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: y
        integer(c_int), intent(out) :: cond ! logical
        ! manifest end
      
      
        cond = ((x > 8_c_int) .or. (x <= 0_c_int)) .and. ((y > 8_c_int) .or. (y <= 0_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__, 
        const int* const y__, 
        int* const cond__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != INTSXP) {
          Rf_error("typeof(y) must be 'integer', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const int* const y__ = INTEGER(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (y__len_ != 1)
          Rf_error("length(y) must be 1, not %0.f",
                    (double)y__len_);
        const R_xlen_t cond__len_ = (1);
        SEXP cond = PROTECT(Rf_allocVector(LGLSXP, cond__len_));
        int* cond__ = LOGICAL(cond);
        
        fn(x__, y__, cond__);
        
        UNPROTECT(1);
        return cond;
      }

---

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = integer(1)), type(y = integer(1)))
          cond_x <- x > 8L || x <= 0L
          cond_y <- y > 8L || y <= 0L
          cond_x && cond_y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: y
        integer(c_int), intent(out) :: out_ ! logical
      
        ! locals
        logical :: cond_x ! logical
        logical :: cond_y ! logical
        ! manifest end
      
      
        cond_x = (x > 8_c_int) .or. (x <= 0_c_int)
        cond_y = (y > 8_c_int) .or. (y <= 0_c_int)
        out_ = cond_x .and. cond_y
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__, 
        const int* const y__, 
        int* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != INTSXP) {
          Rf_error("typeof(y) must be 'integer', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const int* const y__ = INTEGER(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (y__len_ != 1)
          Rf_error("length(y) must be 1, not %0.f",
                    (double)y__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, y__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# short-circuited division is not evaluated eagerly

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          FALSE && (1 / x > 0)
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
        real(c_double), intent(in) :: x
        integer(c_int), intent(out) :: out_ ! logical
      
        ! locals
        logical :: tmp1_ ! logical
        ! manifest end
      
      
        tmp1_ = .false.
        if (tmp1_) then
          tmp1_ = (((1.0_c_double / x) > 0.0_c_double))
        end if
        out_ = tmp1_
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# && and || short-circuit like R's scalar operators

    Code
      fn
    Output
      function(i, x) {
          declare(type(i = integer(1)), type(x = double(3)))
          out <- 0
          if (i <= 3L && x[i] > 0) {
            out <- 1
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(i, x, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: i
        real(c_double), intent(in) :: x(3)
        real(c_double), intent(out) :: out
      
        ! locals
        logical :: tmp1_ ! logical
        ! manifest end
      
      
        out = 0.0_c_double
        tmp1_ = (i <= 3_c_int)
        if (tmp1_) then
          tmp1_ = (x(i) > 0.0_c_double)
        end if
        if (tmp1_) then
          out = 1.0_c_double
        end if
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const i__, 
        const double* const x__, 
        double* const out__);
      
      SEXP fn_(SEXP _args) {
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (i__len_ != 1)
          Rf_error("length(i) must be 1, not %0.f",
                    (double)i__len_);
        if (x__len_ != 3)
          Rf_error("length(x) must be 3, not %0.f",
                    (double)x__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(i__, x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# short-circuit results are private to parallel iterations

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(NA)), type(y = double(NA)))
          n <- length(x)
          out <- logical(n)
          declare(parallel())
          for (i in seq_along(x)) {
            out[i] <- x[i] > 0 && y[i] > 0
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(y__len_)
        integer(c_int), intent(out) :: out(x__len_) ! logical
      
        ! locals
        integer(c_int) :: n
        integer(c_int) :: i
        logical :: tmp1_ ! logical
        ! manifest end
      
      
        n = size(x)
        if (x__len_ < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = .false.
      
        !$omp parallel do private(tmp1_)
        do i = 1, size(x)
          tmp1_ = (x(i) > 0.0_c_double)
          if (tmp1_) then
            tmp1_ = (y(i) > 0.0_c_double)
          end if
          out(i) = tmp1_
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            !$omp critical (quickr_error)
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
            !$omp end critical (quickr_error)
          end subroutine quickr_set_error_msg
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
        int* const out__, 
        const R_xlen_t x__len_, 
        const R_xlen_t y__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          out__,
          x__len_,
          y__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# while re-evaluates hoisted condition code every iteration

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          i <- 1L
          n <- length(x)
          while (i <= n && x[i] > 0) {
            i <- i + 1L
          }
          i
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, i, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: i
      
        ! locals
        integer(c_int) :: n
        logical :: tmp1_ ! logical
        ! manifest end
      
      
        i = 1_c_int
        n = size(x)
        do
          tmp1_ = (i <= n)
          if (tmp1_) then
            tmp1_ = (x(i) > 0.0_c_double)
          end if
          if (.not. (tmp1_)) exit
          i = (i + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        int* const i__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t i__len_ = (1);
        SEXP i = PROTECT(Rf_allocVector(INTSXP, i__len_));
        int* i__ = INTEGER(i);
        
        fn(x__, i__, x__len_);
        
        UNPROTECT(1);
        return i;
      }

