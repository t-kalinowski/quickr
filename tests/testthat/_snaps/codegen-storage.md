# codegen: deferred vector fills allocate and resize

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)), type(out = double(NA)))
          out <- numeric(3)
          out <- numeric(n)
          out <- numeric(1)
          out[1L] <- 7
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:)
        ! manifest end
      
      
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (3)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(3))
        out = 0.0_c_double
        if (n < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (n)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(n))
        out = 0.0_c_double
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (1)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(1))
        out = 0.0_c_double
        out(1_c_int) = 7.0_c_double
        out_ = sum(out)
      
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
        const int* const n__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(n__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# codegen: deferred matrix fills resize axes and reuse storage

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)), type(out = double(NA, NA)))
          out <- matrix(2, 2, 3)
          first <- sum(out)
          out <- matrix(3, n, 2)
          second <- sum(out)
          out <- matrix(4, n, 2)
          first + second + sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        real(c_double) :: first
        real(c_double) :: second
        ! manifest end
      
      
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (2) .or. size(out, 2, kind=c_ptrdiff_t) /= (3)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(2, 3))
        out = 2.0_c_double
        first = sum(out)
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (n) .or. size(out, 2, kind=c_ptrdiff_t) /= (2)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(n, 2))
        out = 3.0_c_double
        second = sum(out)
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (n) .or. size(out, 2, kind=c_ptrdiff_t) /= (2)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(n, 2))
        out = 4.0_c_double
        out_ = ((first + second) + sum(out))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: deferred logical fills convert the interface value

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = logical(1)), type(out = logical(NA, NA)))
          out <- matrix(flag, 2, 3)
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: flag ! logical
        integer(c_int), intent(out) :: out_
      
        ! locals
        logical, allocatable :: out(:, :) ! logical
        ! manifest end
      
      
        block
          logical :: btmp1_ ! logical
      
          btmp1_ = (flag/=0)
          if (allocated(out)) then
            if (size(out, 1, kind=c_ptrdiff_t) /= (2) .or. size(out, 2, kind=c_ptrdiff_t) /= (3)) deallocate(out)
          end if
          if (.not. allocated(out)) allocate(out(2, 3))
          out = btmp1_
        end block
        out_ = sum(merge(1_c_int, 0_c_int, out))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const flag__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(flag__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: a deferred fill reads old storage before resizing

    Code
      fn
    Output
      function() {
          declare(type(out = double(NA, NA)))
          out <- matrix(1, 2, 2)
          out <- matrix(sum(out), 3, 3)
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        ! manifest end
      
      
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (2) .or. size(out, 2, kind=c_ptrdiff_t) /= (2)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(2, 2))
        out = 1.0_c_double
        block
          real(c_double) :: btmp1_
      
          btmp1_ = sum(out)
          if (allocated(out)) then
            if (size(out, 1, kind=c_ptrdiff_t) /= (3) .or. size(out, 2, kind=c_ptrdiff_t) /= (3)) deallocate(out)
          end if
          if (.not. allocated(out)) allocate(out(3, 3))
          out = btmp1_
        end block
        out_ = sum(out)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: reassignment checks independent stored extents

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(m)))
          x <- a
          x <- b
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, x, a__len_, b__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: x(a__len_)
        ! manifest end
      
      
        x = a
        if ((size(x, 1, kind=c_ptrdiff_t)) /= (size(b, 1, kind=c_ptrdiff_t))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `x`")
          return
        end if
        x = b
      
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
        double* const x__,
        const R_xlen_t a__len_,
        const R_xlen_t b__len_,
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
        
        const R_xlen_t x__len_ = a__len_;
        SEXP x = PROTECT(Rf_allocVector(REALSXP, x__len_));
        double* x__ = REAL(x);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          x__,
          a__len_,
          b__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return x;
      }

# codegen: shape-preserving reassignment reuses a local

    Code
      fn
    Output
      function(a) {
          declare(type(a = double(n)))
          x <- a
          x <- a * 2
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, x, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(out) :: x(a__len_)
        ! manifest end
      
      
        x = a
        x = (a * 2.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        double* const x__,
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
        
        const R_xlen_t x__len_ = a__len_;
        SEXP x = PROTECT(Rf_allocVector(REALSXP, x__len_));
        double* x__ = REAL(x);
        
        fn(a__, x__, a__len_);
        
        UNPROTECT(1);
        return x;
      }

# codegen: rebound dimension names do not replace stored extents

    Code
      fn
    Output
      function(n, m, x, y) {
          declare(
            type(n = integer(1)),
            type(m = integer(1)),
            type(x = double(n)),
            type(y = double(m))
          )
          n <- 1L
          x <- y
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, m, x, y, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in out) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(in out) :: x(n)
        real(c_double), intent(in) :: y(m)
        ! manifest end
      
      
        n = 1_c_int
        if ((size(x, 1, kind=c_ptrdiff_t)) /= (size(y, 1, kind=c_ptrdiff_t))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `x`")
          return
        end if
        x = y
      
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
        int* const n__,
        const int* const m__,
        double* const x__,
        const double* const y__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        n = Rf_duplicate(n);
        SETCAR(_args, n);
        int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
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
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != x__len_)
          Rf_error("length(x) must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)_as_int_n);
        const int _as_int_m = Rf_asInteger(m);
        if (_as_int_m != y__len_)
          Rf_error("length(y) must equal m,"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)_as_int_m);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          m__,
          x__,
          y__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        return x;
      }

# codegen: captured assignments use extents outside a shadowing formal

    Code
      fn
    Output
      function(n, m, x, y) {
          declare(
            type(n = integer(1)),
            type(m = integer(1)),
            type(x = double(n)),
            type(y = double(m))
          )
          replace <- function(n) {
            x <<- y
            NULL
          }
          replace(1L)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, m, x, y, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(in out) :: x(n)
        real(c_double), intent(in) :: y(m)
        ! manifest end
      
      
      
        call replace(1_c_int)
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine replace(n)
            use iso_c_binding, only: c_int, c_ptrdiff_t
            implicit none
      
            integer(c_int), intent(in) :: n
      
            if ((size(x, 1, kind=c_ptrdiff_t)) /= (size(y, 1, kind=c_ptrdiff_t))) then
              call quickr_set_error_msg("reassignment must preserve the shape of `x`")
              return
            end if
            x = y
          end subroutine
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
        const int* const n__,
        const int* const m__,
        double* const x__,
        const double* const y__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
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
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != x__len_)
          Rf_error("length(x) must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)_as_int_n);
        const int _as_int_m = Rf_asInteger(m);
        if (_as_int_m != y__len_)
          Rf_error("length(y) must equal m,"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)_as_int_m);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          m__,
          x__,
          y__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        return x;
      }

# codegen: captured copies outlive replacement of their source

    Code
      fn
    Output
      function(y) {
          declare(type(y = double(NA)), type(x = double(NA)))
          x <- y
          read <- function() {
            saved <- x
            x <<- c(10, 20)
            sum(saved)
          }
          read()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(y, out_, y__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! args
        real(c_double), intent(in) :: y(y__len_)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: x(:)
        ! manifest end
      
      
        x = y
      
        call read(out_)
      
        contains
          subroutine read(res)
            use iso_c_binding, only: c_double, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(out) :: res
            real(c_double) :: saved(size(x, 1, kind=c_ptrdiff_t))
      
            saved = x
            x = [ 10.0_c_double, 20.0_c_double ]
            res = sum(saved)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const y__,
        double* const out___,
        const R_xlen_t y__len_);
      
      SEXP fn_(SEXP _args) {
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(y__, out___, y__len_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: flattening a deferred local queries its allocated size

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA, NA)), type(m = double(NA, NA)))
          m <- x
          sum(as.vector(m))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: m(:, :)
        ! manifest end
      
      
        m = x
        out_ = sum(reshape(m, [size(m)]))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        double* const out___,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_);
      
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          out___,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: replacement ranges retain length-one axes

    Code
      fn
    Output
      function() {
          declare(type(out = double(2, 3, 2)), type(block = double(1, 3, 2)))
          out <- array(0, c(2, 3, 2))
          block <- array(1, c(1, 3, 2))
          for (i in seq_len(2L)) {
            out[i:i, , ] <- block
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out(2, 3, 2)
      
        ! locals
        real(c_double) :: block(1, 3, 2)
        integer(c_int) :: i
        ! manifest end
      
      
        out = 0.0_c_double
        block = 1.0_c_double
        do i = 1, 2_c_int
          out(i:i:sign(1, i-i), :, :) = block
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const out__);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out__len_ = (2) * (3) * (2);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 3;
          _dim[2] = 2;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(out__);
        
        UNPROTECT(2);
        return out;
      }

# codegen: vector replacement preserves an aliased right-hand side

    Code
      fn
    Output
      function(x, i) {
          declare(type(x = double(n)), type(i = integer(k)))
          out <- x
          out[i] <- out[rev(i)]
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, i, out, i__len_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: i__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: i(i__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = x
        out(i) = out(i(size(i):1:-1))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const i__,
        double* const out__,
        const R_xlen_t i__len_,
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
        
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          i__,
          out__,
          i__len_,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# codegen: replacement values are captured before computed indices

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3)))
          position <- 0L
          index <- function() {
            position <<- position + 1L
            position
          }
          value <- function() {
            position <<- 1L
            7
          }
          x[index()] <- value()
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x(3)
      
        ! locals
        integer(c_int) :: position
        ! manifest end
      
      
        position = 0_c_int
      
      
        block
          real(c_double) :: btmp1_
          integer(c_int) :: btmp2_
          real(c_double) :: btmp3_
      
          call value(btmp1_)
          btmp3_ = btmp1_
          call index(btmp2_)
          x(btmp2_) = btmp3_
        end block
      
        contains
          subroutine value(res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            real(c_double), intent(out) :: res
      
            position = 1_c_int
            res = 7.0_c_double
          end subroutine
          subroutine index(res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(out) :: res
      
            position = (position + 1_c_int)
            res = position
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const x__);
      
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
        
        fn(x__);
        
        return x;
      }

# codegen: scalar logical replacement writes only selected values

    Code
      fn
    Output
      function(x, mask) {
          declare(type(x = double(n)), type(mask = logical(n)))
          out <- x
          out[mask] <- 0
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: mask(x__len_) ! logical
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = x
        out(pack([(tmp1_, tmp1_=1, size((mask/=0)))], (mask/=0))) = 0.0_c_double
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const mask__,
        double* const out__,
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
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          mask__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# codegen: logical replacement vectors consume selected values in order

    Code
      fn
    Output
      function(x, mask, values) {
          declare(
            type(x = double(n)),
            type(mask = logical(n)),
            type(values = double(k))
          )
          out <- x
          out[mask] <- values
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, values, out, values__len_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: values__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: mask(x__len_) ! logical
        real(c_double), intent(in) :: values(values__len_)
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = x
        out(pack([(tmp1_, tmp1_=1, size((mask/=0)))], (mask/=0))) = values
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const mask__,
        const double* const values__,
        double* const out__,
        const R_xlen_t values__len_,
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
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        // values
        _args = CDR(_args);
        SEXP values = CAR(_args);
        if (TYPEOF(values) != REALSXP) {
          Rf_error("typeof(values) must be 'double', not '%s'", Rf_type2char(TYPEOF(values)));
        }
        const double* const values__ = REAL(values);
        const R_xlen_t values__len_ = Rf_xlength(values);
        
        if (x__len_ != mask__len_)
          Rf_error("length(mask) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)mask__len_, (double)x__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          mask__,
          values__,
          out__,
          values__len_,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# codegen: matrix replacement hoists vector indices

    Code
      fn
    Output
      function(out, block) {
          declare(type(out = integer(2, 3)), type(block = integer(2, 3)))
          out[seq_len(2L), rev(seq_len(3L))] <- block
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out, block) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: out(2, 3)
        integer(c_int), intent(in) :: block(2, 3)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_(3)
          integer(c_int) :: btmp2_(2, 3)
      
          btmp2_ = block
          btmp1_ = [ (tmp1_, tmp1_ = 1, 3_c_int) ]
          out(1:2_c_int, btmp1_(size(btmp1_):1:-1)) = btmp2_
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const out__, const int* const block__);
      
      SEXP fn_(SEXP _args) {
        // out
        _args = CDR(_args);
        SEXP out = CAR(_args);
        if (TYPEOF(out) != INTSXP) {
          Rf_error("typeof(out) must be 'integer', not '%s'", Rf_type2char(TYPEOF(out)));
        }
        out = Rf_duplicate(out);
        SETCAR(_args, out);
        int* const out__ = INTEGER(out);
        const int* const out__dim_ = ({
        SEXP dim_ = Rf_getAttrib(out, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "out must be a 2D-array, but length(dim(out)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int out__dim_1_ = out__dim_[0];
        const int out__dim_2_ = out__dim_[1];
        
        // block
        _args = CDR(_args);
        SEXP block = CAR(_args);
        if (TYPEOF(block) != INTSXP) {
          Rf_error("typeof(block) must be 'integer', not '%s'", Rf_type2char(TYPEOF(block)));
        }
        const int* const block__ = INTEGER(block);
        const int* const block__dim_ = ({
        SEXP dim_ = Rf_getAttrib(block, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "block must be a 2D-array, but length(dim(block)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int block__dim_1_ = block__dim_[0];
        const int block__dim_2_ = block__dim_[1];
        
        if (out__dim_1_ != 2)
          Rf_error("dim(out)[1] must be 2, not %0.f",
                    (double)out__dim_1_);
        if (out__dim_2_ != 3)
          Rf_error("dim(out)[2] must be 3, not %0.f",
                    (double)out__dim_2_);
        if (block__dim_1_ != 2)
          Rf_error("dim(block)[1] must be 2, not %0.f",
                    (double)block__dim_1_);
        if (block__dim_2_ != 3)
          Rf_error("dim(block)[2] must be 3, not %0.f",
                    (double)block__dim_2_);
        
        fn(out__, block__);
        
        return out;
      }

