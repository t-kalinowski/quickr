# sapply reads named iterables without changing values or storage

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA, NA)))
          out <- sapply(x, function(value) value + 1L)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        integer(c_int), intent(in) :: x(x__dim_1_, x__dim_2_)
        integer(c_int), intent(out) :: out((x__dim_1_ * x__dim_2_))
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1_c_int, size(x)
      call closure1_(x((mod((int(tmp1_, kind=c_int) - 1_c_int), size(x, 1)) + 1_c_int), (((int(tmp1_, kind=c_int) - 1_c_int) / size(x,&
      & 1)) + 1_c_int)), out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: value
            integer(c_int), intent(out) :: res
      
      
            res = (value + 1_c_int)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        const R_xlen_t out__len_ = (x__dim_1_ * x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(1);
        return out;
      }

# sapply converts integer-backed logical values at the call site

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(NA)))
          out <- sapply(x, function(value) as.integer(!value))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1_c_int, size(x)
          call closure1_((x(tmp1_) /= 0_c_int), out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_int
            implicit none
      
            logical, intent(in) :: value ! logical
            integer(c_int), intent(out) :: res
      
      
            res = merge(1_c_int, 0_c_int, (.not. value))
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          flags <- x > 0
          out <- sapply(flags, function(value) as.integer(!value))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        logical, allocatable :: flags(:) ! logical
        integer(c_int) :: tmp1_
        ! manifest end
      
        allocate(flags(x__len_))
      
      
        flags = (x > 0.0_c_double)
        do tmp1_ = 1_c_int, size(flags)
          call closure1_(flags(tmp1_), out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_int
            implicit none
      
            logical, intent(in) :: value ! logical
            integer(c_int), intent(out) :: res
      
      
            res = merge(1_c_int, 0_c_int, (.not. value))
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        int* const out__,
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# sapply reads shared logical input in parallel

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(NA)))
          declare(parallel())
          out <- sapply(x, function(value) as.integer(!value))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
      
        !$omp parallel do
        do tmp1_ = 1_c_int, size(x)
          call closure1_((x(tmp1_) /= 0_c_int), out(tmp1_))
      
        end do
        !$omp end parallel do
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_int
            implicit none
      
            logical, intent(in) :: value ! logical
            integer(c_int), intent(out) :: res
      
      
            res = merge(1_c_int, 0_c_int, (.not. value))
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# sapply retains its input when the callback changes its binding

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          change <- function(value) {
            x[length(x)] <<- 99
            value
          }
          out <- sapply(x, change)
          list(out, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        real(c_double), allocatable :: tmp1_(:)
        integer(c_int) :: tmp2_
        ! manifest end
      
        allocate(tmp1_(x__len_))
      
      
      
        tmp1_ = x
        do tmp2_ = 1_c_int, size(tmp1_)
          call change(tmp1_(tmp2_), out(tmp2_))
      
        end do
      
      
        contains
          subroutine change(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
            x(size(x)) = 99.0_c_double
            res = value
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const x__,
        double* const out__,
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, out);
        SET_VECTOR_ELT(_ans, 1, x);
        UNPROTECT(2);
        return _ans;
      }

# sapply follows a formal alias through a nested writer

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          change <- function() {
            x[length(x)] <<- 99
            NULL
          }
          step <- function(value) {
            change()
            value
          }
          map <- function(values) {
            out <- sapply(values, step)
            out
          }
          out <- map(x)
          list(out, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
      
      
      
        call map(x, out)
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine change()
            use iso_c_binding, only: c_double
            implicit none
      
      
      
            x(size(x)) = 99.0_c_double
          end subroutine
          subroutine step(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
            call change()
            res = value
          end subroutine
          subroutine map(values, res)
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(in) :: values(:)
            real(c_double), intent(out) :: res(:)
            real(c_double) :: out(size(values, 1, kind=c_ptrdiff_t))
            real(c_double) :: ctmp1_(size(values, 1, kind=c_ptrdiff_t))
            integer(c_int) :: ctmp2_
      
            ctmp1_ = values
            do ctmp2_ = 1_c_int, size(ctmp1_)
              call step(ctmp1_(ctmp2_), out(ctmp2_))
      
            end do
      
      
            if (size(out, 1, kind=c_ptrdiff_t) /= size(res, 1, kind=c_ptrdiff_t)) then
              call quickr_set_error_msg("closure result shape does not match output shape")
              return
            end if
            res = out
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
        double* const x__,
        double* const out__,
        const R_xlen_t x__len_,
        char* quickr_err_msg);
      
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, out);
        SET_VECTOR_ELT(_ans, 1, x);
        UNPROTECT(2);
        return _ans;
      }

# sapply distinguishes shadowed bindings in callback writes

    Code
      fn
    Output
      function(x, other) {
          declare(type(x = double(NA)), type(other = double(NA)))
          step <- function(value) {
            x[length(x)] <<- 99
            value * 2
          }
          map <- function(x) {
            out <- sapply(x, step)
            out
          }
          out <- map(other)
          list(out, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, other, out, other__len_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: other__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(in) :: other(other__len_)
        real(c_double), intent(out) :: out(other__len_)
        ! manifest end
      
      
      
      
        call map(other, out)
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine step(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
            x(size(x)) = 99.0_c_double
            res = (value * 2.0_c_double)
          end subroutine
          subroutine map(x, res)
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(in) :: x(:)
            real(c_double), intent(out) :: res(:)
            real(c_double) :: out(size(x, 1, kind=c_ptrdiff_t))
            integer(c_int) :: ctmp1_
      
            do ctmp1_ = 1_c_int, size(x)
              call step(x(ctmp1_), out(ctmp1_))
      
            end do
      
      
            if (size(out, 1, kind=c_ptrdiff_t) /= size(res, 1, kind=c_ptrdiff_t)) then
              call quickr_set_error_msg("closure result shape does not match output shape")
              return
            end if
            res = out
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
        double* const x__,
        const double* const other__,
        double* const out__,
        const R_xlen_t other__len_,
        const R_xlen_t x__len_,
        char* quickr_err_msg);
      
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
        
        // other
        _args = CDR(_args);
        SEXP other = CAR(_args);
        if (TYPEOF(other) != REALSXP) {
          Rf_error("typeof(other) must be 'double', not '%s'", Rf_type2char(TYPEOF(other)));
        }
        const double* const other__ = REAL(other);
        const R_xlen_t other__len_ = Rf_xlength(other);
        
        const R_xlen_t out__len_ = other__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          other__,
          out__,
          other__len_,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, out);
        SET_VECTOR_ELT(_ans, 1, x);
        UNPROTECT(2);
        return _ans;
      }

# sapply retains its input when output replaces the same binding

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          x <- sapply(x, function(value) sum(x) + value)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
      
        ! locals
        real(c_double), allocatable :: tmp1_(:)
        integer(c_int) :: tmp2_
        ! manifest end
      
        allocate(tmp1_(x__len_))
      
      
        block
          real(c_double), allocatable :: btmp1_(:)
      
          allocate(btmp1_(x__len_))
          tmp1_ = x
          do tmp2_ = 1_c_int, size(tmp1_)
            call closure1_(tmp1_(tmp2_), btmp1_(tmp2_))
      
          end do
      
      
          x = btmp1_
        end block
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (sum(x) + value)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const x__, const R_xlen_t x__len_);
      
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
        
        
        fn(x__, x__len_);
        
        return x;
      }

# sapply preserves computed-iterable and callback RNG order

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)), type(out = double(n)))
          out <- sapply(runif(n), function(value) value + runif(1L))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out(n)
      
        ! locals
        integer(c_int) :: tmp1_
        real(c_double), allocatable :: tmp2_(:)
        integer(c_int) :: tmp3_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
        allocate(tmp2_(n))
      
      
        tmp2_ = [(unif_rand(), tmp1_=1, n)]
        do tmp3_ = 1_c_int, size(tmp2_)
          call closure1_(tmp2_(tmp3_), out(tmp3_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              btmp1_ = unif_rand()
              res = (value + btmp1_)
            end block
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(const int* const n__, double* const out__);
      
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
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out__len_ = _as_int_n;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        GetRNGstate();
        fn(n__, out__);
        PutRNGstate();
        
        UNPROTECT(1);
        return out;
      }

