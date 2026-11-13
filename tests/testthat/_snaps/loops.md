# repeat/break

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          repeat {
            if (x >= 5L) {
              break
            }
            x <- x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do
          if ((x >= 5_c_int)) then
            exit
          end if
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# repeat + next

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          repeat {
            x <- x + 1L
            if (x < 0L) {
              next
            }
            if (x >= 5L) break
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do
          x = (x + 1_c_int)
          if ((x < 0_c_int)) then
            cycle
          end if
          if ((x >= 5_c_int)) then
            exit
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# break/for

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          for (i in 1:10) {
            x <- x + 1L
            if (x >= 5L) {
              break
            }
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1_c_int, 10_c_int, sign(1, 10_c_int-1_c_int)
          i = tmp1_
          x = (x + 1_c_int)
          if ((x >= 5_c_int)) then
            exit
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (x < 5L) {
            x <- x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while ((x < 5_c_int))
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while + next

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (x < 5L) {
            x <- x + 1L
            if (x < 0L) next
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while ((x < 5_c_int))
          x = (x + 1_c_int)
          if ((x < 0_c_int)) then
            cycle
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while + break

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (TRUE) {
            if (x >= 5L) {
              break
            }
            x <- x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while (.true.)
          if ((x >= 5_c_int)) then
            exit
          end if
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# expr return value

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA)))
          x + 1L
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out___,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# single-statement while/repeat bodies re-run their hoisted statements

    Code
      fn
    Output
      function(m) {
          declare(type(m = double(2, 2)))
          while (m[1, 1] < 100) m <- m %*% m
          m
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: m(2, 2)
        ! manifest end
      
      
        do while ((m(1_c_int, 1_c_int) < 100.0_c_double))
          block
            real(c_double) :: btmp1_(2, 2)
      
      call dgemm('N','N', int(2, kind=c_int), int(2, kind=c_int), int(2, kind=c_int), 1.0_c_double, m, int(2, kind=c_int), m, int(2,&
      & kind=c_int), 0.0_c_double, btmp1_, int(2, kind=c_int))
            m = btmp1_
          end block
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const m__);
      
      SEXP fn_(SEXP _args) {
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != REALSXP) {
          Rf_error("typeof(m) must be 'double', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        m = Rf_duplicate(m);
        SETCAR(_args, m);
        double* const m__ = REAL(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 2)
          Rf_error("dim(m)[1] must be 2, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 2)
          Rf_error("dim(m)[2] must be 2, not %0.f",
                    (double)m__dim_2_);
        
        fn(m__);
        
        return m;
      }

---

    Code
      fn
    Output
      function(m) {
          declare(type(m = double(2, 2)))
          repeat m <- m %*% m
          m
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: m(2, 2)
        ! manifest end
      
      
        do
          block
            real(c_double) :: btmp1_(2, 2)
      
      call dgemm('N','N', int(2, kind=c_int), int(2, kind=c_int), int(2, kind=c_int), 1.0_c_double, m, int(2, kind=c_int), m, int(2,&
      & kind=c_int), 0.0_c_double, btmp1_, int(2, kind=c_int))
            m = btmp1_
          end block
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const m__);
      
      SEXP fn_(SEXP _args) {
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != REALSXP) {
          Rf_error("typeof(m) must be 'double', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        m = Rf_duplicate(m);
        SETCAR(_args, m);
        double* const m__ = REAL(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 2)
          Rf_error("dim(m)[1] must be 2, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 2)
          Rf_error("dim(m)[2] must be 2, not %0.f",
                    (double)m__dim_2_);
        
        fn(m__);
        
        return m;
      }

# serial index loops leave the final iteration value

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          for (i in seq(1L, n)) {
            NULL
          }
          i
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, i) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: i
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1_c_int, n, sign(1, n-1_c_int)
          i = tmp1_
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const i__);
      
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
        const R_xlen_t i__len_ = (1);
        SEXP i = PROTECT(Rf_allocVector(INTSXP, i__len_));
        int* i__ = INTEGER(i);
        
        fn(n__, i__);
        
        UNPROTECT(1);
        return i;
      }

---

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          for (i in seq(1L, n, by = 2L)) {
            NULL
          }
          i
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, i, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: i
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if ((1_c_int /= n) .and. (2_c_int == 0_c_int)) then
          call quickr_set_error_msg("invalid '(to - from)/by'")
          return
        end if
        if (((n > 1_c_int) .and. (2_c_int < 0_c_int)) .or. ((n < 1_c_int) .and. (2_c_int > 0_c_int))) then
          call quickr_set_error_msg("wrong sign in 'by' argument")
          return
        end if
        do tmp1_ = 1_c_int, n, merge(int(2_c_int, kind=c_int), 1_c_int, 1_c_int /= n)
          i = tmp1_
        end do
      
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
        int* const i__,
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
        const R_xlen_t i__len_ = (1);
        SEXP i = PROTECT(Rf_allocVector(INTSXP, i__len_));
        int* i__ = INTEGER(i);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(n__, i__, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return i;
      }

---

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          for (i in rev(seq(1L, n, by = 2L))) {
            NULL
          }
          i
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, i, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: i
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if ((1_c_int /= n) .and. (2_c_int == 0_c_int)) then
          call quickr_set_error_msg("invalid '(to - from)/by'")
          return
        end if
        if (((n > 1_c_int) .and. (2_c_int < 0_c_int)) .or. ((n < 1_c_int) .and. (2_c_int > 0_c_int))) then
          call quickr_set_error_msg("wrong sign in 'by' argument")
          return
        end if
      do tmp1_ = 1_c_int + ((n - 1_c_int) / merge(int(2_c_int, kind=c_int), 1_c_int, 1_c_int /= n)) * merge(int(2_c_int, kind=c_int),&
      & 1_c_int, 1_c_int /= n), 1_c_int, (-merge(int(2_c_int, kind=c_int), 1_c_int, 1_c_int /= n))
          i = tmp1_
        end do
      
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
        int* const i__,
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
        const R_xlen_t i__len_ = (1);
        SEXP i = PROTECT(Rf_allocVector(INTSXP, i__len_));
        int* i__ = INTEGER(i);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(n__, i__, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return i;
      }

# serial index loops preserve next and break values

    Code
      fn
    Output
      function(n, stop_at) {
          declare(type(n = integer(1)), type(stop_at = integer(1)))
          for (i in seq_len(n)) {
            if (i == stop_at) {
              break
            }
            next
          }
          i
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, stop_at, i) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: stop_at
        integer(c_int), intent(out) :: i
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1, n
          i = tmp1_
          if ((i == stop_at)) then
            exit
          end if
          cycle
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const n__,
        const int* const stop_at__,
        int* const i__);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // stop_at
        _args = CDR(_args);
        SEXP stop_at = CAR(_args);
        if (TYPEOF(stop_at) != INTSXP) {
          Rf_error("typeof(stop_at) must be 'integer', not '%s'", Rf_type2char(TYPEOF(stop_at)));
        }
        const int* const stop_at__ = INTEGER(stop_at);
        const R_xlen_t stop_at__len_ = Rf_xlength(stop_at);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (stop_at__len_ != 1)
          Rf_error("length(stop_at) must be 1, not %0.f",
                    (double)stop_at__len_);
        const R_xlen_t i__len_ = (1);
        SEXP i = PROTECT(Rf_allocVector(INTSXP, i__len_));
        int* i__ = INTEGER(i);
        
        fn(n__, stop_at__, i__);
        
        UNPROTECT(1);
        return i;
      }

# serial index loops allow their R variable to be reassigned

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          total <- 0L
          for (i in seq_len(n)) {
            total <- total + i
            i <- i + 10L
          }
          c(total, i)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: out_(2)
      
        ! locals
        integer(c_int) :: total
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        total = 0_c_int
        do tmp1_ = 1, n
          i = tmp1_
          total = (total + i)
          i = (i + 10_c_int)
        end do
        out_ = [ total, i ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const out___);
      
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
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          total <- 0L
          for (i in seq_len(n)) {
            total <- total + i
            for (i in 3L:4L) {
              total <- total + i
            }
            total <- total + i
          }
          c(total, i)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: out_(2)
      
        ! locals
        integer(c_int) :: total
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        integer(c_int) :: tmp2_
        ! manifest end
      
      
        total = 0_c_int
        do tmp1_ = 1, n
          i = tmp1_
          total = (total + i)
          do tmp2_ = 3_c_int, 4_c_int, sign(1, 4_c_int-3_c_int)
            i = tmp2_
            total = (total + i)
          end do
          total = (total + i)
        end do
        out_ = [ total, i ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const out___);
      
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
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# empty index loops execute no iterations

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- 0L
          for (i in seq_along(x)) {
            out <- i
          }
          if (length(x) > 0L) {
            out <- i
          }
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
        integer(c_int), intent(out) :: out
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0_c_int
        do tmp1_ = 1, size(x)
          i = tmp1_
          out = i
        end do
        if ((size(x) > 0_c_int)) then
          out = i
        end if
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
        
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

