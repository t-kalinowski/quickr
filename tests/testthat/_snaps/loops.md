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
        ! manifest end
      
      
        do i = 1_c_int, 10_c_int, sign(1, 10_c_int-1_c_int)
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

