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
            x = x + 1L
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
            x = x + 1L
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

