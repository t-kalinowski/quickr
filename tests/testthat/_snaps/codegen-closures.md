# closure calls lower safe numeric argument expressions

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          combine <- function(half, down, up, whole, magnitude) {
            c(half, down, up, whole, magnitude)
          }
          combine(x / 2, floor(x), ceiling(x), trunc(x), sqrt(abs(x)))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_(5)
        ! manifest end
      
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = 2.0_c_double
          btmp2_ = abs(x)
      call combine((x / btmp1_), (aint(x) - merge(1.0_c_double, 0.0_c_double, (x < aint(x)))), (aint(x) + merge(1.0_c_double,&
      & 0.0_c_double, (x > aint(x)))), aint(x), sqrt(btmp2_), out_)
        end block
      
        contains
          subroutine combine(half, down, up, whole, magnitude, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: half
            real(c_double), intent(in) :: down
            real(c_double), intent(in) :: up
            real(c_double), intent(in) :: whole
            real(c_double), intent(in) :: magnitude
            real(c_double), intent(out) :: res(:)
      
      
            res = [ half, down, up, whole, magnitude ]
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        const R_xlen_t out___len_ = 5;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# closure calls lower reductions and stored dimension queries

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA, NA)))
          combine <- function(total, product, size, rows, cols) {
            c(total, product, as.double(size), as.double(rows), as.double(cols))
          }
          combine(sum(x), prod(x), length(x), nrow(x), ncol(x))
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
        real(c_double), intent(out) :: out_(5)
        ! manifest end
      
      
      
        call combine(sum(x), product(x), size(x), size(x, 1), size(x, 2), out_)
      
        contains
          subroutine combine(total, product, size, rows, cols, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            real(c_double), intent(in) :: total
            real(c_double), intent(in) :: product
            integer(c_int), intent(in) :: size
            integer(c_int), intent(in) :: rows
            integer(c_int), intent(in) :: cols
            real(c_double), intent(out) :: res(:)
      
      
            res = [ total, product, real(size, kind=c_double), real(rows, kind=c_double), real(cols, kind=c_double) ]
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
        
        const R_xlen_t out___len_ = 5;
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

# unused safe closure arguments keep their inferred native types

    Code
      fn
    Output
      function(x, scale) {
          declare(type(x = integer(1)), type(scale = integer(1)))
          ignore <- function(value, literal) 3L
          ignore(x / scale, 1 / 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, scale, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: scale
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = real(scale, kind=c_double)
          btmp2_ = 0.0_c_double
          call ignore((real(x, kind=c_double) / btmp1_), (1.0_c_double / btmp2_), out_)
        end block
      
        contains
          subroutine ignore(value, literal, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(in) :: literal
            integer(c_int), intent(out) :: res
      
      
            res = 3_c_int
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
        const int* const scale__,
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
        
        // scale
        _args = CDR(_args);
        SEXP scale = CAR(_args);
        if (TYPEOF(scale) != INTSXP) {
          Rf_error("typeof(scale) must be 'integer', not '%s'", Rf_type2char(TYPEOF(scale)));
        }
        const int* const scale__ = INTEGER(scale);
        const R_xlen_t scale__len_ = Rf_xlength(scale);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (scale__len_ != 1)
          Rf_error("length(scale) must be 1, not %0.f",
                    (double)scale__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(x__, scale__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# root-defined closures retain captures through shadowing callers

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          identity_local <- function(value) value
          read <- function() identity_local(x)
          outer <- function(x) read()
          outer(2)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
      
      
        call outer(2.0_c_double, out_)
      
        contains
          subroutine identity_local(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = value
          end subroutine
          subroutine read(res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              call identity_local(x, btmp1_)
              res = btmp1_
            end block
          end subroutine
          subroutine outer(x, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: x
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              call read(btmp1_)
              res = btmp1_
            end block
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# closure defaults use lexical bindings through local shadows

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          read <- function(value = x + 1) value
          outer <- function(z) {
            x <- z + 99
            read()
          }
          outer(2)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
      
        call outer(2.0_c_double, out_)
      
        contains
          subroutine read(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = value
          end subroutine
          subroutine outer(z, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: z
            real(c_double), intent(out) :: res
            real(c_double) :: x__local_
      
            x__local_ = (z + 99.0_c_double)
            block
              real(c_double) :: btmp1_
      
              call read((x + 1.0_c_double), btmp1_)
              res = btmp1_
            end block
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# bare defaults preserve captured storage through a write

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          read <- function(value = x) {
            x <<- 8
            value
          }
          read()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
        call read(x, out_)
      
        contains
          subroutine read(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
            x = 8.0_c_double
            res = value
          end subroutine
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# compound actuals distinguish formal shadows from captured writes

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          change <- function(value) {
            x <<- 8
            value
          }
          outer <- function(x) change(x + 1)
          value <- outer(2)
          c(value, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in out) :: x
        real(c_double), intent(out) :: out_(2)
      
        ! locals
        real(c_double) :: value
        ! manifest end
      
      
      
      
        call outer(2.0_c_double, value)
      
        out_ = [ value, x ]
      
        contains
          subroutine change(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
            x = 8.0_c_double
            res = value
          end subroutine
          subroutine outer(x, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: x
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              call change((x + 1.0_c_double), btmp1_)
              res = btmp1_
            end block
          end subroutine
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# explicit forwarding keeps nested procedure arguments accessible

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          identity_local <- function(value) value
          outer <- function(x) {
            inner <- function(value) identity_local(value / 2)
            inner(x)
          }
          outer(2)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
      
        call outer(2.0_c_double, out_)
      
        contains
          subroutine identity_local(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = value
          end subroutine
          subroutine inner(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
              real(c_double) :: btmp2_
      
              btmp1_ = 2.0_c_double
              call identity_local((value / btmp1_), btmp2_)
              res = btmp2_
            end block
          end subroutine
          subroutine outer(x, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: x
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              call inner(x, btmp1_)
              res = btmp1_
            end block
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# optional closure arguments lower presence guards and keyword calls

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          f <- function(a = NULL, b = NULL) {
            if (is.null(a)) {
              a <- x + 1
            }
            if (is.null(b)) {
              b <- 2
            }
            a + b
          }
          c(f(), f(b = 5), f(a = 10, b = 5))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
          real(c_double) :: btmp3_
      
          call f(res = btmp1_)
          call f(b = 5.0_c_double, res = btmp2_)
          call f(10.0_c_double, 5.0_c_double, btmp3_)
          out_ = [ btmp1_, btmp2_, btmp3_ ]
        end block
      
        contains
          subroutine f(a, b, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in), optional :: a
            real(c_double), intent(in), optional :: b
            real(c_double), intent(out) :: res
            real(c_double) :: a__local_
            real(c_double) :: b__local_
      
            if (present(a)) then
              a__local_ = a
            end if
            if (present(b)) then
              b__local_ = b
            end if
            if ((.not. present(a))) then
              a__local_ = (x + 1.0_c_double)
            end if
            if ((.not. present(b))) then
              b__local_ = 2.0_c_double
            end if
            res = (a__local_ + b__local_)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# closure array formals copy before local modification

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          modify <- function(value) {
            value[1L] <- value[1L] + 1
            value
          }
          modified <- modify(x)
          list(original = x, modified = modified)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, modified, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(out) :: modified(x__len_)
        ! manifest end
      
      
      
        call modify(x, modified)
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine modify(value, res)
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(in) :: value(:)
            real(c_double), intent(out) :: res(:)
            real(c_double) :: value__local_(size(value, 1, kind=c_ptrdiff_t))
      
            value__local_ = value
            value__local_(1_c_int) = (value(1_c_int) + 1.0_c_double)
            if (size(value__local_, 1, kind=c_ptrdiff_t) /= size(res, 1, kind=c_ptrdiff_t)) then
              call quickr_set_error_msg("closure result shape does not match output shape")
              return
            end if
            res = value__local_
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
        const double* const x__,
        double* const modified__,
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
        
        const R_xlen_t modified__len_ = x__len_;
        SEXP modified = PROTECT(Rf_allocVector(REALSXP, modified__len_));
        double* modified__ = REAL(modified);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          modified__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, x);
        SET_VECTOR_ELT(_ans, 1, modified);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("original"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("modified"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(3);
        return _ans;
      }

# error-free closure chains omit the shared error buffer

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          add_one <- function(value) value + 1
          outer <- function(value) add_one(value)
          outer(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
      
        call outer(x, out_)
      
        contains
          subroutine add_one(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (value + 1.0_c_double)
          end subroutine
          subroutine outer(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double) :: btmp1_
      
              call add_one(value, btmp1_)
              res = btmp1_
            end block
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# logical closure results use C-compatible output storage

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          positive <- function(value) value > 0
          out <- positive(x)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        integer(c_int), intent(out) :: out ! logical
        ! manifest end
      
      
      
        call positive(x, out)
      
        contains
          subroutine positive(value, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            real(c_double), intent(in) :: value
            integer(c_int), intent(out) :: res ! logical
      
      
            res = (value > 0.0_c_double)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, int* const out__);
      
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
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# closure assignment captures arguments before replacing their target

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          add_one <- function(value) value + 1
          x <- add_one(x)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        ! manifest end
      
      
      
        block
          real(c_double), allocatable :: btmp1_(:)
      
          allocate(btmp1_(x__len_))
          call add_one(x, btmp1_)
          if (quickr_err_msg(1) /= c_null_char) return
          x = btmp1_
        end block
      
        contains
          subroutine add_one(value, res)
            use iso_c_binding, only: c_double, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(in) :: value(:)
            real(c_double), intent(out) :: res(:)
      
      
            if (size((value + 1.0_c_double), 1, kind=c_ptrdiff_t) /= size(res, 1, kind=c_ptrdiff_t)) then
              call quickr_set_error_msg("closure result shape does not match output shape")
              return
            end if
            res = (value + 1.0_c_double)
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
        
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, x__len_, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        return x;
      }

# erroring closure chains return before later RNG effects

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = integer(1)))
          fail <- function(flag) {
            if (flag != 0L) {
              stop("bad")
            }
            NULL
          }
          middle <- function(flag) {
            fail(flag)
            NULL
          }
          outer <- function(flag) {
            middle(flag)
            1
          }
          value <- outer(flag)
          draw <- runif(1)
          value + draw
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: flag
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double) :: value
        real(c_double) :: draw
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
      
      
      
        call outer(flag, value)
        if (quickr_err_msg(1) /= c_null_char) return
        draw = unif_rand()
        out_ = (value + draw)
      
        contains
          subroutine fail(flag)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: flag
      
            if ((flag /= 0_c_int)) then
              call quickr_set_error_msg("bad")
              return
            end if
          end subroutine
          subroutine middle(flag)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: flag
      
            call fail(flag)
            if (quickr_err_msg(1) /= c_null_char) return
          end subroutine
          subroutine outer(flag, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: flag
            real(c_double), intent(out) :: res
      
            call middle(flag)
            if (quickr_err_msg(1) /= c_null_char) return
            res = 1.0_c_double
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const flag__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != INTSXP) {
          Rf_error("typeof(flag) must be 'integer', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = INTEGER(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(flag__, out___, quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# sapply propagates errors from root-defined callees

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = integer(1)))
          fail <- function(flag) {
            if (flag != 0L) {
              stop("bad")
            }
            NULL
          }
          step <- function(i) {
            fail(flag)
            i
          }
          values <- sapply(seq_len(2L), step)
          draw <- runif(1)
          sum(values) + draw
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: flag
        real(c_double), intent(out) :: out_
      
        ! locals
        integer(c_int) :: values(2)
        integer(c_int) :: tmp1_
        real(c_double) :: draw
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
      
      
        do tmp1_ = 1_c_int, 2_c_int
          call step(tmp1_, values(tmp1_))
          if (quickr_err_msg(1) /= c_null_char) return
        end do
      
      
        draw = unif_rand()
        out_ = (sum(values) + draw)
      
        contains
          subroutine fail(flag)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: flag
      
            if ((flag /= 0_c_int)) then
              call quickr_set_error_msg("bad")
              return
            end if
          end subroutine
          subroutine step(i, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            integer(c_int), intent(out) :: res
      
            call fail(flag)
            if (quickr_err_msg(1) /= c_null_char) return
            res = i
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const flag__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != INTSXP) {
          Rf_error("typeof(flag) must be 'integer', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = INTEGER(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(flag__, out___, quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# sapply reads unchanged named vector values directly

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- sapply(x, function(value) value * 2)
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
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        do tmp1_ = 1_c_int, size(x)
          call closure1_(x(tmp1_), out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (value * 2.0_c_double)
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# sapply materializes a stepped sequence before iteration

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          out <- sapply(seq(1L, n, by = 2L), function(value) as.double(value) + 0.5)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out((abs((int(n) - int(1)) / merge(int(2), 1, int(2) /= 0)) + 1))
      
        ! locals
        integer(c_int) :: tmp1_
        integer(c_int), allocatable :: tmp2_(:)
        integer(c_int) :: tmp3_
        ! manifest end
      
        allocate(tmp2_((abs((int(n) - int(1)) / merge(int(2), 1, int(2) /= 0)) + 1)))
      
      
        if ((1_c_int /= n) .and. (2_c_int == 0_c_int)) then
          call quickr_set_error_msg("invalid '(to - from)/by'")
          return
        end if
        if (((n > 1_c_int) .and. (2_c_int < 0_c_int)) .or. ((n < 1_c_int) .and. (2_c_int > 0_c_int))) then
          call quickr_set_error_msg("wrong sign in 'by' argument")
          return
        end if
        tmp2_ = [ (tmp1_, tmp1_ = 1_c_int, n, merge(int(2_c_int, kind=c_int), 1_c_int, 1_c_int /= n)) ]
        do tmp3_ = 1_c_int, size(tmp2_)
          call closure1_(tmp2_(tmp3_), out(tmp3_))
      
        end do
      
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (real(value, kind=c_double) + 0.5_c_double)
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
        double* const out__,
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
        const int _as_int_n = Rf_asInteger(n);
        if ((1 != _as_int_n) && (2 == 0))
          Rf_error("invalid '(to - from)/by'");
        if (((1 < _as_int_n) && (2 < 0)) ||
            ((1 > _as_int_n) && (2 > 0)))
          Rf_error("wrong sign in 'by' argument");
        const R_xlen_t out__len_ = ((((((R_xlen_t)(_as_int_n) - (R_xlen_t)(1)) / (R_xlen_t)(((2) == 0 ? 1 : (2))))) < 0 ? -((((R_xlen_t)(_as_int_n) - (R_xlen_t)(1)) / (R_xlen_t)(((2) == 0 ? 1 : (2))))) : ((((R_xlen_t)(_as_int_n) - (R_xlen_t)(1)) / (R_xlen_t)(((2) == 0 ? 1 : (2)))))) + 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(n__, out__, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

