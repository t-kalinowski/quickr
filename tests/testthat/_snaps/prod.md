# prod promotes integer arrays before multiplying

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          prod(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = product(real(x, kind=c_double))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# prod returns double for logical arrays

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(n)))
          prod(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = product(merge(1.0_c_double, 0.0_c_double, (x/=0)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# prod promotes integer and logical scalars and literals

    Code
      fn
    Output
      function(x, flag) {
          declare(type(x = integer(1)), type(flag = logical(1)))
          c(prod(x), prod(flag), prod(x, flag), prod(2L), prod(TRUE))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, flag, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: flag ! logical
        real(c_double), intent(out) :: out_(5)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = real(x, kind=c_double)
          btmp2_ = merge(1.0_c_double, 0.0_c_double, (flag/=0))
      out_ = [ real(x, kind=c_double), merge(1.0_c_double, 0.0_c_double, (flag/=0)), (btmp1_ * btmp2_), real(2_c_int, kind=c_double),&
      & merge(1.0_c_double, 0.0_c_double, .true.) ]
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const flag__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = 5;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, flag__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# prod promotes each argument before a multi-argument product

    Code
      fn
    Output
      function(x, y, flag) {
          declare(
            type(x = integer(n)),
            type(y = integer(m)),
            type(flag = logical(1))
          )
          prod(first = x, na.rm = FALSE, second = y, flag)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, flag, out_, x__len_, y__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: y(y__len_)
        integer(c_int), intent(in) :: flag ! logical
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
          real(c_double) :: btmp3_
      
          btmp1_ = product(real(x, kind=c_double))
          btmp2_ = product(real(y, kind=c_double))
          btmp3_ = merge(1.0_c_double, 0.0_c_double, (flag/=0))
          out_ = (btmp1_ * btmp2_ * btmp3_)
        end block
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
        const int* const flag__,
        double* const out___,
        const R_xlen_t x__len_,
        const R_xlen_t y__len_);
      
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
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          y__,
          flag__,
          out___,
          x__len_,
          y__len_);
        
        UNPROTECT(1);
        return out_;
      }

# masked integer products promote before reducing

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          prod(x[x > 0L])
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = product(real(x, kind=c_double), mask = (x > 0_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# masked logical products retain the empty-product identity

    Code
      fn
    Output
      function(x, mask) {
          declare(type(x = logical(n)), type(mask = logical(n)))
          prod(x[mask])
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(in) :: mask(x__len_) ! logical
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = product(merge(1.0_c_double, 0.0_c_double, (x/=0)), mask = (mask/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const mask__,
        double* const out___,
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
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          mask__,
          out___,
          x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# prod arguments give local closures the promoted result mode

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          identity <- function(value) value
          identity(prod(x))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
        call identity(product(real(x, kind=c_double)), out_)
      
        contains
          subroutine identity(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
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
        const int* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# prod retains complex result mode

    Code
      fn
    Output
      function(x) {
          declare(type(x = complex(n)))
          prod(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        complex(c_double_complex), intent(in) :: x(x__len_)
        complex(c_double_complex), intent(out) :: out_
        ! manifest end
      
      
        out_ = product(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const Rcomplex* const x__,
        Rcomplex* const out___,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != CPLXSXP) {
          Rf_error("typeof(x) must be 'complex', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const Rcomplex* const x__ = COMPLEX(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(CPLXSXP, out___len_));
        Rcomplex* out___ = COMPLEX(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

