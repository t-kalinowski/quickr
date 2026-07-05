# integer + double promotes to double

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          x + 0.5
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
        real(c_double), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = (x + 0.5_c_double)
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
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# real-valued RHS with integer operand declares a double binding

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          out <- x * 3L
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
        real(c_double), intent(out) :: out
        ! manifest end
      
      
        out = (x * 3_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out__);
      
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
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# misreported mode does not defeat subscript coercion

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- x[as.integer(runif(1) * 3) + 1L]
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
        real(c_double), intent(out) :: out
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out = x((int((unif_rand() * 3_c_int), kind=c_int) + 1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
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
        
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        GetRNGstate();
        fn(x__, out__, x__len_);
        PutRNGstate();
        
        UNPROTECT(1);
        return out;
      }

# c() promotes mixed elements

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          c(1L, 2.5, x)
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
        real(c_double), intent(out) :: out_((2 + x__len_))
        ! manifest end
      
      
        out_ = [ real(1_c_int, kind=c_double), 2.5_c_double, real(x, kind=c_double) ]
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
        
        const R_xlen_t out___len_ = (2 + x__len_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# multi-arg max()/min() promote

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          max(x, 2.5)
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
      
      
        out_ = max(real(maxval(x), kind=c_double), 2.5_c_double)
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

# %% and %/% with mixed modes promote

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = integer(n)), type(b = double(n)))
          a %% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        integer(c_int), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = modulo(real(a, kind=c_double), b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_xlen_t a__len_);
      
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

# ^ always returns double; integer exponent stays exact

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          x ^ -1L
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
        integer(c_int), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = (real(x, kind=c_double) ** (-1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const x__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
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

# logical operands participate in arithmetic as integers

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = logical(1)), type(b = logical(1)))
          a + b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: a ! logical
        integer(c_int), intent(in) :: b ! logical
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
        out_ = (merge(1_c_int, 0_c_int, (a/=0)) + merge(1_c_int, 0_c_int, (b/=0)))
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
        if (TYPEOF(a) != LGLSXP) {
          Rf_error("typeof(a) must be 'logical', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = LOGICAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != LGLSXP) {
          Rf_error("typeof(b) must be 'logical', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const int* const b__ = LOGICAL(b);
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

# abs() and single-arg reductions accept logical operands like R

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(n)))
          abs(x)
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
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = abs(merge(1_c_int, 0_c_int, (x/=0)))
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
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

