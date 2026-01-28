# runif generates random numbers

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          runif(n)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(n)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = [(unif_rand(), tmp1_=1, n)]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
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
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(n__, out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          x * runif(1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(x__len_)
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = (x * unif_rand())
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
        double* const out___, 
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
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(x__, out___, x__len_);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          x * runif(length(x))
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
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = (x * [(unif_rand(), tmp1_=1, x__len_)])
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
        double* const out___, 
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
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(x__, out___, x__len_);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

# runif with min/max

    Code
      fn
    Output
      function(n, a, b) {
          declare(
            type(n = integer(1)),
            type(a = double(1)),
            type(b = double(1))
          )
          runif(n, a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, a, b, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_(n)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = [((a + (unif_rand() * (b - a))), tmp1_=1, n)]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const n__, 
        const double* const a__, 
        const double* const b__, 
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
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
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(
          n__,
          a__,
          b__,
          out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(n, b) {
          declare(
            type(n = integer(1)),
            type(b = double(1))
          )
          runif(n, max = b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, b, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_(n)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = [(unif_rand() * b, tmp1_=1, n)]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const n__, 
        const double* const b__, 
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(n__, b__, out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(b) {
          declare(
            type(b = double(1))
          )
          runif(1, max = b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(b, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = unif_rand() * b
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(const double* const b__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(b__, out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(b) {
          declare(
            type(b = double(1))
          )
          runif(10, max = b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(b, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_(10)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = [(unif_rand() * b, tmp1_=1, 10)]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(const double* const b__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = 10;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(b__, out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

