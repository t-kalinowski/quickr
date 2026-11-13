# package headers support later RNG and power-shaped entrypoints

    Code
      writeLines(pkg$source)
    Output
      declare <- function(...) invisible()
      
      shift <- quickr::quick("shift", function(x) {
        declare(type(x = double(1)))
        x + 1
      })
      
      draw_square <- quickr::quick("draw_square", function(n) {
        declare(type(n = integer(1)))
        runif(n^2L)
      })
    Code
      writeLines(pkg$fortran)
    Output
      subroutine shift(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = (x + 1.0_c_double)
      end subroutine
      
      subroutine draw_square(n, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_((n)**(2))
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        out_ = [(unif_rand(), tmp1_=1, (n)**(2))]
      end subroutine
    Code
      writeLines(pkg$entrypoints)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <Rmath.h>
      #include <R_ext/Random.h>
      
      
      extern void shift(const double* const x__, double* const out___);
      
      SEXP shift_(SEXP _args) {
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
      
        shift(x__, out___);
      
        UNPROTECT(1);
        return out_;
      }
      
      extern void draw_square(const int* const n__, double* const out___);
      
      SEXP draw_square_(SEXP _args) {
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
        const R_xlen_t out___len_ = R_pow((double)(_as_int_n), (double)(2));
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
      
        GetRNGstate();
        draw_square(n__, out___);
        PutRNGstate();
      
        UNPROTECT(1);
        return out_;
      }
      
      static const R_ExternalMethodDef QuickrEntries[] = {
        {"shift_", (DL_FUNC) &shift_, -1},
        {"draw_square_", (DL_FUNC) &draw_square_, -1},
        {NULL, NULL, 0}
      };
      
      #include <R_ext/Rdynload.h>
      
      void R_init_quickr_snapshot_headers(DllInfo *dll) {
        R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
        R_useDynamicSymbols(dll, FALSE);
      }

# package entrypoints retain separate procedures with the same local name

    Code
      writeLines(pkg$source)
    Output
      declare <- function(...) invisible()
      
      first <- quickr::quick("first", function(x) {
        declare(type(x = double(1)))
        f <- function(value) value + 1
        f(x)
      })
      
      second <- quickr::quick("second", function(x) {
        declare(type(x = double(1)))
        f <- function(value) value * 2
        f(x)
      })
    Code
      writeLines(pkg$fortran)
    Output
      subroutine first(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
        call f(x, out_)
      
        contains
          subroutine f(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (value + 1.0_c_double)
          end subroutine
      end subroutine
      
      subroutine second(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
      
        call f(x, out_)
      
        contains
          subroutine f(value, res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(in) :: value
            real(c_double), intent(out) :: res
      
      
            res = (value * 2.0_c_double)
          end subroutine
      end subroutine
    Code
      writeLines(pkg$entrypoints)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <Rmath.h>
      #include <R_ext/Random.h>
      
      
      extern void first(const double* const x__, double* const out___);
      
      SEXP first_(SEXP _args) {
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
      
        first(x__, out___);
      
        UNPROTECT(1);
        return out_;
      }
      
      extern void second(const double* const x__, double* const out___);
      
      SEXP second_(SEXP _args) {
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
      
        second(x__, out___);
      
        UNPROTECT(1);
        return out_;
      }
      
      static const R_ExternalMethodDef QuickrEntries[] = {
        {"first_", (DL_FUNC) &first_, -1},
        {"second_", (DL_FUNC) &second_, -1},
        {NULL, NULL, 0}
      };
      
      #include <R_ext/Rdynload.h>
      
      void R_init_quickrsnapshotclosures(DllInfo *dll) {
        R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
        R_useDynamicSymbols(dll, FALSE);
      }

# packages with their own init receive a registration helper

    Code
      writeLines(pkg$source)
    Output
      declare <- function(...) invisible()
      
      shift <- quickr::quick("shift", function(x) {
        declare(type(x = double(1)))
        x + 1
      })
    Code
      writeLines(pkg$fortran)
    Output
      subroutine shift(x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = (x + 1.0_c_double)
      end subroutine
    Code
      writeLines(pkg$entrypoints)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <Rmath.h>
      #include <R_ext/Random.h>
      
      
      extern void shift(const double* const x__, double* const out___);
      
      SEXP shift_(SEXP _args) {
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
      
        shift(x__, out___);
      
        UNPROTECT(1);
        return out_;
      }
      
      static const R_ExternalMethodDef QuickrEntries[] = {
        {"shift_", (DL_FUNC) &shift_, -1},
        {NULL, NULL, 0}
      };
      
      #include <R_ext/Rdynload.h>
      
      void R_init_quickrsnapshotinit_quick_functions(DllInfo *dll) {
        R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
      }

