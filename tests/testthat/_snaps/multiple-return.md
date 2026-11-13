# multiple return values

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1
          z <- x + 2
          list(y = y, z = z)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: y(x__len_)
        real(c_double), intent(out) :: z(x__len_)
        ! manifest end
      
      
        y = (x + 1.0_c_double)
        z = (x + 2.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const y__,
        double* const z__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(REALSXP, y__len_));
        double* y__ = REAL(y);
        const R_xlen_t z__len_ = x__len_;
        SEXP z = PROTECT(Rf_allocVector(REALSXP, z__len_));
        double* z__ = REAL(z);
        
        fn(
          x__,
          y__,
          z__,
          x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, z);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("y"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("z"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# multiple return values via assignment

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1
          z <- x + 2
          out <- list(y = y, z = z)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: y(x__len_)
        real(c_double), intent(out) :: z(x__len_)
        ! manifest end
      
      
        y = (x + 1.0_c_double)
        z = (x + 2.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const y__,
        double* const z__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(REALSXP, y__len_));
        double* y__ = REAL(y);
        const R_xlen_t z__len_ = x__len_;
        SEXP z = PROTECT(Rf_allocVector(REALSXP, z__len_));
        double* z__ = REAL(z);
        
        fn(
          x__,
          y__,
          z__,
          x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, z);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("y"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("z"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# single return variable still works

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1
          y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        double* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(REALSXP, y__len_));
        double* y__ = REAL(y);
        
        fn(x__, y__, x__len_);
        
        UNPROTECT(1);
        return y;
      }

# custom names for multiple return values are preserved

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          z <- x + 2L
          list(abc = y, def = z)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        integer(c_int), intent(out) :: z(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
        z = (x + 2_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
        int* const z__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        const R_xlen_t z__len_ = x__len_;
        SEXP z = PROTECT(Rf_allocVector(INTSXP, z__len_));
        int* z__ = INTEGER(z);
        
        fn(
          x__,
          y__,
          z__,
          x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, z);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("abc"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("def"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# no names add to unnamed elements

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          z <- x + 2L
          list(y, z)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        integer(c_int), intent(out) :: z(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
        z = (x + 2_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
        int* const z__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        const R_xlen_t z__len_ = x__len_;
        SEXP z = PROTECT(Rf_allocVector(INTSXP, z__len_));
        int* z__ = INTEGER(z);
        
        fn(
          x__,
          y__,
          z__,
          x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, z);
        UNPROTECT(3);
        return _ans;
      }

# single-element unnamed list returns a list

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          list(y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(_ans, 0, y);
        UNPROTECT(2);
        return _ans;
      }

# single-element named list preserves name

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          list(abc = y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(_ans, 0, y);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 1));
        SET_STRING_ELT(_names, 0, Rf_mkChar("abc"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(3);
        return _ans;
      }

# single-element list via assignment returns a list (VECSXP)

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          out <- list(y)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(_ans, 0, y);
        UNPROTECT(2);
        return _ans;
      }

# mixed named and unnamed list work

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          z <- x + 2L
          list(y, abc = z, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, z, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in out) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        integer(c_int), intent(out) :: z(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
        z = (x + 2_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
        int* const z__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        const R_xlen_t z__len_ = x__len_;
        SEXP z = PROTECT(Rf_allocVector(INTSXP, z__len_));
        int* z__ = INTEGER(z);
        
        fn(
          x__,
          y__,
          z__,
          x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 3));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, z);
        SET_VECTOR_ELT(_ans, 2, x);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(_names, 0, Rf_mkChar(""));
        SET_STRING_ELT(_names, 1, Rf_mkChar("abc"));
        SET_STRING_ELT(_names, 2, Rf_mkChar(""));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# duplicate return variables compile once and work

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          list(y, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, y);
        UNPROTECT(2);
        return _ans;
      }

# duplicate return variables with names work

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(n)))
          y <- x + 1L
          list(a = y, b = y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: y(x__len_)
        ! manifest end
      
      
        y = (x + 1_c_int)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const y__,
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
        
        const R_xlen_t y__len_ = x__len_;
        SEXP y = PROTECT(Rf_allocVector(INTSXP, y__len_));
        int* y__ = INTEGER(y);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, y);
        SET_VECTOR_ELT(_ans, 1, y);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("a"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("b"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(3);
        return _ans;
      }

