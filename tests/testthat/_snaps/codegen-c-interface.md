# C snapshots show strict input checks and modified argument copies

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Strict type checks; only the modified argument is duplicated.
    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(n)))
          x <- x + 1
          list(changed = x, unchanged = y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(in out) :: y(x__len_)
        ! manifest end
      
      
        x = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const x__,
        const double* const y__,
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != y__len_)
          Rf_error("length(y) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__len_);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, x);
        SET_VECTOR_ELT(_ans, 1, y);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("changed"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("unchanged"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(2);
        return _ans;
      }

# C snapshots show coercion and copy branches in a non-strict bridge

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(n)))
          x <- x + 1
          list(changed = x, unchanged = y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(in out) :: y(x__len_)
        ! manifest end
      
      
        x = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const x__,
        const double* const y__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          x = Rf_coerceVector(x, REALSXP);
          SETCAR(_args, x);
        } else x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          y = Rf_coerceVector(y, REALSXP);
          SETCAR(_args, y);
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != y__len_)
          Rf_error("length(y) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__len_);
        
        fn(x__, y__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, x);
        SET_VECTOR_ELT(_ans, 1, y);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("changed"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("unchanged"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(2);
        return _ans;
      }

# C snapshots show mixed output storage and array dimensions

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Real, logical, complex, and matrix outputs have separate C allocations.
    Code
      fn
    Output
      function(x, z) {
          declare(type(x = double(n)), type(z = complex(n)))
          values <- x + Re(z)
          keep <- x > 0
          shifted <- z + 1i
          matrix <- matrix(values, nrow = length(x), ncol = 1L)
          list(values = values, keep = keep, shifted = shifted, matrix = matrix)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, z, values, keep, shifted, matrix, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_double_complex, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        complex(c_double_complex), intent(in) :: z(x__len_)
        real(c_double), intent(out) :: values(x__len_)
        integer(c_int), intent(out) :: keep(x__len_) ! logical
        complex(c_double_complex), intent(out) :: shifted(x__len_)
        real(c_double), intent(out) :: matrix(x__len_, 1)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(real(z, kind=c_double), kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        values = (x + real(z, kind=c_double))
        keep = (x > 0.0_c_double)
        shifted = (z + (0.0_c_double, 1.0_c_double))
        matrix = reshape(values, [int(x__len_), int(1)], pad = values)
      
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
        const double* const x__,
        const Rcomplex* const z__,
        double* const values__,
        int* const keep__,
        Rcomplex* const shifted__,
        double* const matrix__,
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
        
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        if (x__len_ != z__len_)
          Rf_error("length(z) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)z__len_, (double)x__len_);
        const R_xlen_t values__len_ = x__len_;
        SEXP values = PROTECT(Rf_allocVector(REALSXP, values__len_));
        double* values__ = REAL(values);
        const R_xlen_t keep__len_ = x__len_;
        SEXP keep = PROTECT(Rf_allocVector(LGLSXP, keep__len_));
        int* keep__ = LOGICAL(keep);
        const R_xlen_t shifted__len_ = x__len_;
        SEXP shifted = PROTECT(Rf_allocVector(CPLXSXP, shifted__len_));
        Rcomplex* shifted__ = COMPLEX(shifted);
        const R_xlen_t matrix__len_ = (x__len_) * (1);
        SEXP matrix = PROTECT(Rf_allocVector(REALSXP, matrix__len_));
        double* matrix__ = REAL(matrix);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = 1;
          Rf_dimgets(matrix, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          z__,
          values__,
          keep__,
          shifted__,
          matrix__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 4));
        SET_VECTOR_ELT(_ans, 0, values);
        SET_VECTOR_ELT(_ans, 1, keep);
        SET_VECTOR_ELT(_ans, 2, shifted);
        SET_VECTOR_ELT(_ans, 3, matrix);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 4));
        SET_STRING_ELT(_names, 0, Rf_mkChar("values"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("keep"));
        SET_STRING_ELT(_names, 2, Rf_mkChar("shifted"));
        SET_STRING_ELT(_names, 3, Rf_mkChar("matrix"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(7);
        return _ans;
      }

# C snapshots show guarded dynamic sequence lengths

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: The C bridge validates the step before allocating the sequence output.
    Code
      fn
    Output
      function(step) {
          declare(type(step = integer(1)))
          out <- seq(1L, 9L, by = step)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(step, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: step
        integer(c_int), intent(out) :: out((abs((int(9) - int(1)) / merge(int(step), 1, int(step) /= 0)) + 1))
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if ((1_c_int /= 9_c_int) .and. (step == 0_c_int)) then
          call quickr_set_error_msg("invalid '(to - from)/by'")
          return
        end if
        if (((9_c_int > 1_c_int) .and. (step < 0_c_int)) .or. ((9_c_int < 1_c_int) .and. (step > 0_c_int))) then
          call quickr_set_error_msg("wrong sign in 'by' argument")
          return
        end if
        out = [ (tmp1_, tmp1_ = 1_c_int, 9_c_int, merge(int(step, kind=c_int), 1_c_int, 1_c_int /= 9_c_int)) ]
      
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
        const int* const step__,
        int* const out__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // step
        _args = CDR(_args);
        SEXP step = CAR(_args);
        if (TYPEOF(step) != INTSXP) {
          Rf_error("typeof(step) must be 'integer', not '%s'", Rf_type2char(TYPEOF(step)));
        }
        const int* const step__ = INTEGER(step);
        const R_xlen_t step__len_ = Rf_xlength(step);
        
        if (step__len_ != 1)
          Rf_error("length(step) must be 1, not %0.f",
                    (double)step__len_);
        const int _as_int_step = Rf_asInteger(step);
        if ((1 != 9) && (_as_int_step == 0))
          Rf_error("invalid '(to - from)/by'");
        if (((1 < 9) && (_as_int_step < 0)) ||
            ((1 > 9) && (_as_int_step > 0)))
          Rf_error("wrong sign in 'by' argument");
        const R_xlen_t out__len_ = ((((((R_xlen_t)(9) - (R_xlen_t)(1)) / (R_xlen_t)(((_as_int_step) == 0 ? 1 : (_as_int_step))))) < 0 ? -((((R_xlen_t)(9) - (R_xlen_t)(1)) / (R_xlen_t)(((_as_int_step) == 0 ? 1 : (_as_int_step))))) : ((((R_xlen_t)(9) - (R_xlen_t)(1)) / (R_xlen_t)(((_as_int_step) == 0 ? 1 : (_as_int_step)))))) + 1);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(step__, out__, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# C snapshots preserve floating operands in size expressions

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Rf_asReal and floating abs precede the final length conversion.
    Code
      fn
    Output
      function(n) {
          declare(type(n = double(1)), type(out = double(as.integer(abs(n) + 1))))
          out <- double(as.integer(abs(n) + 1))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: n
        real(c_double), intent(out) :: out(int((abs(n) + 1)))
        ! manifest end
      
      
        out = 0.0_c_double
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const n__, double* const out__);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != REALSXP) {
          Rf_error("typeof(n) must be 'double', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const double* const n__ = REAL(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const double _as_double_n = Rf_asReal(n);
        const R_xlen_t out__len_ = ((R_xlen_t)((((_as_double_n) < 0 ? -(_as_double_n) : (_as_double_n)) + 1)));
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(n__, out__);
        
        UNPROTECT(1);
        return out;
      }

