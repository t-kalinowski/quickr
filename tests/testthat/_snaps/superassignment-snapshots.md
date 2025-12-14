# translation snapshots: statement closure with host-associated <<-

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Statement local closure lowered as internal subroutine; <<- subset targets are host-associated (no capture arg/decl in the closure).
    Code
      fn
    Output
      function(nx, ny) {
          declare(type(nx = integer(1)), type(ny = integer(1)))
          temp <- matrix(0, nx, ny)
      
          bc <- function() {
            temp[1, ] <<- 1
            temp[nx, ] <<- 2
            temp[, 1] <<- 3
            temp[, ny] <<- 4
            NULL
          }
      
          bc()
          temp
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(nx, ny, temp) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: nx
        integer(c_int), intent(in) :: ny
        real(c_double), intent(out) :: temp(nx, ny)
        ! manifest end
      
      
        temp = 0.0_c_double
      
        call bc()
      
        contains
          subroutine bc()
            use iso_c_binding, only: c_double, c_int
            implicit none
      
      
      
            temp(1_c_int, :) = 1.0_c_double
            temp(nx, :) = 2.0_c_double
            temp(:, 1_c_int) = 3.0_c_double
            temp(:, ny) = 4.0_c_double
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const nx__, 
        const int* const ny__, 
        double* const temp__);
      
      SEXP fn_(SEXP _args) {
        // nx
        _args = CDR(_args);
        SEXP nx = CAR(_args);
        if (TYPEOF(nx) != INTSXP) {
          Rf_error("typeof(nx) must be 'integer', not '%s'", Rf_type2char(TYPEOF(nx)));
        }
        const int* const nx__ = INTEGER(nx);
        const R_xlen_t nx__len_ = Rf_xlength(nx);
        
        // ny
        _args = CDR(_args);
        SEXP ny = CAR(_args);
        if (TYPEOF(ny) != INTSXP) {
          Rf_error("typeof(ny) must be 'integer', not '%s'", Rf_type2char(TYPEOF(ny)));
        }
        const int* const ny__ = INTEGER(ny);
        const R_xlen_t ny__len_ = Rf_xlength(ny);
        
        if (nx__len_ != 1)
          Rf_error("length(nx) must be 1, not %0.f",
                    (double)nx__len_);
        if (ny__len_ != 1)
          Rf_error("length(ny) must be 1, not %0.f",
                    (double)ny__len_);
        const R_xlen_t temp__len_ = (Rf_asInteger(nx)) * (Rf_asInteger(ny));
        SEXP temp = PROTECT(Rf_allocVector(REALSXP, temp__len_));
        double* temp__ = REAL(temp);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = Rf_asInteger(nx);
          _dim[1] = Rf_asInteger(ny);
          Rf_dimgets(temp, _dim_sexp);
        }
        
        fn(nx__, ny__, temp__);
        
        UNPROTECT(2);
        return temp;
      }

# translation snapshots: sapply + <<- mutates host + returns value

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Local closure lowered under contains; x is host-associated (no capture arg), marked modified so the C bridge duplicates x to preserve R semantics.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)), type(out = double(length(x))))
          f <- function(i) {
            x[i] <<- x[i] * 2
            x[i]
          }
          out <- sapply(seq_along(x), f)
          list(x = x, out = out)
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
        real(c_double), intent(in out) :: x(x__len_)
        real(c_double), intent(out) :: out(size(x))
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
      
        do tmp1_ = 1_c_int, size(x)
          call f(tmp1_, out(tmp1_))
        end do
      
        contains
          subroutine f(i, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            real(c_double), intent(out) :: res
      
            x(i) = (x(i) * 2.0_c_double)
            res = x(i)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const x__, 
        double* const out__, 
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, x);
        SET_VECTOR_ELT(_ans, 1, out);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("x"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("out"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(3);
        return _ans;
      }

