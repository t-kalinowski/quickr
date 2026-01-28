# size check blocks redeclare hoisted size temps

    Code
      fn
    Output
      function(n, m, a, b) {
          declare(
            type(n = integer(1)),
            type(m = integer(1)),
            type(a = double(min(n, m))),
            type(b = double(min(n, m))),
            type(out = double(min(n, m)))
          )
          out <- double(min(n, m))
          for (i in seq_len(length(out))) {
            out[i] <- a[i] + b[i]
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, m, a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(in) :: a(min(n, m))
        real(c_double), intent(in) :: b(min(n, m))
        real(c_double), intent(out) :: out(min(n, m))
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
        out = 0
        do i = 1, size(out)
          out(i) = (a(i) + b(i))
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const n__, 
        const int* const m__, 
        const double* const a__, 
        const double* const b__, 
        double* const out__);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
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
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const int _as_int_n = Rf_asInteger(n);
        const int _as_int_m = Rf_asInteger(m);
        {
          const R_xlen_t expected = ((_as_int_n) < (_as_int_m) ? (_as_int_n) : (_as_int_m));
          if (a__len_ != expected)
            Rf_error("length(a) must equal (min(n, m)),"
                     " but are %0.f and %0.f",
                      (double)a__len_, (double)expected);
        }
        {
          const R_xlen_t expected = ((_as_int_n) < (_as_int_m) ? (_as_int_n) : (_as_int_m));
          if (b__len_ != expected)
            Rf_error("length(b) must equal (min(n, m)),"
                     " but are %0.f and %0.f",
                      (double)b__len_, (double)expected);
        }
        const R_xlen_t out__len_ = ((_as_int_n) < (_as_int_m) ? (_as_int_n) : (_as_int_m));
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          n__,
          m__,
          a__,
          b__,
          out__);
        
        UNPROTECT(1);
        return out;
      }

