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
      subroutine fn(n, m, a, b, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(in) :: a(int((min(real(n, kind=c_double), real(m, kind=c_double))), kind=c_ptrdiff_t))
        real(c_double), intent(in) :: b(int((min(real(n, kind=c_double), real(m, kind=c_double))), kind=c_ptrdiff_t))
        real(c_double), intent(out) :: out(int((min(real(n, kind=c_double), real(m, kind=c_double))), kind=c_ptrdiff_t))
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
        if (int((min(real(n, kind=c_double), real(m, kind=c_double))), kind=c_ptrdiff_t) < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = 0.0_c_double
        block
          integer(c_int) :: btmp1_
      
          btmp1_ = size(out)
          if (btmp1_ < 0) then
            call quickr_set_error_msg("seq_len() bound must be non-negative")
            return
          end if
          do i = 1, btmp1_
            out(i) = (a(i) + b(i))
          end do
        end block
      
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
        const int* const n__,
        const int* const m__,
        const double* const a__,
        const double* const b__,
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
        if ((double)(((_as_int_n) < (_as_int_m) ? (_as_int_n) : (_as_int_m))) < 0)
          Rf_error("return dimensions must be non-negative");
        const R_xlen_t out__len_ = ((_as_int_n) < (_as_int_m) ? (_as_int_n) : (_as_int_m));
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          m__,
          a__,
          b__,
          out__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

