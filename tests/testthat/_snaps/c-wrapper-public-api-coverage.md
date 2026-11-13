# C bridge hoisting can be disabled (public API)

    Code
      fn
    Output
      function(x, y, n) {
          declare({
            type(n = integer(1))
            type(x = double(n))
            type(y = double(n + 1))
            type(out = double(n))
          })
          out <- x + 1
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, n, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(n)
        real(c_double), intent(in) :: y((n + 1))
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out(n)
        ! manifest end
      
      
        if ((size(out, 1, kind=c_ptrdiff_t)) /= (size((x + 1.0_c_double), 1, kind=c_ptrdiff_t))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `out`")
          return
        end if
        out = (x + 1.0_c_double)
      
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
        const double* const y__,
        const int* const n__,
        double* const out__,
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != x__len_)
          Rf_error("length(x) must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)_as_int_n);
        {
          const R_xlen_t expected = (_as_int_n + 1);
          if (y__len_ != expected)
            Rf_error("length(y) must equal (n + 1),"
                     " but are %0.f and %0.f",
                      (double)y__len_, (double)expected);
        }
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out__len_ = _as_int_n;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          n__,
          out__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

