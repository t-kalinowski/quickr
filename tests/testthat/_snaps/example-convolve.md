# convolve

    Code
      slow_convolve
    Output
      function(a, b) {
          declare(type(a = double(NA)))
          declare(type(b = double(NA)))
      
          ab <- double(length(a) + length(b) - 1)
          for (i in seq_along(a)) {
            for (j in seq_along(b)) {
              ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
            }
          }
          ab
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_convolve(a, b, ab, a__len_, b__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: ab(((a__len_ + b__len_) - 1))
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        ! manifest end
      
      
      
        if (((a__len_ + b__len_) - 1) < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        ab = 0.0_c_double
        do i = 1, size(a)
          do j = 1, size(b)
            ab(((i + j) - 1_c_int)) = (ab(((i + j) - 1_c_int)) + (a(i) * b(j)))
          end do
        end do
      
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
      
      
      extern void slow_convolve(
        const double* const a__, 
        const double* const b__, 
        double* const ab__, 
        const R_xlen_t a__len_, 
        const R_xlen_t b__len_, 
        char* quickr_err_msg);
      
      SEXP slow_convolve_(SEXP _args) {
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
        
        const R_xlen_t ab__len_ = ((a__len_ + b__len_) - 1);
        SEXP ab = PROTECT(Rf_allocVector(REALSXP, ab__len_));
        double* ab__ = REAL(ab);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        slow_convolve(
          a__,
          b__,
          ab__,
          a__len_,
          b__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return ab;
      }

