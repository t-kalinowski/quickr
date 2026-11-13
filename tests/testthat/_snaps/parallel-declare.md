# serial loop variables stay private inside parallel loops

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA)))
          out <- integer(length(x))
          declare(parallel())
          for (i in seq_along(x)) {
            for (j in 1L:3L) {
              out[i] <- out[i] + j
            }
            out[i] <- out[i] + j
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if (x__len_ < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = 0_c_int
      
        !$omp parallel do private(j)
        do i = 1, size(x)
          do tmp1_ = 1_c_int, 3_c_int, sign(1, 3_c_int-1_c_int)
            j = tmp1_
            out(i) = (out(i) + j)
          end do
          out(i) = (out(i) + j)
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            !$omp critical (quickr_error)
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
            !$omp end critical (quickr_error)
          end subroutine quickr_set_error_msg
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_xlen_t x__len_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

