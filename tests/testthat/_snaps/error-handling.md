# stop() translation includes error plumbing

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: stop() should emit error message and C bridge checks
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          if (x < 0) {
            stop("x must be nonnegative")
          }
          x + 1
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        if ((x < 0.0_c_double)) then
          call quickr_set_error_msg("x must be nonnegative")
          return
        end if
        out_ = (x + 1.0_c_double)
      
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
        double* const out___, 
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# stop() translation wraps long error messages

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          if (x < 0) {
            stop(.(long_msg))
          }
          x + 1
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        if ((x < 0.0_c_double)) then
      call quickr_set_error_msg("abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij&
      & abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij")
          return
        end if
        out_ = (x + 1.0_c_double)
      
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
        double* const out___, 
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

