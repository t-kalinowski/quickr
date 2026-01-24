# openmp error handling snapshots include cancellation

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Single OpenMP parallel loop with stop()
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          declare(parallel())
          for (i in seq_len(1L)) {
            if (x < 0) {
              stop("x must be nonnegative")
            }
          }
          x + 1
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
      
        !$omp parallel do
        do i = 1, 1_c_int
          if ((x < 0.0_c_double)) then
            call quickr_set_error_msg("x must be nonnegative")
            !$omp cancel do
          end if
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
        out_ = (x + 1.0_c_double)
      
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

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Nested OpenMP parallel loops with stop()
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          declare(parallel())
          for (i in seq_len(1L)) {
            declare(parallel())
            for (j in seq_len(1L)) {
              if (x < 0) {
                stop("x must be nonnegative")
              }
            }
          }
          x + 1
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        ! manifest end
      
      
      
        !$omp parallel do
        do i = 1, 1_c_int
      
          !$omp parallel do
          do j = 1, 1_c_int
            if ((x < 0.0_c_double)) then
              call quickr_set_error_msg("x must be nonnegative")
              !$omp cancel do
            end if
          end do
          !$omp end parallel do
          if (quickr_err_msg(1) /= c_null_char) then
            !$omp cancel do
          end if
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
        out_ = (x + 1.0_c_double)
      
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

