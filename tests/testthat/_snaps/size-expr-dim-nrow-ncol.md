# declare() size expressions support nrow() and ncol()

    Code
      fn
    Output
      function(x) {
          declare(
            type(x = double(NA, NA)),
            type(out = double(nrow(x) + ncol(x)))
          )
          out <- double(nrow(x) + ncol(x))
          out[1] <- 1
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out((size(x, 1) + size(x, 2)))
        ! manifest end
      
      
        if ((x__dim_1_ + x__dim_2_) < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((size(out, 1, kind=c_ptrdiff_t)) /= ((x__dim_1_ + x__dim_2_))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `out`")
          return
        end if
        out = 0.0_c_double
        out(1_c_int) = 1.0_c_double
      
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
        double* const out__,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        const R_xlen_t out__len_ = (x__dim_1_ + x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# declare() size expressions support dim(x)[axis] indices

    Code
      fn
    Output
      function(x) {
          declare(
            type(x = double(NA, NA)),
            type(out = double(dim(x)[1L] + dim(x)[2L]))
          )
          out <- double(dim(x)[1L] + dim(x)[2L])
          for (i in seq_len(length(out))) {
            out[i] <- as.double(i)
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out((size(x, 1) + size(x, 2)))
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
        if ((x__dim_1_ + x__dim_2_) < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((size(out, 1, kind=c_ptrdiff_t)) /= ((x__dim_1_ + x__dim_2_))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `out`")
          return
        end if
        out = 0.0_c_double
        do i = 1, size(out)
          out(i) = real(i, kind=c_double)
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
      
      
      extern void fn(
        const double* const x__,
        double* const out__,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        const R_xlen_t out__len_ = (x__dim_1_ + x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

