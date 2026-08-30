# array-expression subscripting hoists into a block-scoped temp

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Fortran disallows (expr)(i,j); quickr uses a block-local temp array.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 4)))
          out <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(3, 4)
        real(c_double), intent(out) :: out
      
        ! locals
        logical :: tmp1_ ! logical
        real(c_double) :: tmp2_
        ! manifest end
      
      
        block
          logical :: btmp1_(3, 4) ! logical
      
          btmp1_ = ((x > 0.0_c_double))
          tmp1_ = btmp1_(2_c_int, 3_c_int)
          if (tmp1_) then
            tmp2_ = 1.0_c_double
          else
            tmp2_ = 0.0_c_double
          end if
          out = tmp2_
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out__);
      
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
        
        if (x__dim_1_ != 3)
          Rf_error("dim(x)[1] must be 3, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 4)
          Rf_error("dim(x)[2] must be 4, not %0.f",
                    (double)x__dim_2_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# block-scoped temps allocate on the heap for runtime shapes

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Block temps with runtime sizes are allocatable so flang doesn't stack-allocate large work arrays.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n, m)))
          out <- ifelse((x > 0.0)[1, 1], 1.0, 0.0)
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
        real(c_double), intent(out) :: out
      
        ! locals
        logical :: tmp1_ ! logical
        real(c_double) :: tmp2_
        ! manifest end
      
      
        block
          logical, allocatable :: btmp1_(:, :) ! logical
      
          allocate(btmp1_(x__dim_1_, x__dim_2_))
          if (size(x, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
            call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
            return
          end if
          if (size(x, 2, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
            call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
            return
          end if
          btmp1_ = ((x > 0.0_c_double))
          tmp1_ = btmp1_(1_c_int, 1_c_int)
          if (tmp1_) then
            tmp2_ = 1.0_c_double
          else
            tmp2_ = 0.0_c_double
          end if
          out = tmp2_
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
        
        const R_xlen_t out__len_ = (1);
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

