# quick() supports svd(x)$d

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 2)))
          svd(x)$d
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
        real(c_double), intent(in) :: x(3, 2)
        real(c_double), intent(out) :: out_(2)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_(2)
          real(c_double) :: btmp2_(3, 2)
          real(c_double) :: btmp3_(2, 2)
          real(c_double) :: btmp4_(3, 2)
          real(c_double) :: btmp5_(2, 2)
          integer(c_int) :: btmp6_
          integer(c_int) :: btmp7_
          real(c_double), allocatable :: btmp8_(:)
          integer(c_int), allocatable :: btmp9_(:)
          real(c_double), allocatable :: btmp10_(:)
      
          allocate(btmp8_((1 + 0)))
          allocate(btmp9_((8 * 2)))
          btmp4_ = x
          btmp7_ = -1_c_int
      call dgesdd('S', int(3, kind=c_int), int(2, kind=c_int), btmp4_, int(3, kind=c_int), btmp1_, btmp2_, int(3, kind=c_int), btmp5_,&
      & int(2, kind=c_int), btmp8_, btmp7_, btmp9_, btmp6_)
          btmp7_ = int(btmp8_(1), kind=c_int)
          allocate(btmp10_(btmp7_))
      call dgesdd('S', int(3, kind=c_int), int(2, kind=c_int), btmp4_, int(3, kind=c_int), btmp1_, btmp2_, int(3, kind=c_int), btmp5_,&
      & int(2, kind=c_int), btmp10_, btmp7_, btmp9_, btmp6_)
          if (btmp6_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd: illegal argument")
            return
          end if
          if (btmp6_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd failed to converge")
            return
          end if
          btmp3_ = transpose(btmp5_)
          out_ = btmp1_
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
        if (x__dim_2_ != 2)
          Rf_error("dim(x)[2] must be 2, not %0.f",
                    (double)x__dim_2_);
        const R_xlen_t out___len_ = 2;
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

