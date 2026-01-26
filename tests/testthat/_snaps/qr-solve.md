# qr.solve translation uses linpack path

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: qr.solve uses dqrdc2/dqrcf (vector rhs)
    Code
      fn
    Output
      function(a, b) {
          declare(
            type(a = double(n, k)),
            type(b = double(n))
          )
          qr.solve(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, a__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: b(a__dim_1_)
        real(c_double), intent(out) :: out_(a__dim_2_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
          real(c_double), allocatable :: btmp2_(:, :)
          real(c_double), allocatable :: btmp3_(:)
          integer(c_int), allocatable :: btmp4_(:)
          real(c_double), allocatable :: btmp5_(:, :)
          integer(c_int) :: btmp6_
          integer(c_int) :: btmp7_
          real(c_double), allocatable :: btmp8_(:, :)
          integer(c_int) :: btmp9_
          integer(c_int) :: btmp10_
      
          allocate(btmp1_(a__dim_1_, a__dim_2_))
          allocate(btmp2_(a__dim_1_, 1))
          allocate(btmp3_(a__dim_2_))
          allocate(btmp4_(a__dim_2_))
          allocate(btmp5_(a__dim_2_, 2))
          allocate(btmp8_(min(a__dim_1_, a__dim_2_), 1))
          btmp1_ = a
          btmp2_ = 0.0_c_double
          btmp2_(1:a__dim_1_, 1) = b
          do btmp7_ = 1_c_int, int(a__dim_2_, kind=c_int)
            btmp4_(btmp7_) = btmp7_
          end do
      call dqrdc2(btmp1_, int(a__dim_1_, kind=c_int), int(a__dim_1_, kind=c_int), int(a__dim_2_, kind=c_int), 1e-07_c_double, btmp6_,&
      & btmp3_, btmp4_, btmp5_)
          if (btmp6_ < int(min(a__dim_1_, a__dim_2_), kind=c_int)) then
            call quickr_set_error_msg("rank deficient matrix in qr.solve")
            return
          end if
          btmp8_ = 0.0_c_double
          call dqrcf(btmp1_, int(a__dim_1_, kind=c_int), btmp6_, btmp3_, btmp2_, int(1, kind=c_int), btmp8_, btmp9_)
          if (btmp9_ /= 0_c_int) then
            call quickr_set_error_msg("exact singularity in 'qr.coef'")
            return
          end if
          out_ = 0.0_c_double
          do btmp10_ = 1_c_int, btmp6_
            out_(btmp4_(btmp10_)) = btmp8_(btmp10_, 1)
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
        const double* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_len_t a__dim_1_, 
        const R_len_t a__dim_2_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const int* const a__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a must be a 2D-array, but length(dim(a)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a__dim_1_ = a__dim_[0];
        const int a__dim_2_ = a__dim_[1];
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__dim_1_ != b__len_)
          Rf_error("length(b) must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__dim_1_);
        const R_xlen_t out___len_ = a__dim_2_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out___,
          a__dim_1_,
          a__dim_2_,
          quickr_err_msg);
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
      # Snapshot note: qr.solve uses dqrdc2/dqrcf (matrix rhs)
    Code
      fn
    Output
      function(a, b) {
          declare(
            type(a = double(n, k)),
            type(b = double(n, m))
          )
          qr.solve(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, a__dim_2_, b__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
        integer(c_int), intent(in), value :: b__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: b(a__dim_1_, b__dim_2_)
        real(c_double), intent(out) :: out_(a__dim_2_, b__dim_2_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
          real(c_double), allocatable :: btmp2_(:, :)
          real(c_double), allocatable :: btmp3_(:)
          integer(c_int), allocatable :: btmp4_(:)
          real(c_double), allocatable :: btmp5_(:, :)
          integer(c_int) :: btmp6_
          integer(c_int) :: btmp7_
          real(c_double), allocatable :: btmp8_(:, :)
          integer(c_int) :: btmp9_
          integer(c_int) :: btmp10_
          integer(c_int) :: btmp11_
      
          allocate(btmp1_(a__dim_1_, a__dim_2_))
          allocate(btmp2_(a__dim_1_, b__dim_2_))
          allocate(btmp3_(a__dim_2_))
          allocate(btmp4_(a__dim_2_))
          allocate(btmp5_(a__dim_2_, 2))
          allocate(btmp8_(min(a__dim_1_, a__dim_2_), b__dim_2_))
          btmp1_ = a
          btmp2_ = 0.0_c_double
          btmp2_(1:a__dim_1_, 1:b__dim_2_) = b
          do btmp7_ = 1_c_int, int(a__dim_2_, kind=c_int)
            btmp4_(btmp7_) = btmp7_
          end do
      call dqrdc2(btmp1_, int(a__dim_1_, kind=c_int), int(a__dim_1_, kind=c_int), int(a__dim_2_, kind=c_int), 1e-07_c_double, btmp6_,&
      & btmp3_, btmp4_, btmp5_)
          if (btmp6_ < int(min(a__dim_1_, a__dim_2_), kind=c_int)) then
            call quickr_set_error_msg("rank deficient matrix in qr.solve")
            return
          end if
          btmp8_ = 0.0_c_double
          call dqrcf(btmp1_, int(a__dim_1_, kind=c_int), btmp6_, btmp3_, btmp2_, int(b__dim_2_, kind=c_int), btmp8_, btmp9_)
          if (btmp9_ /= 0_c_int) then
            call quickr_set_error_msg("exact singularity in 'qr.coef'")
            return
          end if
          out_ = 0.0_c_double
          do btmp11_ = 1_c_int, int(b__dim_2_, kind=c_int)
            do btmp10_ = 1_c_int, btmp6_
              out_(btmp4_(btmp10_), btmp11_) = btmp8_(btmp10_, btmp11_)
            end do
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
        const double* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_len_t a__dim_1_, 
        const R_len_t a__dim_2_, 
        const R_len_t b__dim_2_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const int* const a__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a must be a 2D-array, but length(dim(a)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a__dim_1_ = a__dim_[0];
        const int a__dim_2_ = a__dim_[1];
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const int* const b__dim_ = ({
        SEXP dim_ = Rf_getAttrib(b, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "b must be a 2D-array, but length(dim(b)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int b__dim_1_ = b__dim_[0];
        const int b__dim_2_ = b__dim_[1];
        
        if (a__dim_1_ != b__dim_1_)
          Rf_error("dim(b)[1] must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)b__dim_1_, (double)a__dim_1_);
        const R_xlen_t out___len_ = (a__dim_2_) * (b__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = a__dim_2_;
          _dim[1] = b__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out___,
          a__dim_1_,
          a__dim_2_,
          b__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

