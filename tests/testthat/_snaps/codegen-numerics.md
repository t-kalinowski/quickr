# diag extracts the shorter axis of a rectangular matrix

    Code
      fn
    Output
      function(a) {
          declare(type(a = double(m, n)))
          diag(a)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, out_, a__dim_1_, a__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(out) :: out_(min(a__dim_1_, a__dim_2_))
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          do btmp1_ = 1_c_int, int(min(a__dim_1_, a__dim_2_), kind=c_int)
            out_(btmp1_) = a(btmp1_, btmp1_)
          end do
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        double* const out___,
        const R_len_t a__dim_1_,
        const R_len_t a__dim_2_);
      
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
        
        const R_xlen_t out___len_ = ((a__dim_1_) < (a__dim_2_) ? (a__dim_1_) : (a__dim_2_));
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          out___,
          a__dim_1_,
          a__dim_2_);
        
        UNPROTECT(1);
        return out_;
      }

# diag constructs a matrix from vector values

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          diag(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(x__len_, x__len_)
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          out_ = 0.0_c_double
          do btmp1_ = 1_c_int, int(x__len_, kind=c_int)
            out_(btmp1_, btmp1_) = x(1_c_int + mod(btmp1_ - 1_c_int, int(x__len_, kind=c_int)))
          end do
        end block
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
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = (x__len_) * (x__len_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = x__len_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(2);
        return out_;
      }

# diag broadcasts a scalar over a rectangular diagonal

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          diag(x, nrow = 2, ncol = 3)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_(2, 3)
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          out_ = 0.0_c_double
          do btmp1_ = 1_c_int, int(2, kind=c_int)
            out_(btmp1_, btmp1_) = x
          end do
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out___);
      
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
        const R_xlen_t out___len_ = (2) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# diag constructs an identity with a runtime size

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          diag(nrow = n)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(n, n)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp1_ = 1.0_c_double
          out_ = 0.0_c_double
          do btmp2_ = 1_c_int, int(n, kind=c_int)
            out_(btmp2_, btmp2_) = btmp1_
          end do
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, double* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = (_as_int_n) * (_as_int_n);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = _as_int_n;
          _dim[1] = _as_int_n;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(n__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# SVD returns all three components through a named C list

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 2)))
          result <- svd(x)
          result
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, tmp1_, tmp2_, tmp3_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(3, 2)
        real(c_double), intent(out) :: tmp1_(2)
        real(c_double), intent(out) :: tmp2_(3, 2)
        real(c_double), intent(out) :: tmp3_(2, 2)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_(3, 2)
          real(c_double) :: btmp2_(2, 2)
          integer(c_int) :: btmp3_
          integer(c_int) :: btmp4_
          real(c_double), allocatable :: btmp5_(:)
          integer(c_int), allocatable :: btmp6_(:)
          real(c_double), allocatable :: btmp7_(:)
      
          allocate(btmp5_((1 + 0)))
          allocate(btmp6_((8 * 2)))
          btmp1_ = x
          btmp4_ = -1_c_int
      call dgesdd('S', int(3, kind=c_int), int(2, kind=c_int), btmp1_, int(3, kind=c_int), tmp1_, tmp2_, int(3, kind=c_int), btmp2_,&
      & int(2, kind=c_int), btmp5_, btmp4_, btmp6_, btmp3_)
          btmp4_ = int(btmp5_(1), kind=c_int)
          allocate(btmp7_(btmp4_))
      call dgesdd('S', int(3, kind=c_int), int(2, kind=c_int), btmp1_, int(3, kind=c_int), tmp1_, tmp2_, int(3, kind=c_int), btmp2_,&
      & int(2, kind=c_int), btmp7_, btmp4_, btmp6_, btmp3_)
          if (btmp3_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd: illegal argument")
            return
          end if
          if (btmp3_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd failed to converge")
            return
          end if
          tmp3_ = transpose(btmp2_)
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
        double* const tmp1___,
        double* const tmp2___,
        double* const tmp3___,
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
        const R_xlen_t tmp1___len_ = 2;
        SEXP tmp1_ = PROTECT(Rf_allocVector(REALSXP, tmp1___len_));
        double* tmp1___ = REAL(tmp1_);
        const R_xlen_t tmp2___len_ = (3) * (2);
        SEXP tmp2_ = PROTECT(Rf_allocVector(REALSXP, tmp2___len_));
        double* tmp2___ = REAL(tmp2_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = 2;
          Rf_dimgets(tmp2_, _dim_sexp);
        }
        const R_xlen_t tmp3___len_ = (2) * (2);
        SEXP tmp3_ = PROTECT(Rf_allocVector(REALSXP, tmp3___len_));
        double* tmp3___ = REAL(tmp3_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 2;
          Rf_dimgets(tmp3_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          tmp1___,
          tmp2___,
          tmp3___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 3));
        SET_VECTOR_ELT(_ans, 0, tmp1_);
        SET_VECTOR_ELT(_ans, 1, tmp2_);
        SET_VECTOR_ELT(_ans, 2, tmp3_);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(_names, 0, Rf_mkChar("d"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("u"));
        SET_STRING_ELT(_names, 2, Rf_mkChar("v"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(7);
        return _ans;
      }

# captured SVD components retain stored dimensions

    Code
      fn
    Output
      function(n, x) {
          declare(type(n = integer(1)), type(x = double(n, n)))
          result <- svd(x)
          inner <- function(n) sum(result$u %*% result$u)
          inner(1L)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: x(n, n)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: tmp1_(:)
        real(c_double), allocatable :: tmp2_(:, :)
        real(c_double), allocatable :: tmp3_(:, :)
        ! manifest end
      
        allocate(tmp1_(min(n, n)))
        allocate(tmp2_(n, min(n, n)))
        allocate(tmp3_(n, min(n, n)))
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
          real(c_double), allocatable :: btmp2_(:, :)
          integer(c_int) :: btmp3_
          integer(c_int) :: btmp4_
          real(c_double), allocatable :: btmp5_(:)
          integer(c_int), allocatable :: btmp6_(:)
          real(c_double), allocatable :: btmp7_(:)
      
          allocate(btmp1_(n, n))
          allocate(btmp2_(min(n, n), n))
          allocate(btmp5_((1 + 0)))
          allocate(btmp6_((8 * min(n, n))))
          btmp1_ = x
          btmp4_ = -1_c_int
      call dgesdd('S', int(n, kind=c_int), int(n, kind=c_int), btmp1_, int(n, kind=c_int), tmp1_, tmp2_, int(n, kind=c_int), btmp2_,&
      & int(min(n, n), kind=c_int), btmp5_, btmp4_, btmp6_, btmp3_)
          btmp4_ = int(btmp5_(1), kind=c_int)
          allocate(btmp7_(btmp4_))
      call dgesdd('S', int(n, kind=c_int), int(n, kind=c_int), btmp1_, int(n, kind=c_int), tmp1_, tmp2_, int(n, kind=c_int), btmp2_,&
      & int(min(n, n), kind=c_int), btmp7_, btmp4_, btmp6_, btmp3_)
          if (btmp3_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd: illegal argument")
            return
          end if
          if (btmp3_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesdd failed to converge")
            return
          end if
          tmp3_ = transpose(btmp2_)
        end block
      
        call inner(1_c_int, out_)
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine inner(n, res)
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
            integer(c_int), intent(in) :: n
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double), allocatable :: btmp1_(:, :)
      
              if (size(tmp2_, 2, kind=c_ptrdiff_t) /= size(tmp2_, 1, kind=c_ptrdiff_t)) then
                call quickr_set_error_msg("non-conformable arguments in %*%")
                return
              end if
              allocate(btmp1_(size(tmp2_, 1, kind=c_ptrdiff_t), size(tmp2_, 2, kind=c_ptrdiff_t)))
          if (int(size(tmp2_, 1, kind=c_ptrdiff_t), kind=c_int) == 0_c_int .or. int(size(tmp2_, 2, kind=c_ptrdiff_t), kind=c_int) ==&
          & 0_c_int) then
                btmp1_ = 0.0_c_double
              else
          call dgemm('N','N', int(size(tmp2_, 1, kind=c_ptrdiff_t), kind=c_int), int(size(tmp2_, 2, kind=c_ptrdiff_t), kind=c_int),&
          & int(size(tmp2_, 2, kind=c_ptrdiff_t), kind=c_int), 1.0_c_double, tmp2_, int(size(tmp2_, 1, kind=c_ptrdiff_t), kind=c_int),&
          & tmp2_, int(size(tmp2_, 1, kind=c_ptrdiff_t), kind=c_int), 0.0_c_double, btmp1_, int(size(tmp2_, 1, kind=c_ptrdiff_t),&
          & kind=c_int))
              end if
              res = sum(btmp1_)
            end block
          end subroutine
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
        const double* const x__,
        double* const out___,
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
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != x__dim_1_)
          Rf_error("dim(x)[1] must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__dim_1_, (double)_as_int_n);
        if (_as_int_n != x__dim_2_)
          Rf_error("dim(x)[2] must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__dim_2_, (double)_as_int_n);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          x__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# scalar extrema use min and max without array reduction

    Code
      fn
    Output
      function(m) {
          declare(type(m = integer(2, 2)))
          lo <- 1L
          hi <- 2L
          min(max(m[1, 1], lo), hi)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: m(2, 2)
        integer(c_int), intent(out) :: out_
      
        ! locals
        integer(c_int) :: lo
        integer(c_int) :: hi
        ! manifest end
      
      
        lo = 1_c_int
        hi = 2_c_int
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp1_ = m(1_c_int, 1_c_int)
      
          btmp2_ = max(btmp1_, lo)
      
          out_ = min(btmp2_, hi)
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const m__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 2)
          Rf_error("dim(m)[1] must be 2, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 2)
          Rf_error("dim(m)[2] must be 2, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(m__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# retained singleton dimensions use an array reduction

    Code
      fn
    Output
      function(a, m) {
          declare(type(a = integer(1)), type(m = integer(NA, 2)))
          min(m[a, 1L, drop = FALSE])
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, m, out_, m__dim_1_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: m__dim_1_
      
        ! args
        integer(c_int), intent(in) :: a
        integer(c_int), intent(in) :: m(m__dim_1_, 2)
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
        out_ = minval(m(a, 1_c_int:1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const a__,
        const int* const m__,
        int* const out___,
        const R_len_t m__dim_1_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != INTSXP) {
          Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = INTEGER(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (m__dim_2_ != 2)
          Rf_error("dim(m)[2] must be 2, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(
          a__,
          m__,
          out___,
          m__dim_1_);
        
        UNPROTECT(1);
        return out_;
      }

# numeric reductions emit identities with no data arguments

    Code
      fn
    Output
      function() {
          c(sum(), prod(na.rm = FALSE))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_(2)
        ! manifest end
      
        out_ = [ real(0_c_int, kind=c_double), 1.0_c_double ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(out___);
        
        UNPROTECT(1);
        return out_;
      }

# logical reductions emit identities with no data arguments

    Code
      fn
    Output
      function() {
          c(any(na.rm = FALSE), all())
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(out) :: out_(2) ! logical
        ! manifest end
      
        out_ = [ .false., .true. ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(out___);
        
        UNPROTECT(1);
        return out_;
      }

# named numeric data and exact na.rm preserve multi-argument reductions

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(NA)), type(y = double(NA)))
          c(
            sum(first = x, na.r = y, na.rm = FALSE),
            prod(first = x, na.rm = FALSE, second = y)
          )
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(y__len_)
        real(c_double), intent(out) :: out_(2)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
          real(c_double) :: btmp3_
          real(c_double) :: btmp4_
      
          btmp1_ = sum(x)
          btmp2_ = sum(y)
          btmp3_ = product(x)
          btmp4_ = product(y)
          out_ = [ (btmp1_ + btmp2_), (btmp3_ * btmp4_) ]
        end block
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
        double* const out___,
        const R_xlen_t x__len_,
        const R_xlen_t y__len_);
      
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
        
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          y__,
          out___,
          x__len_,
          y__len_);
        
        UNPROTECT(1);
        return out_;
      }

# named logical reductions combine independently reduced arguments

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = logical(NA)), type(y = logical(NA)))
          c(
            any(first = x, na.rm = FALSE, second = y),
            all(first = x, na.rm = FALSE, second = y)
          )
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(in) :: y(y__len_) ! logical
        integer(c_int), intent(out) :: out_(2) ! logical
        ! manifest end
      
      
        out_ = [ (any((x/=0)) .or. any((y/=0))), (all((x/=0)) .and. all((y/=0))) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const y__,
        int* const out___,
        const R_xlen_t x__len_,
        const R_xlen_t y__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != LGLSXP) {
          Rf_error("typeof(y) must be 'logical', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const int* const y__ = LOGICAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(
          x__,
          y__,
          out___,
          x__len_,
          y__len_);
        
        UNPROTECT(1);
        return out_;
      }

# sum and extrema cast logical data before calling intrinsics

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(n)))
          c(sum(x), min(x), max(x))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out_(3)
        ! manifest end
      
      
        block
          logical, allocatable :: btmp1_(:) ! logical
          logical, allocatable :: btmp2_(:) ! logical
      
          allocate(btmp1_(x__len_))
          allocate(btmp2_(x__len_))
          btmp1_ = (x/=0)
          if (size(btmp1_, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
          btmp2_ = (x/=0)
          if (size(btmp2_, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
      out_ = [ sum(merge(1_c_int, 0_c_int, (x/=0))), minval(merge(1_c_int, 0_c_int, btmp1_)), maxval(merge(1_c_int, 0_c_int, btmp2_)) ]
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
        const int* const x__,
        int* const out___,
        const R_xlen_t x__len_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out___,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# logical scalar and singleton reductions use their value directly

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(1)))
          c(any(x), all(c(x)))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x ! logical
        integer(c_int), intent(out) :: out_(2) ! logical
        ! manifest end
      
      
        out_ = [ (x/=0), (x/=0) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const x__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# extrema discard a statically empty argument

    Code
      fn
    Output
      function() {
          min(numeric(), 5)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_
        ! manifest end
      
        block
          real(c_double) :: btmp1_(0)
      
          btmp1_ = 0.0_c_double
      
          out_ = 5.0_c_double
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(out___);
        
        UNPROTECT(1);
        return out_;
      }

# extrema track whether any dynamic argument is nonempty

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(NA)), type(y = double(NA)))
          min(x, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(y__len_)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp2_ = 0_c_int
      
          if (size(x, kind=c_ptrdiff_t) > 0_c_ptrdiff_t) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = minval(x)
              btmp2_ = 1_c_int
            else
              btmp1_ = min(btmp1_, minval(x))
            end if
          end if
      
          if (size(y, kind=c_ptrdiff_t) > 0_c_ptrdiff_t) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = minval(y)
              btmp2_ = 1_c_int
            else
              btmp1_ = min(btmp1_, minval(y))
            end if
          end if
          if (btmp2_ == 0_c_int) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
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
        const double* const y__,
        double* const out___,
        const R_xlen_t x__len_,
        const R_xlen_t y__len_,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          out___,
          x__len_,
          y__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# numeric masked reductions pass masks to Fortran intrinsics

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          c(sum(x[x > 0]), prod(x[x > 0]))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(2)
        ! manifest end
      
      
        out_ = [ sum(x, mask = (x > 0_c_int)), product(x, mask = (x > 0_c_int)) ]
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
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# logical masked reductions avoid packing the selected array

    Code
      fn
    Output
      function(x, mask) {
          declare(type(x = logical(n)), type(mask = logical(n)))
          c(any(x[mask]), all(x[mask]))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, out_, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(in) :: mask(x__len_) ! logical
        integer(c_int), intent(out) :: out_(2) ! logical
        ! manifest end
      
      
        out_ = [ any((((x/=0)) .and. ((mask/=0)))), all(((.not. ((mask/=0))) .or. ((x/=0)))) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const mask__,
        int* const out___,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != mask__len_)
          Rf_error("length(mask) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)mask__len_, (double)x__len_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(
          x__,
          mask__,
          out___,
          x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# scalar masked reductions retain empty-selection identities

    Code
      fn
    Output
      function(x, mask) {
          declare(type(x = logical(1)), type(mask = logical(1)))
          c(any(x[mask]), all(x[c(mask)]))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x ! logical
        integer(c_int), intent(in) :: mask ! logical
        integer(c_int), intent(out) :: out_(2) ! logical
        ! manifest end
      
      
        out_ = [ merge((x/=0), .false., (mask/=0)), merge((x/=0), .true., (mask/=0)) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const mask__,
        int* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, mask__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# singleton extrema indices still evaluate effectful operands

    Code
      fn
    Output
      function() {
          hi <- which.max(c(runif(1L)))
          lo <- which.min(c(runif(1L)))
          c(hi, lo, runif(1L))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_(3)
      
        ! locals
        integer(c_int) :: hi
        integer(c_int) :: lo
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = unif_rand()
          btmp2_ = btmp1_
          hi = 1_c_int
        end block
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = unif_rand()
          btmp2_ = btmp1_
          lo = 1_c_int
        end block
        block
          real(c_double) :: btmp1_
      
          btmp1_ = unif_rand()
          out_ = [ real(hi, kind=c_double), real(lo, kind=c_double), btmp1_ ]
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

# extrema indices search logical storage and boolean expressions

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          positive <- x > 0
          c(
            which.max(positive),
            which.min(positive),
            which.max(x > 1),
            which.min(x > 1)
          )
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out_(4)
      
        ! locals
        logical, allocatable :: positive(:) ! logical
        ! manifest end
      
        allocate(positive(x__len_))
      
      
        positive = (x > 0.0_c_double)
      out_ = [ max(1_c_int, findloc(positive, .true., 1, kind=c_int)), max(1_c_int, findloc(positive, .false., 1, kind=c_int)),&
      & max(1_c_int, findloc(merge(1_c_int, 0_c_int, (x > 1.0_c_double)), 1_c_int, 1, kind=c_int)), max(1_c_int, findloc(merge(1_c_int,&
      & 0_c_int, (x > 1.0_c_double)), 0_c_int, 1, kind=c_int)) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        int* const out___,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = 4;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# multi-argument extrema materialize operands in evaluation order

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          max(runif(1), x + runif(3))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        block
          real(c_double) :: btmp1_(3)
          real(c_double), allocatable :: btmp2_(:)
          real(c_double) :: btmp3_
          integer(c_int) :: btmp4_
      
          allocate(btmp2_(x__len_))
          btmp4_ = 0_c_int
      
          if (btmp4_ == 0_c_int) then
            btmp3_ = unif_rand()
            btmp4_ = 1_c_int
          else
            btmp3_ = max(btmp3_, unif_rand())
          end if
          btmp1_ = [(unif_rand(), tmp1_=1, 3)]
          if (size(x, kind=c_ptrdiff_t) /= 3) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
            return
          end if
          btmp2_ = (x + btmp1_)
          if (size(btmp2_, kind=c_ptrdiff_t) > 0_c_ptrdiff_t) then
            if (btmp4_ == 0_c_int) then
              btmp3_ = maxval(btmp2_)
              btmp4_ = 1_c_int
            else
              btmp3_ = max(btmp3_, maxval(btmp2_))
            end if
          end if
          if (btmp4_ == 0_c_int) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
          out_ = btmp3_
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const double* const x__,
        double* const out___,
        const R_xlen_t x__len_,
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(
          x__,
          out___,
          x__len_,
          quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# ceiling evaluates a reused effectful operand once

    Code
      fn
    Output
      function() {
          out <- ceiling(runif(1L) * 10)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
      
          btmp1_ = unif_rand()
          btmp2_ = (btmp1_ * 10.0_c_double)
          out = (aint(btmp2_) + merge(1.0_c_double, 0.0_c_double, (btmp2_ > aint(btmp2_))))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(double* const out__);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        GetRNGstate();
        fn(out__);
        PutRNGstate();
        
        UNPROTECT(1);
        return out;
      }

# floor and ceiling reuse names without introducing temporaries

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- floor(x) + ceiling(x)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
      if (size((aint(x) - merge(1.0_c_double, 0.0_c_double, (x < aint(x)))), kind=c_ptrdiff_t) /= size((aint(x) + merge(1.0_c_double,&
      & 0.0_c_double, (x > aint(x)))), kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
      out = ((aint(x) - merge(1.0_c_double, 0.0_c_double, (x < aint(x)))) + (aint(x) + merge(1.0_c_double, 0.0_c_double, (x > aint(x)))))
      
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
        const R_xlen_t x__len_,
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
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

# derived constants remain runtime math operands

    Code
      fn
    Output
      function() {
          c(
            sqrt(floor(0.5) - 1),
            log10(floor(0.5)),
            exp(floor(1000.5)),
            (floor(0.5) - 1)^0.5
          )
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_(4)
        ! manifest end
      
        block
          real(c_double) :: btmp1_
          real(c_double) :: btmp2_
          real(c_double) :: btmp3_
          real(c_double) :: btmp4_
          real(c_double) :: btmp5_
      
          btmp1_ = ((aint(0.5_c_double) - merge(1.0_c_double, 0.0_c_double, (0.5_c_double < aint(0.5_c_double)))) - 1.0_c_double)
          btmp2_ = (aint(0.5_c_double) - merge(1.0_c_double, 0.0_c_double, (0.5_c_double < aint(0.5_c_double))))
          btmp3_ = (aint(1000.5_c_double) - merge(1.0_c_double, 0.0_c_double, (1000.5_c_double < aint(1000.5_c_double))))
          btmp4_ = (((aint(0.5_c_double) - merge(1.0_c_double, 0.0_c_double, (0.5_c_double < aint(0.5_c_double)))) - 1.0_c_double))
          btmp5_ = 0.5_c_double
          out_ = [ sqrt(btmp1_), log10(btmp2_), exp(btmp3_), (btmp4_ ** (btmp5_)) ]
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = 4;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(out___);
        
        UNPROTECT(1);
        return out_;
      }

