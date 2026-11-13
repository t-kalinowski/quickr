# GEMM checks independent dimensions before allocating a local

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n, k)), type(b = double(m, p)))
          out <- a %*% b
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, a__dim_2_, b__dim_1_, b__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
        integer(c_int), intent(in), value :: b__dim_1_
        integer(c_int), intent(in), value :: b__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: b(b__dim_1_, b__dim_2_)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        ! manifest end
      
      
        if (size(a, 2, kind=c_ptrdiff_t) /= size(b, 1, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("non-conformable arguments in %*%")
          return
        end if
        if (.not. allocated(out)) allocate(out(a__dim_1_, b__dim_2_))
      if (int(a__dim_1_, kind=c_int) == 0_c_int .or. int(b__dim_2_, kind=c_int) == 0_c_int .or. int(a__dim_2_, kind=c_int) == 0_c_int)&
      & then
          out = 0.0_c_double
        else
      call dgemm('N','N', int(a__dim_1_, kind=c_int), int(b__dim_2_, kind=c_int), int(a__dim_2_, kind=c_int), 1.0_c_double, a,&
      & int(a__dim_1_, kind=c_int), b, int(b__dim_1_, kind=c_int), 0.0_c_double, out, int(a__dim_1_, kind=c_int))
        end if
      
        out_ = sum(out)
      
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
        const R_len_t b__dim_1_,
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
        
        const R_xlen_t out___len_ = (1);
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
          b__dim_1_,
          b__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# GEMV checks its vector length before allocating a temporary

    Code
      fn
    Output
      function(a, x) {
          declare(type(a = double(m, k)), type(x = double(n)))
          sum(a %*% x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, x, out_, a__dim_1_, a__dim_2_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
      
          if (size(a, 2, kind=c_ptrdiff_t) /= size(x, kind=c_ptrdiff_t)) then
            call quickr_set_error_msg("non-conformable arguments in %*%")
            return
          end if
          allocate(btmp1_(a__dim_1_, 1))
          if (int(a__dim_1_, kind=c_int) == 0_c_int .or. int(a__dim_2_, kind=c_int) == 0_c_int) then
            btmp1_ = 0.0_c_double
          else
      call dgemv('N', int(a__dim_1_, kind=c_int), int(a__dim_2_, kind=c_int), 1.0_c_double, a, int(a__dim_1_, kind=c_int), x, 1_c_int,&
      & 0.0_c_double, btmp1_, 1_c_int)
          end if
          out_ = sum(btmp1_)
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
        const double* const x__,
        double* const out___,
        const R_len_t a__dim_1_,
        const R_len_t a__dim_2_,
        const R_xlen_t x__len_,
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
        
        
        fn(
          a__,
          x__,
          out___,
          a__dim_1_,
          a__dim_2_,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# an explicit vector transpose materializes a row matrix for GEMM

    Code
      fn
    Output
      function(x, m) {
          declare(type(x = double(NA)), type(m = double(3, 3)))
          t(x) %*% m
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, m, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: m(3, 3)
        real(c_double), intent(out) :: out_(1, 3)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
      
          allocate(btmp1_(1, x__len_))
          btmp1_ = reshape(x, [1, int(x__len_)])
          if (size(btmp1_, 2, kind=c_ptrdiff_t) /= 3) then
            call quickr_set_error_msg("non-conformable arguments in %*%")
            return
          end if
          if (int(x__len_, kind=c_int) == 0_c_int) then
            out_ = 0.0_c_double
          else
      call dgemm('N','N', int(1, kind=c_int), int(3, kind=c_int), int(x__len_, kind=c_int), 1.0_c_double, btmp1_, int(1, kind=c_int), m,&
      & int(3, kind=c_int), 0.0_c_double, out_, int(1, kind=c_int))
          end if
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
        const double* const m__,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != REALSXP) {
          Rf_error("typeof(m) must be 'double', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const double* const m__ = REAL(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 3)
          Rf_error("dim(m)[1] must be 3, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 3)
          Rf_error("dim(m)[2] must be 3, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out___len_ = (1) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 1;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          m__,
          out___,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# a vector-matrix product uses transposed GEMV

    Code
      fn
    Output
      function(x, m) {
          declare(type(x = double(NA)), type(m = double(3, 3)))
          x %*% m
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, m, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: m(3, 3)
        real(c_double), intent(out) :: out_(1, 3)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= 3) then
          call quickr_set_error_msg("non-conformable arguments in %*%")
          return
        end if
      call dgemv('T', int(3, kind=c_int), int(3, kind=c_int), 1.0_c_double, m, int(3, kind=c_int), x, 1_c_int, 0.0_c_double, out_,&
      & 1_c_int)
      
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
        const double* const m__,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != REALSXP) {
          Rf_error("typeof(m) must be 'double', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const double* const m__ = REAL(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 3)
          Rf_error("dim(m)[1] must be 3, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 3)
          Rf_error("dim(m)[2] must be 3, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out___len_ = (1) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 1;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          m__,
          out___,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# a vector inner product checks whole vector sizes

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(NA)), type(y = double(NA)))
          x %*% y
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
        real(c_double), intent(out) :: out_(1, 1)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("non-conformable arguments in %*%")
          return
        end if
        if (int(x__len_, kind=c_int) == 0_c_int) then
          out_ = 0.0_c_double
        else
      call dgemm('N','N', int(1, kind=c_int), int(1, kind=c_int), int(x__len_, kind=c_int), 1.0_c_double, x, int(1, kind=c_int), y,&
      & int(y__len_, kind=c_int), 0.0_c_double, out_, int(1, kind=c_int))
        end if
      
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
        
        const R_xlen_t out___len_ = (1) * (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 1;
          _dim[1] = 1;
          Rf_dimgets(out_, _dim_sexp);
        }
        
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
        
        UNPROTECT(2);
        return out_;
      }

# GEMM consumes transpose flags without materializing transposes

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(4, 3)), type(y = double(5, 4)))
          t(x) %*% t(y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(4, 3)
        real(c_double), intent(in) :: y(5, 4)
        real(c_double), intent(out) :: out_(3, 5)
        ! manifest end
      
      
      call dgemm('T','T', int(3, kind=c_int), int(5, kind=c_int), int(4, kind=c_int), 1.0_c_double, x, int(4, kind=c_int), y, int(5,&
      & kind=c_int), 0.0_c_double, out_, int(3, kind=c_int))
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
        double* const out___);
      
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const int* const y__dim_ = ({
        SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "y must be a 2D-array, but length(dim(y)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int y__dim_1_ = y__dim_[0];
        const int y__dim_2_ = y__dim_[1];
        
        if (x__dim_1_ != 4)
          Rf_error("dim(x)[1] must be 4, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 3)
          Rf_error("dim(x)[2] must be 3, not %0.f",
                    (double)x__dim_2_);
        if (y__dim_1_ != 5)
          Rf_error("dim(y)[1] must be 5, not %0.f",
                    (double)y__dim_1_);
        if (y__dim_2_ != 4)
          Rf_error("dim(y)[2] must be 4, not %0.f",
                    (double)y__dim_2_);
        const R_xlen_t out___len_ = (3) * (5);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = 5;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, y__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# BLAS operands are promoted from integer and logical storage

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = integer(2, 3)), type(y = logical(3, 2)))
          x %*% y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x(2, 3)
        integer(c_int), intent(in) :: y(3, 2) ! logical
        real(c_double), intent(out) :: out_(2, 2)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_(2, 3)
          real(c_double) :: btmp2_(3, 2)
      
          btmp1_ = real(x, kind=c_double)
          btmp2_ = merge(1.0_c_double, 0.0_c_double, (y/=0))
      call dgemm('N','N', int(2, kind=c_int), int(2, kind=c_int), int(3, kind=c_int), 1.0_c_double, btmp1_, int(2, kind=c_int), btmp2_,&
      & int(3, kind=c_int), 0.0_c_double, out_, int(2, kind=c_int))
        end block
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
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != LGLSXP) {
          Rf_error("typeof(y) must be 'logical', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const int* const y__ = LOGICAL(y);
        const int* const y__dim_ = ({
        SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "y must be a 2D-array, but length(dim(y)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int y__dim_1_ = y__dim_[0];
        const int y__dim_2_ = y__dim_[1];
        
        if (x__dim_1_ != 2)
          Rf_error("dim(x)[1] must be 2, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 3)
          Rf_error("dim(x)[2] must be 3, not %0.f",
                    (double)x__dim_2_);
        if (y__dim_1_ != 3)
          Rf_error("dim(y)[1] must be 3, not %0.f",
                    (double)y__dim_1_);
        if (y__dim_2_ != 2)
          Rf_error("dim(y)[2] must be 2, not %0.f",
                    (double)y__dim_2_);
        const R_xlen_t out___len_ = (2) * (2);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 2;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, y__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# repeated SYRK calls reuse a symbolic destination

    Code
      fn
    Output
      function(x, steps) {
          declare(type(x = double(n, k)), type(steps = integer(1)))
          out <- matrix(0, ncol(x), ncol(x))
          for (i in seq_len(steps)) {
            out <- crossprod(x)
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, steps, out, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
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
        integer(c_int), intent(in) :: steps
        real(c_double), intent(out) :: out(x__dim_2_, x__dim_2_)
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        do tmp1_ = 1, steps
          i = tmp1_
          block
            integer(c_int) :: btmp1_
            integer(c_int) :: btmp2_
      
            if ((size(out, 1, kind=c_ptrdiff_t)) /= (x__dim_2_)) then
              call quickr_set_error_msg("reassignment must preserve the shape of `out`")
              return
            end if
            if ((size(out, 2, kind=c_ptrdiff_t)) /= (x__dim_2_)) then
              call quickr_set_error_msg("reassignment must preserve the shape of `out`")
              return
            end if
            if (int(x__dim_2_, kind=c_int) == 0_c_int .or. int(x__dim_1_, kind=c_int) == 0_c_int) then
              out = 0.0_c_double
            else
      call dsyrk('U', 'T', int(x__dim_2_, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int),&
      & 0.0_c_double, out, int(x__dim_2_, kind=c_int))
            end if
            do btmp2_ = 1_c_int, int(x__dim_2_, kind=c_int) - 1_c_int
              do btmp1_ = btmp2_ + 1_c_int, int(x__dim_2_, kind=c_int)
                out(btmp1_, btmp2_) = out(btmp2_, btmp1_)
              end do
            end do
          end block
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
        const int* const steps__,
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
        
        // steps
        _args = CDR(_args);
        SEXP steps = CAR(_args);
        if (TYPEOF(steps) != INTSXP) {
          Rf_error("typeof(steps) must be 'integer', not '%s'", Rf_type2char(TYPEOF(steps)));
        }
        const int* const steps__ = INTEGER(steps);
        const R_xlen_t steps__len_ = Rf_xlength(steps);
        
        if (steps__len_ != 1)
          Rf_error("length(steps) must be 1, not %0.f",
                    (double)steps__len_);
        const R_xlen_t out__len_ = (x__dim_2_) * (x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_2_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          steps__,
          out__,
          x__dim_1_,
          x__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out;
      }

# tcrossprod uses untransposed SYRK and fills both triangles

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 2)))
          tcrossprod(x)
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
        real(c_double), intent(in) :: x(3, 2)
        real(c_double), intent(out) :: out_(3, 3)
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
      
      call dsyrk('U', 'N', int(3, kind=c_int), int(2, kind=c_int), 1.0_c_double, x, int(3, kind=c_int), 0.0_c_double, out_, int(3,&
      & kind=c_int))
          do btmp2_ = 1_c_int, int(3, kind=c_int) - 1_c_int
            do btmp1_ = btmp2_ + 1_c_int, int(3, kind=c_int)
              out_(btmp1_, btmp2_) = out_(btmp2_, btmp1_)
            end do
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
        const R_xlen_t out___len_ = (3) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# two-argument crossprod uses GEMM with a column-vector operand

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n, k)), type(y = double(n)))
          crossprod(x, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(in) :: y(x__dim_1_)
        real(c_double), intent(out) :: out_(x__dim_2_, 1)
        ! manifest end
      
      
        if (int(x__dim_2_, kind=c_int) == 0_c_int .or. int(x__dim_1_, kind=c_int) == 0_c_int) then
          out_ = 0.0_c_double
        else
      call dgemm('T','N', int(x__dim_2_, kind=c_int), int(1, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_,&
      & kind=c_int), y, int(x__dim_1_, kind=c_int), 0.0_c_double, out_, int(x__dim_2_, kind=c_int))
        end if
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
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_);
      
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__dim_1_ != y__len_)
          Rf_error("length(y) must equal dim(x)[1],"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__dim_1_);
        const R_xlen_t out___len_ = (x__dim_2_) * (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_2_;
          _dim[1] = 1;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          x__,
          y__,
          out___,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out_;
      }

# outer multiplication initializes a GER destination

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          outer(x, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(y__len_)
        real(c_double), intent(out) :: out_(x__len_, y__len_)
        ! manifest end
      
      
        if (int(x__len_, kind=c_int) == 0_c_int .or. int(y__len_, kind=c_int) == 0_c_int) then
          out_ = 0.0_c_double
        else
          out_ = 0.0_c_double
      call dger(int(x__len_, kind=c_int), int(y__len_, kind=c_int), 1.0_c_double, x, 1_c_int, y, 1_c_int, out_, int(x__len_, kind=c_int))
        end if
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
        
        const R_xlen_t out___len_ = (x__len_) * (y__len_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = y__len_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          x__,
          y__,
          out___,
          x__len_,
          y__len_);
        
        UNPROTECT(2);
        return out_;
      }

# BLAS destination checks use stored extents after rebinding

    Code
      fn
    Output
      function(x, m) {
          declare(type(x = double(n, k)), type(m = integer(1)))
          out <- matrix(0, m, m)
          m <- m + 1L
          out <- crossprod(x)
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, m, out_, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
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
        integer(c_int), intent(in out) :: m
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        ! manifest end
      
        allocate(out(m, m))
      
      
        out = 0.0_c_double
        m = (m + 1_c_int)
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
      
          if ((size(out, 1, kind=c_ptrdiff_t)) /= (x__dim_2_)) then
            call quickr_set_error_msg("reassignment must preserve the shape of `out`")
            return
          end if
          if ((size(out, 2, kind=c_ptrdiff_t)) /= (x__dim_2_)) then
            call quickr_set_error_msg("reassignment must preserve the shape of `out`")
            return
          end if
          if (int(x__dim_2_, kind=c_int) == 0_c_int .or. int(x__dim_1_, kind=c_int) == 0_c_int) then
            out = 0.0_c_double
          else
      call dsyrk('U', 'T', int(x__dim_2_, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int),&
      & 0.0_c_double, out, int(x__dim_2_, kind=c_int))
          end if
          do btmp2_ = 1_c_int, int(x__dim_2_, kind=c_int) - 1_c_int
            do btmp1_ = btmp2_ + 1_c_int, int(x__dim_2_, kind=c_int)
              out(btmp1_, btmp2_) = out(btmp2_, btmp1_)
            end do
          end do
        end block
        out_ = sum(out)
      
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
        int* const m__,
        double* const out___,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        m = Rf_duplicate(m);
        SETCAR(_args, m);
        int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          m__,
          out___,
          x__dim_1_,
          x__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# BLAS destinations check literal singleton result axes

    Code
      fn
    Output
      function(x, m) {
          declare(type(x = double(n, 1)), type(m = integer(1)))
          out <- matrix(0, m, m)
          out <- crossprod(x)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, m, out, x__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, 1)
        integer(c_int), intent(in) :: m
        real(c_double), intent(out) :: out(m, m)
        ! manifest end
      
      
        out = 0.0_c_double
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
      
          if ((size(out, 1, kind=c_ptrdiff_t)) /= (1)) then
            call quickr_set_error_msg("reassignment must preserve the shape of `out`")
            return
          end if
          if ((size(out, 2, kind=c_ptrdiff_t)) /= (1)) then
            call quickr_set_error_msg("reassignment must preserve the shape of `out`")
            return
          end if
          if (int(x__dim_1_, kind=c_int) == 0_c_int) then
            out = 0.0_c_double
          else
      call dsyrk('U', 'T', int(1, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int), 0.0_c_double,&
      & out, int(1, kind=c_int))
          end if
          do btmp2_ = 1_c_int, int(1, kind=c_int) - 1_c_int
            do btmp1_ = btmp2_ + 1_c_int, int(1, kind=c_int)
              out(btmp1_, btmp2_) = out(btmp2_, btmp1_)
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
        const double* const x__,
        const int* const m__,
        double* const out__,
        const R_len_t x__dim_1_,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        if (x__dim_2_ != 1)
          Rf_error("dim(x)[2] must be 1, not %0.f",
                    (double)x__dim_2_);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const int _as_int_m = Rf_asInteger(m);
        const R_xlen_t out__len_ = (_as_int_m) * (_as_int_m);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = _as_int_m;
          _dim[1] = _as_int_m;
          Rf_dimgets(out, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          m__,
          out__,
          x__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out;
      }

# aliased BLAS input is protected by a separate result

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n, n)))
          x <- crossprod(x)
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, x__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in out) :: x(x__dim_1_, x__dim_1_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          allocate(btmp1_(x__dim_1_, x__dim_1_))
          if (int(x__dim_1_, kind=c_int) == 0_c_int) then
            btmp1_ = 0.0_c_double
          else
      call dsyrk('U', 'T', int(x__dim_1_, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int),&
      & 0.0_c_double, btmp1_, int(x__dim_1_, kind=c_int))
          end if
          do btmp3_ = 1_c_int, int(x__dim_1_, kind=c_int) - 1_c_int
            do btmp2_ = btmp3_ + 1_c_int, int(x__dim_1_, kind=c_int)
              btmp1_(btmp2_, btmp3_) = btmp1_(btmp3_, btmp2_)
            end do
          end do
          if ((size(x, 1, kind=c_ptrdiff_t)) /= (size(btmp1_, 1, kind=c_ptrdiff_t))) then
            call quickr_set_error_msg("reassignment must preserve the shape of `x`")
            return
          end if
          if ((size(x, 2, kind=c_ptrdiff_t)) /= (size(btmp1_, 2, kind=c_ptrdiff_t))) then
            call quickr_set_error_msg("reassignment must preserve the shape of `x`")
            return
          end if
          x = btmp1_
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
        double* const x__,
        const R_len_t x__dim_1_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        if (x__dim_1_ != x__dim_2_)
          Rf_error("dim(x)[2] must equal dim(x)[1],"
                   " but are %0.f and %0.f",
                    (double)x__dim_2_, (double)x__dim_1_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, x__dim_1_, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        return x;
      }

# a deferred BLAS destination can change its shape

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n, k)), type(out = double(NA, NA)))
          out <- x
          out <- crossprod(x)
          sum(out)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        ! manifest end
      
      
        out = x
        block
          real(c_double), allocatable :: btmp1_(:, :)
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          allocate(btmp1_(x__dim_2_, x__dim_2_))
          if (int(x__dim_2_, kind=c_int) == 0_c_int .or. int(x__dim_1_, kind=c_int) == 0_c_int) then
            btmp1_ = 0.0_c_double
          else
      call dsyrk('U', 'T', int(x__dim_2_, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int),&
      & 0.0_c_double, btmp1_, int(x__dim_2_, kind=c_int))
          end if
          do btmp3_ = 1_c_int, int(x__dim_2_, kind=c_int) - 1_c_int
            do btmp2_ = btmp3_ + 1_c_int, int(x__dim_2_, kind=c_int)
              btmp1_(btmp2_, btmp3_) = btmp1_(btmp3_, btmp2_)
            end do
          end do
          out = btmp1_
        end block
        out_ = sum(out)
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
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_);
      
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
        
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          out___,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(1);
        return out_;
      }

# a known empty GEMM contraction fills a nonempty result

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(2, 0)), type(b = double(0, 3)))
          a %*% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a(2, 0)
        real(c_double), intent(in) :: b(0, 3)
        real(c_double), intent(out) :: out_(2, 3)
        ! manifest end
      
      
        out_ = 0.0_c_double
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
        double* const out___);
      
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
        
        if (a__dim_1_ != 2)
          Rf_error("dim(a)[1] must be 2, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 0)
          Rf_error("dim(a)[2] must be 0, not %0.f",
                    (double)a__dim_2_);
        if (b__dim_1_ != 0)
          Rf_error("dim(b)[1] must be 0, not %0.f",
                    (double)b__dim_1_);
        if (b__dim_2_ != 3)
          Rf_error("dim(b)[2] must be 3, not %0.f",
                    (double)b__dim_2_);
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
        
        fn(a__, b__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# a known empty GEMV contraction fills a nonempty result

    Code
      fn
    Output
      function(a, x) {
          declare(type(a = double(3, 0)), type(x = double(0)))
          a %*% x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a(3, 0)
        real(c_double), intent(in) :: x(0)
        real(c_double), intent(out) :: out_(3, 1)
        ! manifest end
      
      
        out_ = 0.0_c_double
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__,
        const double* const x__,
        double* const out___);
      
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
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (a__dim_1_ != 3)
          Rf_error("dim(a)[1] must be 3, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 0)
          Rf_error("dim(a)[2] must be 0, not %0.f",
                    (double)a__dim_2_);
        if (x__len_ != 0)
          Rf_error("length(x) must be 0, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (3) * (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = 1;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(a__, x__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# a known empty output omits the BLAS call

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(0, 3)), type(b = double(3, 2)))
          a %*% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a(0, 3)
        real(c_double), intent(in) :: b(3, 2)
        real(c_double), intent(out) :: out_(0, 2)
        ! manifest end
      
      
        out_ = 0.0_c_double
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
        double* const out___);
      
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
        
        if (a__dim_1_ != 0)
          Rf_error("dim(a)[1] must be 0, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 3)
          Rf_error("dim(a)[2] must be 3, not %0.f",
                    (double)a__dim_2_);
        if (b__dim_1_ != 3)
          Rf_error("dim(b)[1] must be 3, not %0.f",
                    (double)b__dim_1_);
        if (b__dim_2_ != 2)
          Rf_error("dim(b)[2] must be 2, not %0.f",
                    (double)b__dim_2_);
        const R_xlen_t out___len_ = (0) * (2);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 0;
          _dim[1] = 2;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(a__, b__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# TRSV uses RHS storage extents after rebinding

    Code
      fn
    Output
      function(a, b, m) {
          declare(
            type(a = double(2, 2)),
            type(b = double(m)),
            type(m = integer(1))
          )
          out <- numeric(2)
          m <- m + 1L
          out <- forwardsolve(a, b)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, m, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(2, 2)
        real(c_double), intent(in) :: b(m)
        integer(c_int), intent(in out) :: m
        real(c_double), intent(out) :: out(2)
        ! manifest end
      
      
        out = 0.0_c_double
        m = (m + 1_c_int)
        if (2 /= size(b, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("non-conformable arguments in triangular solve")
          return
        end if
        if ((2) /= (size(b, 1, kind=c_ptrdiff_t))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `out`")
          return
        end if
        out = b
        call dtrsv('L', 'N', 'N', int(2, kind=c_int), a, int(2, kind=c_int), out, 1_c_int)
      
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
        int* const m__,
        double* const out__,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        m = Rf_duplicate(m);
        SETCAR(_args, m);
        int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        if (a__dim_1_ != 2)
          Rf_error("dim(a)[1] must be 2, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 2)
          Rf_error("dim(a)[2] must be 2, not %0.f",
                    (double)a__dim_2_);
        const int _as_int_m = Rf_asInteger(m);
        if (_as_int_m != b__len_)
          Rf_error("length(b) must equal m,"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)_as_int_m);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const R_xlen_t out__len_ = 2;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          m__,
          out__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# TRSM uses the stored RHS width and checks its destination

    Code
      fn
    Output
      function(a, b, m, columns) {
          declare(
            type(a = double(2, 2)),
            type(b = double(2, m)),
            type(m = integer(1)),
            type(columns = integer(1))
          )
          out <- matrix(0, 2, columns)
          m <- m + 1L
          out <- backsolve(a, b)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, m, columns, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(2, 2)
        real(c_double), intent(in) :: b(2, m)
        integer(c_int), intent(in out) :: m
        integer(c_int), intent(in) :: columns
        real(c_double), intent(out) :: out(2, columns)
        ! manifest end
      
      
        out = 0.0_c_double
        m = (m + 1_c_int)
        if ((size(out, 2, kind=c_ptrdiff_t)) /= (size(b, 2, kind=c_ptrdiff_t))) then
          call quickr_set_error_msg("reassignment must preserve the shape of `out`")
          return
        end if
        out = b
      call dtrsm('L', 'U', 'N', 'N', int(2, kind=c_int), int(size(b, 2, kind=c_ptrdiff_t), kind=c_int), 1.0_c_double, a, int(2,&
      & kind=c_int), out, int(2, kind=c_int))
      
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
        int* const m__,
        const int* const columns__,
        double* const out__,
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
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        m = Rf_duplicate(m);
        SETCAR(_args, m);
        int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        // columns
        _args = CDR(_args);
        SEXP columns = CAR(_args);
        if (TYPEOF(columns) != INTSXP) {
          Rf_error("typeof(columns) must be 'integer', not '%s'", Rf_type2char(TYPEOF(columns)));
        }
        const int* const columns__ = INTEGER(columns);
        const R_xlen_t columns__len_ = Rf_xlength(columns);
        
        if (a__dim_1_ != 2)
          Rf_error("dim(a)[1] must be 2, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 2)
          Rf_error("dim(a)[2] must be 2, not %0.f",
                    (double)a__dim_2_);
        if (b__dim_1_ != 2)
          Rf_error("dim(b)[1] must be 2, not %0.f",
                    (double)b__dim_1_);
        const int _as_int_m = Rf_asInteger(m);
        if (_as_int_m != b__dim_2_)
          Rf_error("dim(b)[2] must equal m,"
                   " but are %0.f and %0.f",
                    (double)b__dim_2_, (double)_as_int_m);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        if (columns__len_ != 1)
          Rf_error("length(columns) must be 1, not %0.f",
                    (double)columns__len_);
        const int _as_int_columns = Rf_asInteger(columns);
        const R_xlen_t out__len_ = (2) * (_as_int_columns);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = _as_int_columns;
          Rf_dimgets(out, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          m__,
          columns__,
          out__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out;
      }

# a transposed triangular solve passes its operation flag to BLAS

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(4, 4)), type(b = double(4)))
          backsolve(a, b, transpose = TRUE)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a(4, 4)
        real(c_double), intent(in) :: b(4)
        real(c_double), intent(out) :: out_(4)
        ! manifest end
      
      
        out_ = b
        call dtrsv('U', 'T', 'N', int(4, kind=c_int), a, int(4, kind=c_int), out_, 1_c_int)
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
        double* const out___);
      
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
        
        if (a__dim_1_ != 4)
          Rf_error("dim(a)[1] must be 4, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 4)
          Rf_error("dim(a)[2] must be 4, not %0.f",
                    (double)a__dim_2_);
        if (b__len_ != 4)
          Rf_error("length(b) must be 4, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = 4;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(a__, b__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# triangular solve checks unproven square and RHS dimensions

    Code
      fn
    Output
      function(a, x) {
          declare(type(a = double(n, k)), type(x = double(NA)))
          forwardsolve(a, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, x, out_, a__dim_1_, a__dim_2_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(size(x, 1, kind=c_ptrdiff_t))
        ! manifest end
      
      
        if (size(a, 1, kind=c_ptrdiff_t) /= size(a, 2, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("triangular solve requires a square matrix")
          return
        end if
        if (size(a, 1, kind=c_ptrdiff_t) /= size(x, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("non-conformable arguments in triangular solve")
          return
        end if
        out_ = x
        call dtrsv('L', 'N', 'N', int(a__dim_1_, kind=c_int), a, int(a__dim_1_, kind=c_int), out_, 1_c_int)
      
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
        const double* const x__,
        double* const out___,
        const R_len_t a__dim_1_,
        const R_len_t a__dim_2_,
        const R_xlen_t x__len_,
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
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          x__,
          out___,
          a__dim_1_,
          a__dim_2_,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# GESV solves a vector with proven square dimensions

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n, n)), type(b = double(n)))
          solve(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_1_)
        real(c_double), intent(in) :: b(a__dim_1_)
        real(c_double), intent(out) :: out_(a__dim_1_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:, :)
          integer(c_int), allocatable :: btmp2_(:)
          integer(c_int) :: btmp3_
      
          allocate(btmp1_(a__dim_1_, a__dim_1_))
          btmp1_ = a
          out_ = b
          allocate(btmp2_(a__dim_1_))
      call dgesv(int(a__dim_1_, kind=c_int), int(1, kind=c_int), btmp1_, int(a__dim_1_, kind=c_int), btmp2_, out_, int(a__dim_1_,&
      & kind=c_int), btmp3_)
          if (btmp3_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
            return
          end if
          if (btmp3_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
            return
          end if
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
        
        if (a__dim_1_ != a__dim_2_)
          Rf_error("dim(a)[2] must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)a__dim_1_);
        if (a__dim_1_ != b__len_)
          Rf_error("length(b) must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__dim_1_);
        const R_xlen_t out___len_ = a__dim_1_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out___,
          a__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# GESV checks squareness before solving a matrix RHS

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n, k)), type(b = double(n, p)))
          solve(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, a__dim_2_, b__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
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
          integer(c_int), allocatable :: btmp2_(:)
          integer(c_int) :: btmp3_
      
          if (size(a, 1, kind=c_ptrdiff_t) /= size(a, 2, kind=c_ptrdiff_t)) then
            call quickr_set_error_msg("solve requires a square matrix")
            return
          end if
          allocate(btmp1_(a__dim_1_, a__dim_2_))
          btmp1_ = a
          out_ = b
          allocate(btmp2_(a__dim_1_))
      call dgesv(int(a__dim_1_, kind=c_int), int(b__dim_2_, kind=c_int), btmp1_, int(a__dim_1_, kind=c_int), btmp2_, out_,&
      & int(a__dim_1_, kind=c_int), btmp3_)
          if (btmp3_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
            return
          end if
          if (btmp3_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
            return
          end if
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

# GESV handles a scalar RHS for a one-by-one matrix

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(1, 1)), type(b = double(1)))
          solve(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(1, 1)
        real(c_double), intent(in) :: b
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_(1, 1)
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          btmp1_ = a
          out_ = b
          call dgesv(int(1, kind=c_int), int(1, kind=c_int), btmp1_, int(1, kind=c_int), btmp2_, out_, int(1, kind=c_int), btmp3_)
          if (btmp3_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
            return
          end if
          if (btmp3_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
            return
          end if
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
        
        if (a__dim_1_ != 1)
          Rf_error("dim(a)[1] must be 1, not %0.f",
                    (double)a__dim_1_);
        if (a__dim_2_ != 1)
          Rf_error("dim(a)[2] must be 1, not %0.f",
                    (double)a__dim_2_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# matrix inversion uses GETRF and GETRI workspace and info checks

    Code
      fn
    Output
      function(a) {
          declare(type(a = double(n, n)))
          solve(a)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, out_, a__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_1_)
        real(c_double), intent(out) :: out_(a__dim_1_, a__dim_1_)
        ! manifest end
      
      
        block
          integer(c_int), allocatable :: btmp1_(:)
          integer(c_int) :: btmp2_
          real(c_double), allocatable :: btmp3_(:)
      
          out_ = a
          allocate(btmp1_(a__dim_1_))
          allocate(btmp3_(a__dim_1_))
          call dgetrf(int(a__dim_1_, kind=c_int), int(a__dim_1_, kind=c_int), out_, int(a__dim_1_, kind=c_int), btmp1_, btmp2_)
          if (btmp2_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgetrf: system is exactly singular")
            return
          end if
          if (btmp2_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgetrf: illegal argument")
            return
          end if
          call dgetri(int(a__dim_1_, kind=c_int), out_, int(a__dim_1_, kind=c_int), btmp1_, btmp3_, int(a__dim_1_, kind=c_int), btmp2_)
          if (btmp2_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgetri: system is exactly singular")
            return
          end if
          if (btmp2_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dgetri: illegal argument")
            return
          end if
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
        double* const out___,
        const R_len_t a__dim_1_,
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
        
        if (a__dim_1_ != a__dim_2_)
          Rf_error("dim(a)[2] must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)a__dim_1_);
        const R_xlen_t out___len_ = (a__dim_1_) * (a__dim_1_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = a__dim_1_;
          _dim[1] = a__dim_1_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          out___,
          a__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# Cholesky factorization and inversion initialize both triangles

    Code
      fn
    Output
      function(a) {
          declare(type(a = double(n, n)))
          upper <- chol(a)
          chol2inv(upper)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, out_, a__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_1_)
        real(c_double), intent(out) :: out_(a__dim_1_, a__dim_1_)
      
        ! locals
        real(c_double), allocatable :: upper(:, :)
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          if (.not. allocated(upper)) allocate(upper(a__dim_1_, a__dim_1_))
          upper = a
          call dpotrf('U', int(a__dim_1_, kind=c_int), upper, int(a__dim_1_, kind=c_int), btmp1_)
          if (btmp1_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dpotrf: leading minor is not positive definite")
            return
          end if
          if (btmp1_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dpotrf: illegal argument")
            return
          end if
          do btmp2_ = 2_c_int, int(a__dim_1_, kind=c_int)
            do btmp3_ = 1_c_int, btmp2_ - 1_c_int
              upper(btmp2_, btmp3_) = 0.0_c_double
            end do
          end do
        end block
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          if (size(upper, 1, kind=c_ptrdiff_t) /= size(upper, 2, kind=c_ptrdiff_t)) then
            call quickr_set_error_msg("chol2inv requires a square matrix")
            return
          end if
          out_ = upper
          call dpotri('U', int(a__dim_1_, kind=c_int), out_, int(a__dim_1_, kind=c_int), btmp1_)
          if (btmp1_ > 0_c_int) then
            call quickr_set_error_msg("Lapack routine dpotri: matrix is not positive definite")
            return
          end if
          if (btmp1_ < 0_c_int) then
            call quickr_set_error_msg("Lapack routine dpotri: illegal argument")
            return
          end if
          do btmp3_ = 1_c_int, int(a__dim_1_, kind=c_int) - 1_c_int
            do btmp2_ = btmp3_ + 1_c_int, int(a__dim_1_, kind=c_int)
              out_(btmp2_, btmp3_) = out_(btmp3_, btmp2_)
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
        double* const out___,
        const R_len_t a__dim_1_,
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
        
        if (a__dim_1_ != a__dim_2_)
          Rf_error("dim(a)[2] must equal dim(a)[1],"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)a__dim_1_);
        const R_xlen_t out___len_ = (a__dim_1_) * (a__dim_1_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = a__dim_1_;
          _dim[1] = a__dim_1_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          out___,
          a__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# QR evaluates an effectful tolerance before checking RHS shape

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n, k)), type(b = double(m)))
          qr.solve(a, b, tol = runif(1))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__dim_1_, a__dim_2_, b__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: a__dim_1_
        integer(c_int), intent(in), value :: a__dim_2_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: a(a__dim_1_, a__dim_2_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: out_(a__dim_2_)
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        block
          real(c_double) :: btmp1_
          real(c_double), allocatable :: btmp2_(:, :)
          real(c_double), allocatable :: btmp3_(:, :)
          real(c_double), allocatable :: btmp4_(:)
          integer(c_int), allocatable :: btmp5_(:)
          real(c_double), allocatable :: btmp6_(:, :)
          integer(c_int) :: btmp7_
          integer(c_int) :: btmp8_
          real(c_double), allocatable :: btmp9_(:, :)
          integer(c_int) :: btmp10_
          integer(c_int) :: btmp11_
      
          btmp1_ = unif_rand()
          if (size(a, 1, kind=c_ptrdiff_t) /= size(b, kind=c_ptrdiff_t)) then
            call quickr_set_error_msg("non-conformable arguments in qr.solve")
            return
          end if
          allocate(btmp2_(a__dim_1_, a__dim_2_))
          btmp2_ = a
          allocate(btmp3_(a__dim_1_, 1))
          btmp3_ = 0.0_c_double
          btmp3_(1:a__dim_1_, 1) = b
          allocate(btmp4_(a__dim_2_))
          allocate(btmp5_(a__dim_2_))
          allocate(btmp6_(a__dim_2_, 2))
          do btmp8_ = 1_c_int, int(a__dim_2_, kind=c_int)
            btmp5_(btmp8_) = btmp8_
          end do
      call dqrdc2(btmp2_, int(a__dim_1_, kind=c_int), int(a__dim_1_, kind=c_int), int(a__dim_2_, kind=c_int), btmp1_, btmp7_, btmp4_,&
      & btmp5_, btmp6_)
          if (btmp7_ < int(min(a__dim_1_, a__dim_2_), kind=c_int)) then
            call quickr_set_error_msg("rank deficient matrix in qr.solve")
            return
          end if
          allocate(btmp9_(min(a__dim_1_, a__dim_2_), 1))
          btmp9_ = 0.0_c_double
          call dqrcf(btmp2_, int(a__dim_1_, kind=c_int), btmp7_, btmp4_, btmp3_, int(1, kind=c_int), btmp9_, btmp10_)
          if (btmp10_ /= 0_c_int) then
            call quickr_set_error_msg("exact singularity in 'qr.coef'")
            return
          end if
          out_ = 0.0_c_double
          do btmp11_ = 1_c_int, btmp7_
            out_(btmp5_(btmp11_)) = btmp9_(btmp11_, 1)
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const double* const a__,
        const double* const b__,
        double* const out___,
        const R_len_t a__dim_1_,
        const R_len_t a__dim_2_,
        const R_xlen_t b__len_,
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
        
        const R_xlen_t out___len_ = a__dim_2_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(
          a__,
          b__,
          out___,
          a__dim_1_,
          a__dim_2_,
          b__len_,
          quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

