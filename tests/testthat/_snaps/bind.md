# cbind/rbind reject rank > 2 inputs with clear errors

    Code
      capture_bind_error(r2f(bad_cbind))
    Output
      cbind() only supports rank 0-2 inputs
    Code
      capture_bind_error(r2f(bad_rbind))
    Output
      rbind() only supports rank 0-2 inputs

# cbind validates independently supplied dimension values

    Code
      fn
    Output
      function(x, y, n, m) {
          declare(
            type(n = integer(1)),
            type(m = integer(1)),
            type(x = double(n)),
            type(y = double(m))
          )
          cbind(x, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, n, m, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(n)
        real(c_double), intent(in) :: y(m)
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(out) :: out_(n, 2)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
        out_ = reshape([reshape(x, [int(n), 1]), reshape(y, [int(n), 1])], [int(n), int(2)])
      
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
        const int* const n__,
        const int* const m__,
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != x__len_)
          Rf_error("length(x) must equal n,"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)_as_int_n);
        const int _as_int_m = Rf_asInteger(m);
        if (_as_int_m != y__len_)
          Rf_error("length(y) must equal m,"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)_as_int_m);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const R_xlen_t out___len_ = (_as_int_n) * (2);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = _as_int_n;
          _dim[1] = 2;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          n__,
          m__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# cbind and rbind check independently inferred dimensions at runtime

    Code
      fn
    Output
      function(A, B) {
          declare(type(A = double(NA, 2)), type(B = double(NA, 1)))
          cbind(A, B)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, B, out_, A__dim_1_, B__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_1_
        integer(c_int), intent(in), value :: B__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(A__dim_1_, 2)
        real(c_double), intent(in) :: B(B__dim_1_, 1)
        real(c_double), intent(out) :: out_(A__dim_1_, 3)
        ! manifest end
      
      
        if (size(A, 1, kind=c_ptrdiff_t) /= size(B, 1, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
        out_ = reshape([A, B], [int(A__dim_1_), int(3)])
      
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
        const double* const A__,
        const double* const B__,
        double* const out___,
        const R_len_t A__dim_1_,
        const R_len_t B__dim_1_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // B
        _args = CDR(_args);
        SEXP B = CAR(_args);
        if (TYPEOF(B) != REALSXP) {
          Rf_error("typeof(B) must be 'double', not '%s'", Rf_type2char(TYPEOF(B)));
        }
        const double* const B__ = REAL(B);
        const int* const B__dim_ = ({
        SEXP dim_ = Rf_getAttrib(B, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "B must be a 2D-array, but length(dim(B)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int B__dim_1_ = B__dim_[0];
        const int B__dim_2_ = B__dim_[1];
        
        if (A__dim_2_ != 2)
          Rf_error("dim(A)[2] must be 2, not %0.f",
                    (double)A__dim_2_);
        if (B__dim_2_ != 1)
          Rf_error("dim(B)[2] must be 1, not %0.f",
                    (double)B__dim_2_);
        const R_xlen_t out___len_ = (A__dim_1_) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = A__dim_1_;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          B__,
          out___,
          A__dim_1_,
          B__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

---

    Code
      fn
    Output
      function(A, B) {
          declare(type(A = double(2, NA)), type(B = double(1, NA)))
          rbind(A, B)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, B, out_, A__dim_2_, B__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_2_
        integer(c_int), intent(in), value :: B__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(2, A__dim_2_)
        real(c_double), intent(in) :: B(1, B__dim_2_)
        real(c_double), intent(out) :: out_(3, A__dim_2_)
        ! manifest end
      
      
        if (size(A, 2, kind=c_ptrdiff_t) /= size(B, 2, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("rbind() requires inputs with matching dimensions")
          return
        end if
        out_ = transpose(reshape([transpose(A), transpose(B)], [int(A__dim_2_), int(3)]))
      
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
        const double* const A__,
        const double* const B__,
        double* const out___,
        const R_len_t A__dim_2_,
        const R_len_t B__dim_2_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // B
        _args = CDR(_args);
        SEXP B = CAR(_args);
        if (TYPEOF(B) != REALSXP) {
          Rf_error("typeof(B) must be 'double', not '%s'", Rf_type2char(TYPEOF(B)));
        }
        const double* const B__ = REAL(B);
        const int* const B__dim_ = ({
        SEXP dim_ = Rf_getAttrib(B, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "B must be a 2D-array, but length(dim(B)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int B__dim_1_ = B__dim_[0];
        const int B__dim_2_ = B__dim_[1];
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (B__dim_1_ != 1)
          Rf_error("dim(B)[1] must be 1, not %0.f",
                    (double)B__dim_1_);
        const R_xlen_t out___len_ = (3) * (A__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = A__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          B__,
          out___,
          A__dim_2_,
          B__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# binding ignores NULL alongside nonempty dynamic inputs

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          cbind(NULL, x, y, NULL)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
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
        real(c_double), intent(out) :: out_(x__len_, 2)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
        out_ = reshape([reshape(x, [int(x__len_), 1]), reshape(y, [int(x__len_), 1])], [int(x__len_), int(2)])
      
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
        
        const R_xlen_t out___len_ = (x__len_) * (2);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = 2;
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

---

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          rbind(NULL, x, y, NULL)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
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
        real(c_double), intent(out) :: out_(2, x__len_)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("rbind() requires inputs with matching dimensions")
          return
        end if
      out_ = transpose(reshape([transpose(reshape(x, [1, int(x__len_)])), transpose(reshape(y, [1, int(x__len_)]))], [int(x__len_),&
      & int(2)]))
      
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
        
        const R_xlen_t out___len_ = (2) * (x__len_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = x__len_;
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

# binding accepts distinct symbolic dimensions and scalar inputs

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          cbind(x, y, 1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
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
        real(c_double), intent(out) :: out_(x__len_, 3)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
      out_ = reshape([reshape(x, [int(x__len_), 1]), reshape(y, [int(x__len_), 1]), reshape(spread(1.0_c_double, 1, int(x__len_)),&
      & [int(x__len_), 1])], [int(x__len_), int(3)])
      
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
        
        const R_xlen_t out___len_ = (x__len_) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = 3;
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

---

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          rbind(1, x, y)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
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
        real(c_double), intent(out) :: out_(3, x__len_)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("rbind() requires inputs with matching dimensions")
          return
        end if
      out_ = transpose(reshape([transpose(reshape(spread(1.0_c_double, 1, int(x__len_)), [1, int(x__len_)])), transpose(reshape(x, [1,&
      & int(x__len_)])), transpose(reshape(y, [1, int(x__len_)]))], [int(x__len_), int(3)]))
      
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
        
        const R_xlen_t out___len_ = (3) * (x__len_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = x__len_;
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

# binding checks a dynamic common dimension against a known one

    Code
      fn
    Output
      function(A, B) {
          declare(type(A = double(NA, 2)), type(B = double(3, 1)))
          cbind(A, B)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, B, out_, A__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(A__dim_1_, 2)
        real(c_double), intent(in) :: B(3, 1)
        real(c_double), intent(out) :: out_(A__dim_1_, 3)
        ! manifest end
      
      
        if (size(A, 1, kind=c_ptrdiff_t) /= 3) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
        out_ = reshape([A, B], [int(A__dim_1_), int(3)])
      
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
        const double* const A__,
        const double* const B__,
        double* const out___,
        const R_len_t A__dim_1_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // B
        _args = CDR(_args);
        SEXP B = CAR(_args);
        if (TYPEOF(B) != REALSXP) {
          Rf_error("typeof(B) must be 'double', not '%s'", Rf_type2char(TYPEOF(B)));
        }
        const double* const B__ = REAL(B);
        const int* const B__dim_ = ({
        SEXP dim_ = Rf_getAttrib(B, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "B must be a 2D-array, but length(dim(B)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int B__dim_1_ = B__dim_[0];
        const int B__dim_2_ = B__dim_[1];
        
        if (A__dim_2_ != 2)
          Rf_error("dim(A)[2] must be 2, not %0.f",
                    (double)A__dim_2_);
        if (B__dim_1_ != 3)
          Rf_error("dim(B)[1] must be 3, not %0.f",
                    (double)B__dim_1_);
        if (B__dim_2_ != 1)
          Rf_error("dim(B)[2] must be 1, not %0.f",
                    (double)B__dim_2_);
        const R_xlen_t out___len_ = (A__dim_1_) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = A__dim_1_;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          B__,
          out___,
          A__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

