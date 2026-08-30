# drop() translation snapshots

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(1L, 3L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(1, 3)
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
        out_ = A(1, :)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const A__, double* const out___);
      
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
        
        if (A__dim_1_ != 1)
          Rf_error("dim(A)[1] must be 1, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 3)
          Rf_error("dim(A)[2] must be 3, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(A__, out___);
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(3L, 1L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(3, 1)
        real(c_double), intent(out) :: out_(3)
        ! manifest end
      
      
        out_ = A(:, 1)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const A__, double* const out___);
      
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
        
        if (A__dim_1_ != 3)
          Rf_error("dim(A)[1] must be 3, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 1)
          Rf_error("dim(A)[2] must be 1, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(A__, out___);
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(1L, NA)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_, A__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(1, A__dim_2_)
        real(c_double), intent(out) :: out_(A__dim_2_)
        ! manifest end
      
      
        if (size(A, 2) == 1) then
          call quickr_set_error_msg("drop() requires singleton dimensions to be known at compile time")
          return
        end if
        out_ = A(1, :)
      
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
        double* const out___,
        const R_len_t A__dim_2_,
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
        
        if (A__dim_1_ != 1)
          Rf_error("dim(A)[1] must be 1, not %0.f",
                    (double)A__dim_1_);
        const R_xlen_t out___len_ = A__dim_2_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          out___,
          A__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A) {
          declare(type(A = double(NA, 1L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_, A__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(A__dim_1_, 1)
        real(c_double), intent(out) :: out_(A__dim_1_)
        ! manifest end
      
      
        if (size(A, 1) == 1) then
          call quickr_set_error_msg("drop() requires singleton dimensions to be known at compile time")
          return
        end if
        out_ = A(:, 1)
      
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
        
        if (A__dim_2_ != 1)
          Rf_error("dim(A)[2] must be 1, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = A__dim_1_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          out___,
          A__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A, n) {
          declare(type(n = integer(1)))
          declare(type(A = double(1L, n)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(1, n)
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(n)
        ! manifest end
      
      
      
        if (size(A, 2) == 1) then
          call quickr_set_error_msg("drop() requires singleton dimensions to be known at compile time")
          return
        end if
        out_ = A(1, :)
      
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
        const int* const n__,
        double* const out___,
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
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (A__dim_1_ != 1)
          Rf_error("dim(A)[1] must be 1, not %0.f",
                    (double)A__dim_1_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != A__dim_2_)
          Rf_error("dim(A)[2] must equal n,"
                   " but are %0.f and %0.f",
                    (double)A__dim_2_, (double)_as_int_n);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          n__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(A, n) {
          declare(type(n = integer(1)))
          declare(type(A = double(n, 1L)))
          drop(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(n, 1)
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(n)
        ! manifest end
      
      
      
        if (size(A, 1) == 1) then
          call quickr_set_error_msg("drop() requires singleton dimensions to be known at compile time")
          return
        end if
        out_ = A(:, 1)
      
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
        const int* const n__,
        double* const out___,
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
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != A__dim_1_)
          Rf_error("dim(A)[1] must equal n,"
                   " but are %0.f and %0.f",
                    (double)A__dim_1_, (double)_as_int_n);
        if (A__dim_2_ != 1)
          Rf_error("dim(A)[2] must be 1, not %0.f",
                    (double)A__dim_2_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          n__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

