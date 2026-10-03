# bitwAnd, bitwOr, and bitwXor match R on integer vectors

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = integer(NA)), type(b = integer(NA)))
          bitwAnd(a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_, b__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: a(a__len_)
        integer(c_int), intent(in) :: b(b__len_)
        integer(c_int), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        if (size(a, kind=c_ptrdiff_t) == 0 .or. size(a, kind=c_ptrdiff_t) /= size(b, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        out_ = iand(a, b)
      
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
        const int* const a__, 
        const int* const b__, 
        int* const out___, 
        const R_xlen_t a__len_, 
        const R_xlen_t b__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != INTSXP) {
          Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = INTEGER(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != INTSXP) {
          Rf_error("typeof(b) must be 'integer', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const int* const b__ = INTEGER(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          b__,
          out___,
          a__len_,
          b__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# bitwNot matches R

    Code
      fn
    Output
      function(a) {
          declare(type(a = integer(NA)))
          bitwNot(a)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, out_, a__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        integer(c_int), intent(in) :: a(a__len_)
        integer(c_int), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = not(a)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const a__, 
        int* const out___, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != INTSXP) {
          Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = INTEGER(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(a__, out___, a__len_);
        
        UNPROTECT(1);
        return out_;
      }

# bitwShiftL and bitwShiftR match R across shift counts

    Code
      fn
    Output
      function(a, n) {
          declare(type(a = integer(NA)), type(n = integer(NA)))
          bitwShiftL(a, n)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, n, out_, a__len_, n__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: n__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: a(a__len_)
        integer(c_int), intent(in) :: n(n__len_)
        integer(c_int), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        if (size(a, kind=c_ptrdiff_t) == 0 .or. size(a, kind=c_ptrdiff_t) /= size(n, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
          return
        end if
        out_ = merge(shiftl(a, min(max(n, 0_c_int), 31_c_int)), (-huge(0_c_int) - 1_c_int), (n >= 0_c_int .and. n <= 31_c_int))
      
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
        const int* const a__, 
        const int* const n__, 
        int* const out___, 
        const R_xlen_t a__len_, 
        const R_xlen_t n__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != INTSXP) {
          Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const int* const a__ = INTEGER(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          a__,
          n__,
          out___,
          a__len_,
          n__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

