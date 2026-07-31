# ifelse promotes branches and shapes like test

    Code
      fn
    Output
      function(c, a) {
          declare(type(c = logical(n)), type(a = double(n)))
          ifelse(c, 1L, a)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(c, a, out_, c__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: c__len_
      
        ! args
        integer(c_int), intent(in) :: c(c__len_) ! logical
        real(c_double), intent(in) :: a(c__len_)
        real(c_double), intent(out) :: out_(c__len_)
        ! manifest end
      
      
        out_ = merge(real(1_c_int, kind=c_double), a, (c/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const c__, 
        const double* const a__, 
        double* const out___, 
        const R_xlen_t c__len_);
      
      SEXP fn_(SEXP _args) {
        // c
        _args = CDR(_args);
        SEXP c = CAR(_args);
        if (TYPEOF(c) != LGLSXP) {
          Rf_error("typeof(c) must be 'logical', not '%s'", Rf_type2char(TYPEOF(c)));
        }
        const int* const c__ = LOGICAL(c);
        const R_xlen_t c__len_ = Rf_xlength(c);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        if (c__len_ != a__len_)
          Rf_error("length(a) must equal length(c),"
                   " but are %0.f and %0.f",
                    (double)a__len_, (double)c__len_);
        const R_xlen_t out___len_ = c__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          c__,
          a__,
          out___,
          c__len_);
        
        UNPROTECT(1);
        return out_;
      }

# ifelse guards unknown branch lengths at runtime

    Code
      fn
    Output
      function(c, a, b) {
          declare(type(c = logical(NA)), type(a = double(NA)), type(b = double(NA)))
          ifelse(c, a, b)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(c, a, b, out_, a__len_, b__len_, c__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: c__len_
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: c(c__len_) ! logical
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: out_(c__len_)
        ! manifest end
      
      
        if (size(a, 1) /= size((c/=0), 1)) then
      call quickr_set_error_msg("ifelse() `yes` and `no` must be scalars or match the shape of `test`; R-style recycling is not&
      & supported")
          return
        end if
        if (size(b, 1) /= size((c/=0), 1)) then
      call quickr_set_error_msg("ifelse() `yes` and `no` must be scalars or match the shape of `test`; R-style recycling is not&
      & supported")
          return
        end if
        out_ = merge(a, b, (c/=0))
      
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
        const int* const c__, 
        const double* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_xlen_t a__len_, 
        const R_xlen_t b__len_, 
        const R_xlen_t c__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // c
        _args = CDR(_args);
        SEXP c = CAR(_args);
        if (TYPEOF(c) != LGLSXP) {
          Rf_error("typeof(c) must be 'logical', not '%s'", Rf_type2char(TYPEOF(c)));
        }
        const int* const c__ = LOGICAL(c);
        const R_xlen_t c__len_ = Rf_xlength(c);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        const R_xlen_t out___len_ = c__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          c__,
          a__,
          b__,
          out___,
          a__len_,
          b__len_,
          c__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

