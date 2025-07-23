# arithmetic expressions in dimensions compile

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          x <- double(n * 2L)
          y <- double(n - 1L)
          length(x) + length(y)
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
        integer(c_int), intent(out) :: out_
      
        ! locals
        real(c_double) :: x((n * 2))
        real(c_double) :: y((n - 1))
        ! manifest end
      
      
        x = 0
        y = 0
        out_ = (size(x) + size(y))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", R_typeToChar(n));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# integer division and modulus in dimensions compile

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          out <- double(n %/% 2L + n %% 2L)
          length(out)
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
        integer(c_int), intent(out) :: out_
      
        ! locals
        real(c_double) :: out((int(n) / int(2) + mod(int(n), int(2))))
        ! manifest end
      
      
        out = 0
        out_ = size(out)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", R_typeToChar(n));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# matrix dimension expressions compile

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)))
          out <- matrix(1, n + 1L, n %/% 2L + 1L)
          dim(out)
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
        integer(c_int), intent(out) :: out_(2)
      
        ! locals
        real(c_double) :: out((n + 1), (int(n) / int(2) + 1))
        ! manifest end
      
      
        out = 1.0_c_double
        out_ = shape(out)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const n__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", R_typeToChar(n));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

