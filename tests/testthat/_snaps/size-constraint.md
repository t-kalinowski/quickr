# size constraint

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n + 1)))
          a = sum(b)
          a
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in out) :: a(a__len_)
        real(c_double), intent(in) :: b((a__len_ + 1))
        ! manifest end
      
      
        a = sum(b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const a__, 
        const double* const b__, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        a = Rf_duplicate(a);
        SETCAR(_args, a);
        double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        {
          const R_xlen_t expected = (a__len_ + 1);
          if (b__len_ != expected)
            Rf_error("length(b) must equal (length(a) + 1),"
                     " but are %0.f and %0.f",
                      (double)b__len_, (double)expected);
        }
        
        fn(a__, b__, a__len_);
        
        return a;
      }

