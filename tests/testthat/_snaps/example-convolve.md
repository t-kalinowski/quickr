# convolve

    Code
      slow_convolve
    Output
      function(a, b) {
          declare(type(a = double(NA)))
          declare(type(b = double(NA)))
      
          ab <- double(length(a) + length(b) - 1)
          for (i in seq_along(a)) {
            for (j in seq_along(b)) {
              ab[i + j - 1] = ab[i + j - 1] + a[i] * b[j]
            }
          }
          ab
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_convolve(a, b, ab, a__len_, b__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: ab(((a__len_ + b__len_) - 1))
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        ! manifest end
      
      
      
        ab = 0
        do i = 1, size(a)
          do j = 1, size(b)
            ab(((i + j) - 1_c_int)) = (ab(((i + j) - 1_c_int)) + (a(i) * b(j)))
          end do
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_convolve(
        const double* const a__, 
        const double* const b__, 
        double* const ab__, 
        const R_xlen_t a__len_, 
        const R_xlen_t b__len_);
      
      SEXP slow_convolve_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        const R_xlen_t ab__len_ = ((a__len_ + b__len_) - 1);
        SEXP ab = PROTECT(Rf_allocVector(REALSXP, ab__len_));
        double* ab__ = REAL(ab);
        
        slow_convolve(
          a__,
          b__,
          ab__,
          a__len_,
          b__len_);
        
        UNPROTECT(1);
        return ab;
      }

