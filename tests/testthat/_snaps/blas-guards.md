# guard text is pinned (one snapshot per mechanism)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Unverifiable BLAS dims emit one size guard before the call.
    Code
      fn
    Output
      function(m, x) {
          declare(type(m = double(3, 3)), type(x = double(NA)))
          m %*% x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m, x, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: m(3, 3)
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(3, 1)
        ! manifest end
      
      
        if (3 /= size(x)) then
          call quickr_set_error_msg("non-conformable arguments in %*%")
          return
        end if
      call dgemv('N', int(3, kind=c_int), int(3, kind=c_int), 1.0_c_double, m, int(3, kind=c_int), x, 1_c_int, 0.0_c_double, out_,&
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
        const double* const m__, 
        const double* const x__, 
        double* const out___, 
        const R_xlen_t x__len_, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
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
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (m__dim_1_ != 3)
          Rf_error("dim(m)[1] must be 3, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 3)
          Rf_error("dim(m)[2] must be 3, not %0.f",
                    (double)m__dim_2_);
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
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          m__,
          x__,
          out___,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

