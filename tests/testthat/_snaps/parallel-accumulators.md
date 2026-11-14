# parallel loops retain independent indexed accumulation

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA, NA)))
          out <- integer(nrow(x))
          declare(parallel())
          for (i in seq_len(nrow(x))) {
            for (j in seq_len(ncol(x))) {
              out[i] <- out[i] + x[i, j]
            }
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x(x__dim_1_, x__dim_2_)
        integer(c_int), intent(out) :: out(x__dim_1_)
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if (x__dim_1_ < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = 0_c_int
      
        !$omp parallel do private(j)
        do i = 1, size(x, 1)
          do tmp1_ = 1, size(x, 2)
            j = tmp1_
            out(i) = (out(i) + x(i, j))
          end do
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            !$omp critical (quickr_error)
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
            !$omp end critical (quickr_error)
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
        int* const out__,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_,
        char* quickr_err_msg);
      
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
        
        const R_xlen_t out__len_ = x__dim_1_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# parallel sapply callbacks can accumulate in local variables

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA)))
          declare(parallel())
          out <- sapply(x, function(value) {
            total <- 0L
            for (j in seq_len(3L)) {
              total <- total + value
            }
            total
          })
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
      
        !$omp parallel do
        do tmp1_ = 1_c_int, size(x)
          call closure1_(x(tmp1_), out(tmp1_))
      
        end do
        !$omp end parallel do
      
        contains
          subroutine closure1_(value, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: value
            integer(c_int), intent(out) :: res
            integer(c_int) :: total
            integer(c_int) :: j
            integer(c_int) :: ctmp1_
      
            total = 0_c_int
            do ctmp1_ = 1, 3_c_int
              j = ctmp1_
              total = (total + value)
            end do
            res = total
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

