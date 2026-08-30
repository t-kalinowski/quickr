# translation snapshots: hoist+block inside contains (sapply closure)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: The closure body indexes an array expression, which forces a hoisted temporary and a Fortran block inside the internal procedure.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- double(length(x))
          out <- sapply(seq_along(out), function(i) (x + 1.0)[i])
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        if (x__len_ < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = 0.0_c_double
        do tmp1_ = 1_c_int, x__len_
          call closure1_(tmp1_, out(tmp1_))
          if (quickr_err_msg(1) /= c_null_char) return
        end do
      
      
        contains
          subroutine closure1_(i, res)
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
            integer(c_int), intent(in) :: i
            real(c_double), intent(out) :: res
      
      
            block
              real(c_double), allocatable :: btmp1_(:)
      
              allocate(btmp1_(x__len_))
              if (size(x, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
          call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
          & supported")
                return
              end if
              btmp1_ = ((x + 1.0_c_double))
              res = btmp1_(i)
            end block
          end subroutine
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
        double* const out__, 
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
        
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out__,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# translation snapshots: hoist+block inside contains (statement closure + <<-)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Statement local closure lowered under contains; RHS indexing forces hoisting + a block inside the internal procedure; LHS uses [<<- to mutate the host variable.
    Code
      fn
    Output
      function(nx, ny) {
          declare(type(nx = integer(1)), type(ny = integer(1)))
          temp <- matrix(0, nx, ny)
      
          bc <- function() {
            temp[1, 1] <<- (temp + 1.0)[1, 1]
            temp[nx, ny] <<- (temp + 2.0)[nx, ny]
            NULL
          }
      
          bc()
          temp
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(nx, ny, temp, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: nx
        integer(c_int), intent(in) :: ny
        real(c_double), intent(out) :: temp(nx, ny)
        ! manifest end
      
      
        temp = 0.0_c_double
      
        call bc()
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine bc()
            use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
            implicit none
      
      
      
            block
              real(c_double), allocatable :: btmp1_(:, :)
      
              allocate(btmp1_(nx, ny))
              if (size(temp, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
                call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
                return
              end if
              if (size(temp, 2, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
                call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
                return
              end if
              btmp1_ = ((temp + 1.0_c_double))
              temp(1_c_int, 1_c_int) = btmp1_(1_c_int, 1_c_int)
            end block
            block
              real(c_double), allocatable :: btmp1_(:, :)
      
              allocate(btmp1_(nx, ny))
              if (size(temp, 1, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
                call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
                return
              end if
              if (size(temp, 2, kind=c_ptrdiff_t) == 0_c_ptrdiff_t) then
                call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
                return
              end if
              btmp1_ = ((temp + 2.0_c_double))
              temp(nx, ny) = btmp1_(nx, ny)
            end block
          end subroutine
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
        const int* const nx__, 
        const int* const ny__, 
        double* const temp__, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // nx
        _args = CDR(_args);
        SEXP nx = CAR(_args);
        if (TYPEOF(nx) != INTSXP) {
          Rf_error("typeof(nx) must be 'integer', not '%s'", Rf_type2char(TYPEOF(nx)));
        }
        const int* const nx__ = INTEGER(nx);
        const R_xlen_t nx__len_ = Rf_xlength(nx);
        
        // ny
        _args = CDR(_args);
        SEXP ny = CAR(_args);
        if (TYPEOF(ny) != INTSXP) {
          Rf_error("typeof(ny) must be 'integer', not '%s'", Rf_type2char(TYPEOF(ny)));
        }
        const int* const ny__ = INTEGER(ny);
        const R_xlen_t ny__len_ = Rf_xlength(ny);
        
        if (nx__len_ != 1)
          Rf_error("length(nx) must be 1, not %0.f",
                    (double)nx__len_);
        if (ny__len_ != 1)
          Rf_error("length(ny) must be 1, not %0.f",
                    (double)ny__len_);
        const int _as_int_nx = Rf_asInteger(nx);
        const int _as_int_ny = Rf_asInteger(ny);
        const R_xlen_t temp__len_ = (_as_int_nx) * (_as_int_ny);
        SEXP temp = PROTECT(Rf_allocVector(REALSXP, temp__len_));
        double* temp__ = REAL(temp);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = _as_int_nx;
          _dim[1] = _as_int_ny;
          Rf_dimgets(temp, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          nx__,
          ny__,
          temp__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return temp;
      }

