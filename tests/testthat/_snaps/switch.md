# switch() as a statement dispatches on position

    Code
      fn
    Output
      function(ops) {
          declare(type(ops = integer(NA)))
          acc <- 0L
          bump <- function() {
            acc <<- acc + 100L
            0L
          }
          reset <- function() {
            acc <<- 0L
          }
          for (i in seq_along(ops)) {
            switch(
              ops[i] + 1L,
              acc <- acc + 1L,
              {
                acc <- acc * 2L
              },
              bump(),
              reset(),
              next,
              break
            )
            acc <- acc + 1000L
          }
          acc
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(ops, acc, ops__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: ops__len_
      
        ! args
        integer(c_int), intent(in) :: ops(ops__len_)
        integer(c_int), intent(out) :: acc
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
        acc = 0_c_int
      
      
        do i = 1, size(ops)
          select case ((ops(i) + 1_c_int))
          case (1)
            acc = (acc + 1_c_int)
          case (2)
            acc = (acc * 2_c_int)
          case (3)
            block
              integer(c_int) :: btmp1_
      
              call bump(btmp1_)
            end block
          case (4)
            call reset()
          case (5)
            cycle
          case (6)
            exit
          end select
          acc = (acc + 1000_c_int)
        end do
      
        contains
          subroutine bump(res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(out) :: res
      
            acc = (acc + 100_c_int)
            res = 0_c_int
          end subroutine
          subroutine reset()
            use iso_c_binding, only: c_int
            implicit none
      
      
      
      
            acc = 0_c_int
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const ops__, 
        int* const acc__, 
        const R_xlen_t ops__len_);
      
      SEXP fn_(SEXP _args) {
        // ops
        _args = CDR(_args);
        SEXP ops = CAR(_args);
        if (TYPEOF(ops) != INTSXP) {
          Rf_error("typeof(ops) must be 'integer', not '%s'", Rf_type2char(TYPEOF(ops)));
        }
        const int* const ops__ = INTEGER(ops);
        const R_xlen_t ops__len_ = Rf_xlength(ops);
        
        const R_xlen_t acc__len_ = (1);
        SEXP acc = PROTECT(Rf_allocVector(INTSXP, acc__len_));
        int* acc__ = INTEGER(acc);
        
        fn(ops__, acc__, ops__len_);
        
        UNPROTECT(1);
        return acc;
      }

# switch() as a value

    Code
      fn
    Output
      function(op) {
          declare(type(op = integer(1)))
          n <- switch(op + 1L, 4L, 8L, 12L)
          n * 2L
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(op, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: op
        integer(c_int), intent(out) :: out_
      
        ! locals
        integer(c_int) :: n
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          select case ((op + 1_c_int))
          case (1)
            btmp1_ = 4_c_int
          case (2)
            btmp1_ = 8_c_int
          case (3)
            btmp1_ = 12_c_int
          case default
            call quickr_set_error_msg("switch() index is out of range; R would return NULL, which quickr cannot return")
            return
          end select
          n = btmp1_
        end block
        out_ = (n * 2_c_int)
      
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
        const int* const op__, 
        int* const out___, 
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // op
        _args = CDR(_args);
        SEXP op = CAR(_args);
        if (TYPEOF(op) != INTSXP) {
          Rf_error("typeof(op) must be 'integer', not '%s'", Rf_type2char(TYPEOF(op)));
        }
        const int* const op__ = INTEGER(op);
        const R_xlen_t op__len_ = Rf_xlength(op);
        
        if (op__len_ != 1)
          Rf_error("length(op) must be 1, not %0.f",
                    (double)op__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(op__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

