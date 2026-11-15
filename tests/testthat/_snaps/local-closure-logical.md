# zero-argument local closures can return logical constants

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          yes <- function() TRUE
          no <- function() FALSE
          positive <- yes()
          negative <- no()
          c(positive, negative, x > 0L)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(out) :: out_(3) ! logical
      
        ! locals
        logical :: positive ! logical
        logical :: negative ! logical
        ! manifest end
      
      
      
      
        call yes(positive)
      
        call no(negative)
      
        out_ = [ positive, negative, (x > 0_c_int) ]
      
        contains
          subroutine yes(res)
            implicit none
      
            logical, intent(out) :: res ! logical
      
      
            res = .true.
          end subroutine
          subroutine no(res)
            implicit none
      
            logical, intent(out) :: res ! logical
      
      
            res = .false.
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const x__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# logical-only closures work in selected and skipped lazy branches

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = logical(1)))
          yes <- function() TRUE
          no <- function() FALSE
          c(flag && yes(), flag && no(), flag || yes(), flag || no())
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: flag ! logical
        integer(c_int), intent(out) :: out_(4) ! logical
      
        ! locals
        logical :: tmp1_ ! logical
        logical :: tmp2_ ! logical
        logical :: tmp3_ ! logical
        logical :: tmp4_ ! logical
        ! manifest end
      
      
      
      
        tmp1_ = (flag/=0)
        if (tmp1_) then
          block
            logical :: btmp1_ ! logical
      
            call yes(btmp1_)
            tmp1_ = btmp1_
          end block
        end if
        tmp2_ = (flag/=0)
        if (tmp2_) then
          block
            logical :: btmp1_ ! logical
      
            call no(btmp1_)
            tmp2_ = btmp1_
          end block
        end if
        tmp3_ = (flag/=0)
        if (.not. tmp3_) then
          block
            logical :: btmp1_ ! logical
      
            call yes(btmp1_)
            tmp3_ = btmp1_
          end block
        end if
        tmp4_ = (flag/=0)
        if (.not. tmp4_) then
          block
            logical :: btmp1_ ! logical
      
            call no(btmp1_)
            tmp4_ = btmp1_
          end block
        end if
        out_ = [ tmp1_, tmp2_, tmp3_, tmp4_ ]
      
        contains
          subroutine yes(res)
            implicit none
      
            logical, intent(out) :: res ! logical
      
      
            res = .true.
          end subroutine
          subroutine no(res)
            implicit none
      
            logical, intent(out) :: res ! logical
      
      
            res = .false.
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const flag__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = 4;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(flag__, out___);
        
        UNPROTECT(1);
        return out_;
      }

