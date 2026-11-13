# shared-shape ifelse writes one inline selection

    Code
      fn
    Output
      function(test, yes, no) {
          declare(
            type(test = logical(n)),
            type(yes = double(n)),
            type(no = double(n))
          )
          out <- ifelse(test, yes + 1, no)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, yes, no, out, test__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: test__len_
      
        ! args
        integer(c_int), intent(in) :: test(test__len_) ! logical
        real(c_double), intent(in) :: yes(test__len_)
        real(c_double), intent(in) :: no(test__len_)
        real(c_double), intent(out) :: out(test__len_)
        ! manifest end
      
      
        block
      
          out = merge((yes + 1.0_c_double), no, (test/=0))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const test__,
        const double* const yes__,
        const double* const no__,
        double* const out__,
        const R_xlen_t test__len_);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const R_xlen_t test__len_ = Rf_xlength(test);
        
        // yes
        _args = CDR(_args);
        SEXP yes = CAR(_args);
        if (TYPEOF(yes) != REALSXP) {
          Rf_error("typeof(yes) must be 'double', not '%s'", Rf_type2char(TYPEOF(yes)));
        }
        const double* const yes__ = REAL(yes);
        const R_xlen_t yes__len_ = Rf_xlength(yes);
        
        // no
        _args = CDR(_args);
        SEXP no = CAR(_args);
        if (TYPEOF(no) != REALSXP) {
          Rf_error("typeof(no) must be 'double', not '%s'", Rf_type2char(TYPEOF(no)));
        }
        const double* const no__ = REAL(no);
        const R_xlen_t no__len_ = Rf_xlength(no);
        
        if (test__len_ != yes__len_)
          Rf_error("length(yes) must equal length(test),"
                   " but are %0.f and %0.f",
                    (double)yes__len_, (double)test__len_);
        if (test__len_ != no__len_)
          Rf_error("length(no) must equal length(test),"
                   " but are %0.f and %0.f",
                    (double)no__len_, (double)test__len_);
        const R_xlen_t out__len_ = test__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          test__,
          yes__,
          no__,
          out__,
          test__len_);
        
        UNPROTECT(1);
        return out;
      }

# matrix ifelse promotes branches without changing result shape

    Code
      fn
    Output
      function(test, yes) {
          declare(type(test = logical(n, m)), type(yes = integer(n, m)))
          ifelse(test, yes, 0.5)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, yes, out_, test__dim_1_, test__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: test__dim_1_
        integer(c_int), intent(in), value :: test__dim_2_
      
        ! args
        integer(c_int), intent(in) :: test(test__dim_1_, test__dim_2_) ! logical
        integer(c_int), intent(in) :: yes(test__dim_1_, test__dim_2_)
        real(c_double), intent(out) :: out_(test__dim_1_, test__dim_2_)
        ! manifest end
      
      
        block
      
          out_ = merge(real(yes, kind=c_double), 0.5_c_double, (test/=0))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const test__,
        const int* const yes__,
        double* const out___,
        const R_len_t test__dim_1_,
        const R_len_t test__dim_2_);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const int* const test__dim_ = ({
        SEXP dim_ = Rf_getAttrib(test, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "test must be a 2D-array, but length(dim(test)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int test__dim_1_ = test__dim_[0];
        const int test__dim_2_ = test__dim_[1];
        
        // yes
        _args = CDR(_args);
        SEXP yes = CAR(_args);
        if (TYPEOF(yes) != INTSXP) {
          Rf_error("typeof(yes) must be 'integer', not '%s'", Rf_type2char(TYPEOF(yes)));
        }
        const int* const yes__ = INTEGER(yes);
        const int* const yes__dim_ = ({
        SEXP dim_ = Rf_getAttrib(yes, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "yes must be a 2D-array, but length(dim(yes)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int yes__dim_1_ = yes__dim_[0];
        const int yes__dim_2_ = yes__dim_[1];
        
        if (test__dim_1_ != yes__dim_1_)
          Rf_error("dim(yes)[1] must equal dim(test)[1],"
                   " but are %0.f and %0.f",
                    (double)yes__dim_1_, (double)test__dim_1_);
        if (test__dim_2_ != yes__dim_2_)
          Rf_error("dim(yes)[2] must equal dim(test)[2],"
                   " but are %0.f and %0.f",
                    (double)yes__dim_2_, (double)test__dim_2_);
        const R_xlen_t out___len_ = (test__dim_1_) * (test__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = test__dim_1_;
          _dim[1] = test__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          test__,
          yes__,
          out___,
          test__dim_1_,
          test__dim_2_);
        
        UNPROTECT(2);
        return out_;
      }

# logical ifelse preserves the C logical storage convention

    Code
      fn
    Output
      function(test, yes) {
          declare(type(test = logical(n)), type(yes = logical(n)))
          ifelse(test, yes, FALSE)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, yes, out_, test__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: test__len_
      
        ! args
        integer(c_int), intent(in) :: test(test__len_) ! logical
        integer(c_int), intent(in) :: yes(test__len_) ! logical
        integer(c_int), intent(out) :: out_(test__len_) ! logical
        ! manifest end
      
      
        block
      
          out_ = merge((yes/=0), .false., (test/=0))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const test__,
        const int* const yes__,
        int* const out___,
        const R_xlen_t test__len_);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const R_xlen_t test__len_ = Rf_xlength(test);
        
        // yes
        _args = CDR(_args);
        SEXP yes = CAR(_args);
        if (TYPEOF(yes) != LGLSXP) {
          Rf_error("typeof(yes) must be 'logical', not '%s'", Rf_type2char(TYPEOF(yes)));
        }
        const int* const yes__ = LOGICAL(yes);
        const R_xlen_t yes__len_ = Rf_xlength(yes);
        
        if (test__len_ != yes__len_)
          Rf_error("length(yes) must equal length(test),"
                   " but are %0.f and %0.f",
                    (double)yes__len_, (double)test__len_);
        const R_xlen_t out___len_ = test__len_;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(
          test__,
          yes__,
          out___,
          test__len_);
        
        UNPROTECT(1);
        return out_;
      }

# nested ifelse captures its result before later mutations

    Code
      fn
    Output
      function(test, x) {
          declare(type(test = logical(n)), type(x = double(n)))
          change <- function() {
            test <<- !test
            x <<- x + 10
            0
          }
          ifelse(test, x, -x) + change()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, x, out_, test__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: test__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in out) :: test(test__len_) ! logical
        real(c_double), intent(in out) :: x(test__len_)
        real(c_double), intent(out) :: out_(test__len_)
        ! manifest end
      
      
      
        block
          real(c_double), allocatable :: btmp1_(:)
          real(c_double) :: btmp2_
      
          allocate(btmp1_(test__len_))
          btmp1_ = merge(x, (-x), (test/=0))
          call change(btmp2_)
          if (quickr_err_msg(1) /= c_null_char) return
          out_ = (btmp1_ + btmp2_)
        end block
      
        contains
          subroutine change(res)
            use iso_c_binding, only: c_double, c_ptrdiff_t
            implicit none
      
            real(c_double), intent(out) :: res
      
            if ((size(test, 1, kind=c_ptrdiff_t)) /= (size((.not. (test/=0)), 1, kind=c_ptrdiff_t))) then
              call quickr_set_error_msg("reassignment must preserve the shape of `test`")
              return
            end if
            test = (.not. (test/=0))
            if ((size(x, 1, kind=c_ptrdiff_t)) /= (size((x + 10.0_c_double), 1, kind=c_ptrdiff_t))) then
              call quickr_set_error_msg("reassignment must preserve the shape of `x`")
              return
            end if
            x = (x + 10.0_c_double)
            res = 0.0_c_double
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
        int* const test__,
        double* const x__,
        double* const out___,
        const R_xlen_t test__len_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        test = Rf_duplicate(test);
        SETCAR(_args, test);
        int* const test__ = LOGICAL(test);
        const R_xlen_t test__len_ = Rf_xlength(test);
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (test__len_ != x__len_)
          Rf_error("length(x) must equal length(test),"
                   " but are %0.f and %0.f",
                    (double)x__len_, (double)test__len_);
        const R_xlen_t out___len_ = test__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          test__,
          x__,
          out___,
          test__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# ifelse computes an effectful selector once

    Code
      fn
    Output
      function() {
          ifelse(runif(3) > 0.5, 1, -1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: out_(3)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
        block
          real(c_double) :: btmp1_(3)
      
          btmp1_ = [(unif_rand(), tmp1_=1, 3)]
          out_ = merge(1.0_c_double, -1.0_c_double, (btmp1_ > 0.5_c_double))
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(double* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

# scalar ifelse runs only the selected effectful branch

    Code
      fn
    Output
      function(test, x) {
          declare(type(test = logical(1)), type(x = double(1)))
          ifelse(test, runif(1), x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, x, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: test ! logical
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        block
          logical :: btmp1_ ! logical
          real(c_double) :: btmp2_
      
          btmp1_ = (test/=0)
          if (btmp1_) then
            btmp2_ = unif_rand()
          else
            btmp2_ = x
          end if
          out_ = btmp2_
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const test__,
        const double* const x__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const R_xlen_t test__len_ = Rf_xlength(test);
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (test__len_ != 1)
          Rf_error("length(test) must be 1, not %0.f",
                    (double)test__len_);
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        GetRNGstate();
        fn(test__, x__, out___);
        PutRNGstate();
        
        UNPROTECT(1);
        return out_;
      }

# vector ifelse allocates an effectful branch when selected

    Code
      fn
    Output
      function(test, n) {
          declare(type(test = logical(3)), type(n = integer(1)))
          ifelse(test, runif(n), 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: test(3) ! logical
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(3)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
        block
          logical :: btmp1_(3) ! logical
          real(c_double) :: btmp3_(3)
      
          btmp1_ = (test/=0)
          if (any(btmp1_)) then
            block
              real(c_double), allocatable :: btmp2_(:)
      
              allocate(btmp2_(n))
              btmp2_ = [(unif_rand(), tmp1_=1, n)]
              if (size(btmp2_, 1, kind=c_ptrdiff_t) /= 3) then
      call quickr_set_error_msg("ifelse() `yes` and `no` must be scalars or match the shape of `test`; R-style recycling is not&
      & supported")
                return
              end if
              where (btmp1_) btmp3_ = btmp2_
            end block
          end if
          if (any(.not. btmp1_)) then
            where (.not. btmp1_) btmp3_ = 0.0_c_double
          end if
          out_ = btmp3_
        end block
      
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const test__,
        const int* const n__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const R_xlen_t test__len_ = Rf_xlength(test);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (test__len_ != 3)
          Rf_error("length(test) must be 3, not %0.f",
                    (double)test__len_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = 3;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(
          test__,
          n__,
          out___,
          quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# ifelse allocates a named BLAS result inside its selected branch

    Code
      fn
    Output
      function(test, x) {
          declare(type(test = logical(k, k)), type(x = double(n, k)))
          ifelse(test, crossprod(x), 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(test, x, out_, test__dim_1_, x__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: test__dim_1_
        integer(c_int), intent(in), value :: x__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: test(test__dim_1_, test__dim_1_) ! logical
        real(c_double), intent(in) :: x(x__dim_1_, test__dim_1_)
        real(c_double), intent(out) :: out_(test__dim_1_, test__dim_1_)
        ! manifest end
      
      
        block
          logical, allocatable :: btmp4_(:, :) ! logical
          real(c_double), allocatable :: btmp5_(:, :)
      
          allocate(btmp4_(test__dim_1_, test__dim_1_))
          btmp4_ = (test/=0)
          if (any(btmp4_)) then
            block
              real(c_double), allocatable :: btmp1_(:, :)
              integer(c_int) :: btmp2_
              integer(c_int) :: btmp3_
      
              allocate(btmp1_(test__dim_1_, test__dim_1_))
              if (int(test__dim_1_, kind=c_int) == 0_c_int .or. int(x__dim_1_, kind=c_int) == 0_c_int) then
                btmp1_ = 0.0_c_double
              else
      call dsyrk('U', 'T', int(test__dim_1_, kind=c_int), int(x__dim_1_, kind=c_int), 1.0_c_double, x, int(x__dim_1_, kind=c_int),&
      & 0.0_c_double, btmp1_, int(test__dim_1_, kind=c_int))
              end if
              do btmp3_ = 1_c_int, int(test__dim_1_, kind=c_int) - 1_c_int
                do btmp2_ = btmp3_ + 1_c_int, int(test__dim_1_, kind=c_int)
                  btmp1_(btmp2_, btmp3_) = btmp1_(btmp3_, btmp2_)
                end do
              end do
              if (size(btmp1_, 1, kind=c_ptrdiff_t) /= size(btmp4_, 1, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("ifelse() `yes` and `no` must be scalars or match the shape of `test`; R-style recycling is not&
      & supported")
                return
              end if
              if (size(btmp1_, 2, kind=c_ptrdiff_t) /= size(btmp4_, 2, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("ifelse() `yes` and `no` must be scalars or match the shape of `test`; R-style recycling is not&
      & supported")
                return
              end if
              if (.not. allocated(btmp5_)) allocate(btmp5_(test__dim_1_, test__dim_1_))
              where (btmp4_) btmp5_ = btmp1_
            end block
          end if
          if (any(.not. btmp4_)) then
            if (.not. allocated(btmp5_)) allocate(btmp5_(test__dim_1_, test__dim_1_))
            where (.not. btmp4_) btmp5_ = 0.0_c_double
          end if
          if (.not. allocated(btmp5_)) allocate(btmp5_(test__dim_1_, test__dim_1_))
          out_ = btmp5_
        end block
      
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
        const int* const test__,
        const double* const x__,
        double* const out___,
        const R_len_t test__dim_1_,
        const R_len_t x__dim_1_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // test
        _args = CDR(_args);
        SEXP test = CAR(_args);
        if (TYPEOF(test) != LGLSXP) {
          Rf_error("typeof(test) must be 'logical', not '%s'", Rf_type2char(TYPEOF(test)));
        }
        const int* const test__ = LOGICAL(test);
        const int* const test__dim_ = ({
        SEXP dim_ = Rf_getAttrib(test, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "test must be a 2D-array, but length(dim(test)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int test__dim_1_ = test__dim_[0];
        const int test__dim_2_ = test__dim_[1];
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "x must be a 2D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        
        if (test__dim_1_ != test__dim_2_)
          Rf_error("dim(test)[2] must equal dim(test)[1],"
                   " but are %0.f and %0.f",
                    (double)test__dim_2_, (double)test__dim_1_);
        if (test__dim_1_ != x__dim_2_)
          Rf_error("dim(x)[2] must equal dim(test)[1],"
                   " but are %0.f and %0.f",
                    (double)x__dim_2_, (double)test__dim_1_);
        const R_xlen_t out___len_ = (test__dim_1_) * (test__dim_1_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = test__dim_1_;
          _dim[1] = test__dim_1_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          test__,
          x__,
          out___,
          test__dim_1_,
          x__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# an ordinary fill allocates after a conditional LAPACK write

    Code
      fn
    Output
      function(n, a, b, flag) {
          declare(
            type(n = integer(1)),
            type(a = double(n, n)),
            type(b = double(n)),
            type(flag = logical(1))
          )
          if (flag) {
            z <- solve(a, b)
          }
          z <- numeric(n)
          sum(z)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, a, b, flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: a(n, n)
        real(c_double), intent(in) :: b(n)
        integer(c_int), intent(in) :: flag ! logical
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: z(:)
        ! manifest end
      
        allocate(z(n))
      
      
        if ((flag/=0)) then
          block
            real(c_double), allocatable :: btmp1_(:, :)
            integer(c_int), allocatable :: btmp2_(:)
            integer(c_int) :: btmp3_
      
            allocate(btmp1_(n, n))
            btmp1_ = a
            if (.not. allocated(z)) allocate(z(n))
            z = b
            allocate(btmp2_(n))
            call dgesv(int(n, kind=c_int), int(1, kind=c_int), btmp1_, int(n, kind=c_int), btmp2_, z, int(n, kind=c_int), btmp3_)
            if (btmp3_ > 0_c_int) then
              call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
              return
            end if
            if (btmp3_ < 0_c_int) then
              call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
              return
            end if
          end block
        end if
        if (n < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((size(z, 1, kind=c_ptrdiff_t)) /= (n)) then
          call quickr_set_error_msg("reassignment must preserve the shape of `z`")
          return
        end if
        z = 0.0_c_double
        out_ = sum(z)
      
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
        const int* const n__,
        const double* const a__,
        const double* const b__,
        const int* const flag__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const int* const a__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a must be a 2D-array, but length(dim(a)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a__dim_1_ = a__dim_[0];
        const int a__dim_2_ = a__dim_[1];
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != a__dim_1_)
          Rf_error("dim(a)[1] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_1_, (double)_as_int_n);
        if (_as_int_n != a__dim_2_)
          Rf_error("dim(a)[2] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)_as_int_n);
        if (_as_int_n != b__len_)
          Rf_error("length(b) must equal n,"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)_as_int_n);
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          a__,
          b__,
          flag__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# initialization before a conditional write retains its allocation

    Code
      fn
    Output
      function(n, a, b, flag) {
          declare(
            type(n = integer(1)),
            type(a = double(n, n)),
            type(b = double(n)),
            type(flag = logical(1))
          )
          z <- numeric(n)
          if (flag) {
            z <- solve(a, b)
          }
          sum(z)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, a, b, flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: a(n, n)
        real(c_double), intent(in) :: b(n)
        integer(c_int), intent(in) :: flag ! logical
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: z(:)
        ! manifest end
      
        allocate(z(n))
      
      
        if (n < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        z = 0.0_c_double
        if ((flag/=0)) then
          block
            real(c_double), allocatable :: btmp1_(:, :)
            integer(c_int), allocatable :: btmp2_(:)
            integer(c_int) :: btmp3_
      
            allocate(btmp1_(n, n))
            btmp1_ = a
            if ((size(z, 1, kind=c_ptrdiff_t)) /= (n)) then
              call quickr_set_error_msg("reassignment must preserve the shape of `z`")
              return
            end if
            z = b
            allocate(btmp2_(n))
            call dgesv(int(n, kind=c_int), int(1, kind=c_int), btmp1_, int(n, kind=c_int), btmp2_, z, int(n, kind=c_int), btmp3_)
            if (btmp3_ > 0_c_int) then
              call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
              return
            end if
            if (btmp3_ < 0_c_int) then
              call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
              return
            end if
          end block
        end if
        out_ = sum(z)
      
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
        const int* const n__,
        const double* const a__,
        const double* const b__,
        const int* const flag__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const int* const a__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a must be a 2D-array, but length(dim(a)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a__dim_1_ = a__dim_[0];
        const int a__dim_2_ = a__dim_[1];
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        // flag
        _args = CDR(_args);
        SEXP flag = CAR(_args);
        if (TYPEOF(flag) != LGLSXP) {
          Rf_error("typeof(flag) must be 'logical', not '%s'", Rf_type2char(TYPEOF(flag)));
        }
        const int* const flag__ = LOGICAL(flag);
        const R_xlen_t flag__len_ = Rf_xlength(flag);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != a__dim_1_)
          Rf_error("dim(a)[1] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_1_, (double)_as_int_n);
        if (_as_int_n != a__dim_2_)
          Rf_error("dim(a)[2] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)_as_int_n);
        if (_as_int_n != b__len_)
          Rf_error("length(b) must equal n,"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)_as_int_n);
        if (flag__len_ != 1)
          Rf_error("length(flag) must be 1, not %0.f",
                    (double)flag__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          a__,
          b__,
          flag__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# conditional writes retain storage across loop iterations

    Code
      fn
    Output
      function(n, a, b) {
          declare(
            type(n = integer(1)),
            type(a = double(n, n)),
            type(b = double(n))
          )
          out <- 0
          for (i in 1:3) {
            if (i > 1L) {
              z <- solve(a, b)
            }
            z <- numeric(n)
            out <- out + sum(z)
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, a, b, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(in) :: a(n, n)
        real(c_double), intent(in) :: b(n)
        real(c_double), intent(out) :: out
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: tmp1_
        real(c_double), allocatable :: z(:)
        ! manifest end
      
        allocate(z(n))
      
      
        out = 0.0_c_double
        do tmp1_ = 1_c_int, 3_c_int, sign(1, 3_c_int-1_c_int)
          i = tmp1_
          if ((i > 1_c_int)) then
            block
              real(c_double), allocatable :: btmp1_(:, :)
              integer(c_int), allocatable :: btmp2_(:)
              integer(c_int) :: btmp3_
      
              allocate(btmp1_(n, n))
              btmp1_ = a
              if (.not. allocated(z)) allocate(z(n))
              z = b
              allocate(btmp2_(n))
              call dgesv(int(n, kind=c_int), int(1, kind=c_int), btmp1_, int(n, kind=c_int), btmp2_, z, int(n, kind=c_int), btmp3_)
              if (btmp3_ > 0_c_int) then
                call quickr_set_error_msg("Lapack routine dgesv: system is exactly singular")
                return
              end if
              if (btmp3_ < 0_c_int) then
                call quickr_set_error_msg("Lapack routine dgesv: illegal argument")
                return
              end if
            end block
          end if
          if (n < 0) then
            call quickr_set_error_msg("invalid 'length' argument")
            return
          end if
          if ((size(z, 1, kind=c_ptrdiff_t)) /= (n)) then
            call quickr_set_error_msg("reassignment must preserve the shape of `z`")
            return
          end if
          z = 0.0_c_double
          out = (out + sum(z))
        end do
      
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
        const int* const n__,
        const double* const a__,
        const double* const b__,
        double* const out__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", Rf_type2char(TYPEOF(a)));
        }
        const double* const a__ = REAL(a);
        const int* const a__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a must be a 2D-array, but length(dim(a)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a__dim_1_ = a__dim_[0];
        const int a__dim_2_ = a__dim_[1];
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", Rf_type2char(TYPEOF(b)));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        if (_as_int_n != a__dim_1_)
          Rf_error("dim(a)[1] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_1_, (double)_as_int_n);
        if (_as_int_n != a__dim_2_)
          Rf_error("dim(a)[2] must equal n,"
                   " but are %0.f and %0.f",
                    (double)a__dim_2_, (double)_as_int_n);
        if (_as_int_n != b__len_)
          Rf_error("length(b) must equal n,"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)_as_int_n);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          n__,
          a__,
          b__,
          out__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

# lazy closure diagnostics retain effects before the error

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = logical(1)))
          predicate <- function() {
            draw <- runif(1)
            invalid <- abs()
            later <- runif(2)
            TRUE
          }
          flag && predicate()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: flag ! logical
        integer(c_int), intent(out) :: out_ ! logical
      
        ! locals
        logical :: tmp1_ ! logical
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
      
        tmp1_ = (flag/=0)
        if (tmp1_) then
          block
            logical :: btmp1_ ! logical
      
            call predicate(btmp1_)
            if (quickr_err_msg(1) /= c_null_char) return
            if (.true.) then
              call quickr_set_error_msg("`abs` requires exactly one argument")
              return
            end if
            tmp1_ = .false.
          end block
        end if
        out_ = tmp1_
      
        contains
          subroutine predicate(res)
            use iso_c_binding, only: c_double
            implicit none
      
            logical, intent(out) :: res ! logical
            real(c_double) :: draw
      
            draw = unif_rand()
            if (.true.) then
              call quickr_set_error_msg("`abs` requires exactly one argument")
              return
            end if
      
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const flag__,
        int* const out___,
        char* quickr_err_msg);
      
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
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(flag__, out___, quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# lazy while-condition diagnostics retain their hoisted effects

    Code
      fn
    Output
      function(flag) {
          declare(type(flag = logical(1)))
          predicate <- function() {
            while ((runif(1) + abs()) > 0) {
              later <- runif(2)
            }
            TRUE
          }
          flag && predicate()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(flag, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: flag ! logical
        integer(c_int), intent(out) :: out_ ! logical
      
        ! locals
        logical :: tmp1_ ! logical
        ! manifest end
      
        interface
          function unif_rand() bind(c, name = "unif_rand") result(u)
            use iso_c_binding, only: c_double
            real(c_double) :: u
          end function unif_rand
        end interface
      
      
      
        tmp1_ = (flag/=0)
        if (tmp1_) then
          block
            logical :: btmp1_ ! logical
      
            call predicate(btmp1_)
            if (quickr_err_msg(1) /= c_null_char) return
            if (.true.) then
              call quickr_set_error_msg("`abs` requires exactly one argument")
              return
            end if
            tmp1_ = .false.
          end block
        end if
        out_ = tmp1_
      
        contains
          subroutine predicate(res)
            use iso_c_binding, only: c_double
            implicit none
      
            logical, intent(out) :: res ! logical
      
            block
              real(c_double) :: btmp1_
      
              btmp1_ = unif_rand()
            end block
            if (.true.) then
              call quickr_set_error_msg("`abs` requires exactly one argument")
              return
            end if
      
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
      #include <R_ext/Random.h>
      
      
      extern void fn(
        const int* const flag__,
        int* const out___,
        char* quickr_err_msg);
      
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
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        GetRNGstate();
        fn(flag__, out___, quickr_err_msg);
        PutRNGstate();
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# parallel loops keep generated temporaries inside iteration blocks

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- numeric(length(x))
          declare(parallel())
          for (i in seq_along(x)) {
            out[i] <- sqrt(abs(x[i]))
          }
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
        integer(c_int) :: i
        ! manifest end
      
      
        if (x__len_ < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        out = 0.0_c_double
      
        !$omp parallel do
        do i = 1, size(x)
          block
            real(c_double) :: btmp1_
      
            btmp1_ = abs(x(i))
            out(i) = sqrt(btmp1_)
          end block
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

# parallel sapply propagates a closure error through cancellation

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(1)))
          declare(parallel())
          out <- sapply(seq_len(1L), function(i) {
            if (x < 0) {
              stop("x must be nonnegative")
            }
            x + 1
          })
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x
        real(c_double), intent(out) :: out
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
      
        !$omp parallel do
        do tmp1_ = 1_c_int, 1_c_int
          call closure1_(tmp1_, out)
          if (quickr_err_msg(1) /= c_null_char) then
          !$omp cancel do
        end if
        end do
        !$omp end parallel do
        if (quickr_err_msg(1) /= c_null_char) return
      
        contains
          subroutine closure1_(i, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            real(c_double), intent(out) :: res
      
            if ((x < 0.0_c_double)) then
              call quickr_set_error_msg("x must be nonnegative")
              return
            end if
            res = (x + 1.0_c_double)
          end subroutine
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
        const double* const x__,
        double* const out__,
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
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, out__, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out;
      }

