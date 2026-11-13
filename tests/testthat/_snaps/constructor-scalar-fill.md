# nested logical fills broadcast into rank-three results

    Code
      fn
    Output
      function(k, n) {
          declare(type(k = integer(1)), type(n = integer(1)))
          array(logical(k), dim = c(2L, n, 3L))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(k, n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: k
        integer(c_int), intent(in) :: n
        integer(c_int), intent(out) :: out_(2, n, 3) ! logical
        ! manifest end
      
      
        if (k < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((k) == 0 .and. (2) > 0 .and. (n) > 0 .and. (3) > 0) then
          call quickr_set_error_msg("array() with empty data would produce NA values, which are not supported")
          return
        end if
        out_ = .false.
      
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
        const int* const k__,
        const int* const n__,
        int* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != INTSXP) {
          Rf_error("typeof(k) must be 'integer', not '%s'", Rf_type2char(TYPEOF(k)));
        }
        const int* const k__ = INTEGER(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (k__len_ != 1)
          Rf_error("length(k) must be 1, not %0.f",
                    (double)k__len_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = (2) * (_as_int_n) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = _as_int_n;
          _dim[2] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          k__,
          n__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# known nonempty fills need no source storage or error machinery

    Code
      fn
    Output
      function() {
          matrix(integer(2L), nrow = 2L, ncol = 3L)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(out) :: out_(2, 3)
        ! manifest end
      
        out_ = 0_c_int
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const out___);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t out___len_ = (2) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(out___);
        
        UNPROTECT(2);
        return out_;
      }

# nested fills with scalar targets need no source indexing

    Code
      fn
    Output
      function(k) {
          declare(type(k = integer(1)))
          sum(array(integer(k), dim = 1L) + 1L)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(k, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: k
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
        if (k < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((k) == 0 .and. (1) > 0) then
          call quickr_set_error_msg("array() with empty data would produce NA values, which are not supported")
          return
        end if
        out_ = (0_c_int + 1_c_int)
      
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
        const int* const k__,
        int* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != INTSXP) {
          Rf_error("typeof(k) must be 'integer', not '%s'", Rf_type2char(TYPEOF(k)));
        }
        const int* const k__ = INTEGER(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        if (k__len_ != 1)
          Rf_error("length(k) must be 1, not %0.f",
                    (double)k__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(k__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# nested fills materialize their result for array consumers

    Code
      fn
    Output
      function(k, n) {
          declare(type(k = integer(1)), type(n = integer(1)))
          sum(array(integer(k), dim = c(2L, n)) + 1L) +
            sum(matrix(double(k), nrow = n, ncol = 2L) + 1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(k, n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: k
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        block
          integer(c_int), allocatable :: btmp1_(:, :)
          real(c_double), allocatable :: btmp2_(:, :)
      
          allocate(btmp1_(2, n))
          allocate(btmp2_(n, 2))
          if (k < 0) then
            call quickr_set_error_msg("invalid 'length' argument")
            return
          end if
          if ((k) == 0 .and. (2) > 0 .and. (n) > 0) then
            call quickr_set_error_msg("array() with empty data would produce NA values, which are not supported")
            return
          end if
          btmp1_ = 0_c_int
          if (k < 0) then
            call quickr_set_error_msg("invalid 'length' argument")
            return
          end if
          if ((k) == 0 .and. (n) > 0 .and. (2) > 0) then
            call quickr_set_error_msg("matrix() with empty data would produce NA values, which are not supported")
            return
          end if
          btmp2_ = 0.0_c_double
          out_ = (sum((btmp1_ + 1_c_int)) + sum((btmp2_ + 1.0_c_double)))
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
        const int* const k__,
        const int* const n__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != INTSXP) {
          Rf_error("typeof(k) must be 'integer', not '%s'", Rf_type2char(TYPEOF(k)));
        }
        const int* const k__ = INTEGER(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (k__len_ != 1)
          Rf_error("length(k) must be 1, not %0.f",
                    (double)k__len_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          k__,
          n__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# nested fills resize deferred destinations without a source array

    Code
      fn
    Output
      function(k, n, m) {
          declare(
            type(k = integer(1)),
            type(n = integer(1)),
            type(m = integer(1)),
            type(out = double(NA, NA))
          )
          out <- matrix(1, nrow = 2L, ncol = 3L)
          first <- sum(out)
          out <- array(numeric(k), dim = c(n, m))
          first + sum(out + 1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(k, n, m, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: k
        integer(c_int), intent(in) :: n
        integer(c_int), intent(in) :: m
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        real(c_double) :: first
        ! manifest end
      
      
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (2) .or. size(out, 2, kind=c_ptrdiff_t) /= (3)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(2, 3))
        out = 1.0_c_double
        first = sum(out)
        if (k < 0) then
          call quickr_set_error_msg("invalid 'length' argument")
          return
        end if
        if ((k) == 0 .and. (n) > 0 .and. (m) > 0) then
          call quickr_set_error_msg("array() with empty data would produce NA values, which are not supported")
          return
        end if
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (n) .or. size(out, 2, kind=c_ptrdiff_t) /= (m)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(n, m))
        out = 0.0_c_double
        out_ = (first + sum((out + 1.0_c_double)))
      
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
        const int* const k__,
        const int* const n__,
        const int* const m__,
        double* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != INTSXP) {
          Rf_error("typeof(k) must be 'integer', not '%s'", Rf_type2char(TYPEOF(k)));
        }
        const int* const k__ = INTEGER(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const R_xlen_t m__len_ = Rf_xlength(m);
        
        if (k__len_ != 1)
          Rf_error("length(k) must be 1, not %0.f",
                    (double)k__len_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        if (m__len_ != 1)
          Rf_error("length(m) must be 1, not %0.f",
                    (double)m__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          k__,
          n__,
          m__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# nested fills resize captured storage through superassignment

    Code
      fn
    Output
      function(n) {
          declare(type(n = integer(1)), type(out = double(NA, NA)))
          out <- matrix(1, nrow = 2L, ncol = 3L)
          reset_matrix <- function() {
            out <<- matrix(numeric(n), nrow = n, ncol = 2L)
            sum(out + 1)
          }
          reset_array <- function() {
            out <<- array(numeric(n), dim = c(2L, n))
            sum(out + 2)
          }
          first <- reset_matrix()
          first + reset_array()
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(n, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: out(:, :)
        real(c_double) :: first
        ! manifest end
      
      
        if (allocated(out)) then
          if (size(out, 1, kind=c_ptrdiff_t) /= (2) .or. size(out, 2, kind=c_ptrdiff_t) /= (3)) deallocate(out)
        end if
        if (.not. allocated(out)) allocate(out(2, 3))
        out = 1.0_c_double
      
      
        call reset_matrix(first)
        if (quickr_err_msg(1) /= c_null_char) return
        block
          real(c_double) :: btmp1_
      
          call reset_array(btmp1_)
          if (quickr_err_msg(1) /= c_null_char) return
          out_ = (first + btmp1_)
        end block
      
        contains
          subroutine reset_matrix(res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(out) :: res
      
            block
              real(c_double), allocatable :: btmp1_(:, :)
      
              allocate(btmp1_(n, 2))
              if (n < 0) then
                call quickr_set_error_msg("invalid 'length' argument")
                return
              end if
              if ((n) == 0 .and. (n) > 0 .and. (2) > 0) then
                call quickr_set_error_msg("matrix() with empty data would produce NA values, which are not supported")
                return
              end if
              btmp1_ = 0.0_c_double
              out = btmp1_
            end block
            res = sum((out + 1.0_c_double))
          end subroutine
          subroutine reset_array(res)
            use iso_c_binding, only: c_double
            implicit none
      
            real(c_double), intent(out) :: res
      
            block
              real(c_double), allocatable :: btmp1_(:, :)
      
              allocate(btmp1_(2, n))
              if (n < 0) then
                call quickr_set_error_msg("invalid 'length' argument")
                return
              end if
              if ((n) == 0 .and. (2) > 0 .and. (n) > 0) then
                call quickr_set_error_msg("array() with empty data would produce NA values, which are not supported")
                return
              end if
              btmp1_ = 0.0_c_double
              out = btmp1_
            end block
            res = sum((out + 2.0_c_double))
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
        const int* const n__,
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
        
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(n__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

