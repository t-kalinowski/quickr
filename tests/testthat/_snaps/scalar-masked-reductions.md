# masked scalar sums and products retain empty identities and modes

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Masked integer scalar
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = MODE(1)), type(mask = logical(1)))
              total <- sum(x[mask])
              product <- prod(x[mask])
              list(total = total, product = product)
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, total, product) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: mask ! logical
        integer(c_int), intent(out) :: total
        real(c_double), intent(out) :: product
        ! manifest end
      
      
        total = merge(x, 0_c_int, (mask/=0))
        product = merge(real(x, kind=c_double), 1.0_c_double, (mask/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const mask__,
        int* const total__,
        double* const product__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t total__len_ = (1);
        SEXP total = PROTECT(Rf_allocVector(INTSXP, total__len_));
        int* total__ = INTEGER(total);
        const R_xlen_t product__len_ = (1);
        SEXP product = PROTECT(Rf_allocVector(REALSXP, product__len_));
        double* product__ = REAL(product);
        
        fn(
          x__,
          mask__,
          total__,
          product__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, total);
        SET_VECTOR_ELT(_ans, 1, product);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("total"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("product"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Masked logical scalar
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = MODE(1)), type(mask = logical(1)))
              total <- sum(x[mask])
              product <- prod(x[mask])
              list(total = total, product = product)
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, total, product) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: x ! logical
        integer(c_int), intent(in) :: mask ! logical
        integer(c_int), intent(out) :: total
        real(c_double), intent(out) :: product
        ! manifest end
      
      
        total = merge(merge(1_c_int, 0_c_int, (x/=0)), 0_c_int, (mask/=0))
        product = merge(merge(1.0_c_double, 0.0_c_double, (x/=0)), 1.0_c_double, (mask/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        const int* const mask__,
        int* const total__,
        double* const product__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t total__len_ = (1);
        SEXP total = PROTECT(Rf_allocVector(INTSXP, total__len_));
        int* total__ = INTEGER(total);
        const R_xlen_t product__len_ = (1);
        SEXP product = PROTECT(Rf_allocVector(REALSXP, product__len_));
        double* product__ = REAL(product);
        
        fn(
          x__,
          mask__,
          total__,
          product__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, total);
        SET_VECTOR_ELT(_ans, 1, product);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("total"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("product"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Masked double scalar
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = MODE(1)), type(mask = logical(1)))
              total <- sum(x[mask])
              product <- prod(x[mask])
              list(total = total, product = product)
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, total, product) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x
        integer(c_int), intent(in) :: mask ! logical
        real(c_double), intent(out) :: total
        real(c_double), intent(out) :: product
        ! manifest end
      
      
        total = merge(x, 0.0_c_double, (mask/=0))
        product = merge(x, 1.0_c_double, (mask/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const mask__,
        double* const total__,
        double* const product__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t total__len_ = (1);
        SEXP total = PROTECT(Rf_allocVector(REALSXP, total__len_));
        double* total__ = REAL(total);
        const R_xlen_t product__len_ = (1);
        SEXP product = PROTECT(Rf_allocVector(REALSXP, product__len_));
        double* product__ = REAL(product);
        
        fn(
          x__,
          mask__,
          total__,
          product__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, total);
        SET_VECTOR_ELT(_ans, 1, product);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("total"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("product"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Masked complex scalar
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = MODE(1)), type(mask = logical(1)))
              total <- sum(x[mask])
              product <- prod(x[mask])
              list(total = total, product = product)
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, total, product) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_int
        implicit none
      
        ! manifest start
        ! args
        complex(c_double_complex), intent(in) :: x
        integer(c_int), intent(in) :: mask ! logical
        complex(c_double_complex), intent(out) :: total
        complex(c_double_complex), intent(out) :: product
        ! manifest end
      
      
        total = merge(x, (0.0_c_double, 0.0_c_double), (mask/=0))
        product = merge(x, (1.0_c_double, 0.0_c_double), (mask/=0))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const Rcomplex* const x__,
        const int* const mask__,
        Rcomplex* const total__,
        Rcomplex* const product__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != CPLXSXP) {
          Rf_error("typeof(x) must be 'complex', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const Rcomplex* const x__ = COMPLEX(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t total__len_ = (1);
        SEXP total = PROTECT(Rf_allocVector(CPLXSXP, total__len_));
        Rcomplex* total__ = COMPLEX(total);
        const R_xlen_t product__len_ = (1);
        SEXP product = PROTECT(Rf_allocVector(CPLXSXP, product__len_));
        Rcomplex* product__ = COMPLEX(product);
        
        fn(
          x__,
          mask__,
          total__,
          product__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, total);
        SET_VECTOR_ELT(_ans, 1, product);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("total"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("product"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# masked scalar extrema reject an empty selection

    Code
      fn
    Output
      function(x, mask) {
                declare(type(x = MODE(1)), type(mask = logical(1)))
                REDUCE(x[mask])
              }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in) :: mask ! logical
        integer(c_int), intent(out) :: out_
        ! manifest end
      
      
        if (.not. ((mask/=0))) then
          call quickr_set_error_msg("min()/max() of empty inputs are not supported")
          return
        end if
        out_ = x
      
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
        const int* const x__,
        const int* const mask__,
        int* const out___,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        const int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          mask__,
          out___,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# multi-argument extrema ignore empty scalar selections

    Code
      fn
    Output
      function(x, y, mask_x, mask_y) {
          declare(
            type(x = integer(1)),
            type(y = double(1)),
            type(mask_x = logical(1)),
            type(mask_y = logical(1))
          )
          low <- min(x[mask_x], y[mask_y])
          high <- max(x[mask_x], y[mask_y])
          list(low = low, high = high)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, mask_x, mask_y, low, high, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x
        real(c_double), intent(in) :: y
        integer(c_int), intent(in) :: mask_x ! logical
        integer(c_int), intent(in) :: mask_y ! logical
        real(c_double), intent(out) :: low
        real(c_double), intent(out) :: high
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp2_ = 0_c_int
      
          if ((mask_x/=0)) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = real(x, kind=c_double)
              btmp2_ = 1_c_int
            else
              btmp1_ = min(btmp1_, real(x, kind=c_double))
            end if
          end if
      
          if ((mask_y/=0)) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = y
              btmp2_ = 1_c_int
            else
              btmp1_ = min(btmp1_, y)
            end if
          end if
          if (btmp2_ == 0_c_int) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
          low = btmp1_
        end block
        block
          real(c_double) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp2_ = 0_c_int
      
          if ((mask_x/=0)) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = real(x, kind=c_double)
              btmp2_ = 1_c_int
            else
              btmp1_ = max(btmp1_, real(x, kind=c_double))
            end if
          end if
      
          if ((mask_y/=0)) then
            if (btmp2_ == 0_c_int) then
              btmp1_ = y
              btmp2_ = 1_c_int
            else
              btmp1_ = max(btmp1_, y)
            end if
          end if
          if (btmp2_ == 0_c_int) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
          high = btmp1_
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
        const int* const x__,
        const double* const y__,
        const int* const mask_x__,
        const int* const mask_y__,
        double* const low__,
        double* const high__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        // mask_x
        _args = CDR(_args);
        SEXP mask_x = CAR(_args);
        if (TYPEOF(mask_x) != LGLSXP) {
          Rf_error("typeof(mask_x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask_x)));
        }
        const int* const mask_x__ = LOGICAL(mask_x);
        const R_xlen_t mask_x__len_ = Rf_xlength(mask_x);
        
        // mask_y
        _args = CDR(_args);
        SEXP mask_y = CAR(_args);
        if (TYPEOF(mask_y) != LGLSXP) {
          Rf_error("typeof(mask_y) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask_y)));
        }
        const int* const mask_y__ = LOGICAL(mask_y);
        const R_xlen_t mask_y__len_ = Rf_xlength(mask_y);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (y__len_ != 1)
          Rf_error("length(y) must be 1, not %0.f",
                    (double)y__len_);
        if (mask_x__len_ != 1)
          Rf_error("length(mask_x) must be 1, not %0.f",
                    (double)mask_x__len_);
        if (mask_y__len_ != 1)
          Rf_error("length(mask_y) must be 1, not %0.f",
                    (double)mask_y__len_);
        const R_xlen_t low__len_ = (1);
        SEXP low = PROTECT(Rf_allocVector(REALSXP, low__len_));
        double* low__ = REAL(low);
        const R_xlen_t high__len_ = (1);
        SEXP high = PROTECT(Rf_allocVector(REALSXP, high__len_));
        double* high__ = REAL(high);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          mask_x__,
          mask_y__,
          low__,
          high__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, low);
        SET_VECTOR_ELT(_ans, 1, high);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("low"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("high"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# masked scalar reductions run before later argument effects

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: sum before effects
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = integer(1)), type(mask = logical(1)))
              change <- function() {
                x <<- 9L
                mask <<- !mask
                4L
              }
              result <- REDUCE(x[mask], change())
              result
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, result) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        integer(c_int), intent(in out) :: mask ! logical
        integer(c_int), intent(out) :: result
        ! manifest end
      
      
      
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
      
          btmp1_ = merge(x, 0_c_int, (mask/=0))
          call change(btmp2_)
          result = (btmp1_ + btmp2_)
        end block
      
        contains
          subroutine change(res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(out) :: res
      
            x = 9_c_int
            mask = (.not. (mask/=0))
            res = 4_c_int
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        int* const x__,
        int* const mask__,
        int* const result__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        mask = Rf_duplicate(mask);
        SETCAR(_args, mask);
        int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t result__len_ = (1);
        SEXP result = PROTECT(Rf_allocVector(INTSXP, result__len_));
        int* result__ = INTEGER(result);
        
        fn(x__, mask__, result__);
        
        UNPROTECT(1);
        return result;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: min before effects
    Code
      fn
    Output
      function(x, mask) {
              declare(type(x = integer(1)), type(mask = logical(1)))
              change <- function() {
                x <<- 9L
                mask <<- !mask
                4L
              }
              result <- REDUCE(x[mask], change())
              result
            }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, mask, result, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_int, c_null_char
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in out) :: x
        integer(c_int), intent(in out) :: mask ! logical
        integer(c_int), intent(out) :: result
        ! manifest end
      
      
      
        block
          integer(c_int) :: btmp1_
          integer(c_int) :: btmp2_
          integer(c_int) :: btmp3_
      
          btmp3_ = 0_c_int
      
          if ((mask/=0)) then
            if (btmp3_ == 0_c_int) then
              btmp2_ = x
              btmp3_ = 1_c_int
            else
              btmp2_ = min(btmp2_, x)
            end if
          end if
          call change(btmp1_)
          if (btmp3_ == 0_c_int) then
            btmp2_ = btmp1_
            btmp3_ = 1_c_int
          else
            btmp2_ = min(btmp2_, btmp1_)
          end if
          if (btmp3_ == 0_c_int) then
            call quickr_set_error_msg("min()/max() of empty inputs are not supported")
            return
          end if
          result = btmp2_
        end block
      
        contains
          subroutine change(res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(out) :: res
      
            x = 9_c_int
            mask = (.not. (mask/=0))
            res = 4_c_int
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
        int* const x__,
        int* const mask__,
        int* const result__,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // mask
        _args = CDR(_args);
        SEXP mask = CAR(_args);
        if (TYPEOF(mask) != LGLSXP) {
          Rf_error("typeof(mask) must be 'logical', not '%s'", Rf_type2char(TYPEOF(mask)));
        }
        mask = Rf_duplicate(mask);
        SETCAR(_args, mask);
        int* const mask__ = LOGICAL(mask);
        const R_xlen_t mask__len_ = Rf_xlength(mask);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        if (mask__len_ != 1)
          Rf_error("length(mask) must be 1, not %0.f",
                    (double)mask__len_);
        const R_xlen_t result__len_ = (1);
        SEXP result = PROTECT(Rf_allocVector(INTSXP, result__len_));
        int* result__ = INTEGER(result);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          mask__,
          result__,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return result;
      }

