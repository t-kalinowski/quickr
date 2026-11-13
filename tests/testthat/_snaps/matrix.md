# matrix

    Code
      r2f(fn)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: a
        integer(c_int), intent(in) :: b
        real(c_double), intent(out) :: out(a, b)
        ! manifest end
      
      
      
        out = 0.0_c_double
      end subroutine
      
      @r: function (a, b)
        {
            declare(type(a = integer(1)))
            declare(type(b = integer(1)))
            out <- matrix(0, a, b)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const int* const a__,
          const int* const b__,
          double* const out__);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != INTSXP) {
            Rf_error("typeof(a) must be 'integer', not '%s'", Rf_type2char(TYPEOF(a)));
          }
          const int* const a__ = INTEGER(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          // b
          _args = CDR(_args);
          SEXP b = CAR(_args);
          if (TYPEOF(b) != INTSXP) {
            Rf_error("typeof(b) must be 'integer', not '%s'", Rf_type2char(TYPEOF(b)));
          }
          const int* const b__ = INTEGER(b);
          const R_xlen_t b__len_ = Rf_xlength(b);
        
          if (a__len_ != 1)
            Rf_error("length(a) must be 1, not %0.f",
                      (double)a__len_);
          if (b__len_ != 1)
            Rf_error("length(b) must be 1, not %0.f",
                      (double)b__len_);
          const int _as_int_a = Rf_asInteger(a);
          const int _as_int_b = Rf_asInteger(b);
          const R_xlen_t out__len_ = (_as_int_a) * (_as_int_b);
          SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
          double* out__ = REAL(out);
          {
            const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
            int* const _dim = INTEGER(_dim_sexp);
            _dim[0] = _as_int_a;
            _dim[1] = _as_int_b;
            Rf_dimgets(out, _dim_sexp);
          }
        
          fn(a__, b__, out__);
        
          UNPROTECT(2);
          return out;
        }

# reuse implicit size

    Code
      print(fsub)
    Output
      subroutine fn(a1, a2, out, a1__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a1__len_
      
        ! args
        real(c_double), intent(in) :: a1(a1__len_)
        real(c_double), intent(in) :: a2(a1__len_, a1__len_)
        real(c_double), intent(out) :: out(a1__len_)
        ! manifest end
      
      
      
        out = (a1 + a2(1_c_int, :))
      end subroutine
      
      @r: function (a1, a2)
        {
            declare(type(a1 = double(n)))
            declare(type(a2 = double(n, n)))
            out <- a1 + a2[1, ]
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const double* const a1__,
          const double* const a2__,
          double* const out__,
          const R_xlen_t a1__len_);
        
        SEXP fn_(SEXP _args) {
          // a1
          _args = CDR(_args);
          SEXP a1 = CAR(_args);
          if (TYPEOF(a1) != REALSXP) {
            Rf_error("typeof(a1) must be 'double', not '%s'", Rf_type2char(TYPEOF(a1)));
          }
          const double* const a1__ = REAL(a1);
          const R_xlen_t a1__len_ = Rf_xlength(a1);
        
          // a2
          _args = CDR(_args);
          SEXP a2 = CAR(_args);
          if (TYPEOF(a2) != REALSXP) {
            Rf_error("typeof(a2) must be 'double', not '%s'", Rf_type2char(TYPEOF(a2)));
          }
          const double* const a2__ = REAL(a2);
          const int* const a2__dim_ = ({
          SEXP dim_ = Rf_getAttrib(a2, R_DimSymbol);
          if (Rf_length(dim_) != 2) Rf_error(
            "a2 must be a 2D-array, but length(dim(a2)) is %i",
            (int) Rf_length(dim_));
          INTEGER(dim_);});
          const int a2__dim_1_ = a2__dim_[0];
          const int a2__dim_2_ = a2__dim_[1];
        
          if (a1__len_ != a2__dim_1_)
            Rf_error("dim(a2)[1] must equal length(a1),"
                     " but are %0.f and %0.f",
                      (double)a2__dim_1_, (double)a1__len_);
          if (a1__len_ != a2__dim_2_)
            Rf_error("dim(a2)[2] must equal length(a1),"
                     " but are %0.f and %0.f",
                      (double)a2__dim_2_, (double)a1__len_);
          const R_xlen_t out__len_ = a1__len_;
          SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
          double* out__ = REAL(out);
        
          fn(
            a1__,
            a2__,
            out__,
            a1__len_);
        
          UNPROTECT(1);
          return out;
        }
    Code
      cat(c_wrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a1__,
        const double* const a2__,
        double* const out__,
        const R_xlen_t a1__len_);
      
      SEXP fn_(SEXP _args) {
        // a1
        _args = CDR(_args);
        SEXP a1 = CAR(_args);
        if (TYPEOF(a1) != REALSXP) {
          Rf_error("typeof(a1) must be 'double', not '%s'", Rf_type2char(TYPEOF(a1)));
        }
        const double* const a1__ = REAL(a1);
        const R_xlen_t a1__len_ = Rf_xlength(a1);
        
        // a2
        _args = CDR(_args);
        SEXP a2 = CAR(_args);
        if (TYPEOF(a2) != REALSXP) {
          Rf_error("typeof(a2) must be 'double', not '%s'", Rf_type2char(TYPEOF(a2)));
        }
        const double* const a2__ = REAL(a2);
        const int* const a2__dim_ = ({
        SEXP dim_ = Rf_getAttrib(a2, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "a2 must be a 2D-array, but length(dim(a2)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int a2__dim_1_ = a2__dim_[0];
        const int a2__dim_2_ = a2__dim_[1];
        
        if (a1__len_ != a2__dim_1_)
          Rf_error("dim(a2)[1] must equal length(a1),"
                   " but are %0.f and %0.f",
                    (double)a2__dim_1_, (double)a1__len_);
        if (a1__len_ != a2__dim_2_)
          Rf_error("dim(a2)[2] must equal length(a1),"
                   " but are %0.f and %0.f",
                    (double)a2__dim_2_, (double)a1__len_);
        const R_xlen_t out__len_ = a1__len_;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          a1__,
          a2__,
          out__,
          a1__len_);
        
        UNPROTECT(1);
        return out;
      }

# t() and diag() preserve integer mode

    Code
      fn
    Output
      function(m) {
          declare(type(m = integer(3, 3)))
          out <- diag(m)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m, out) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: m(3, 3)
        integer(c_int), intent(out) :: out(3)
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          do btmp1_ = 1_c_int, int(3, kind=c_int)
            out(btmp1_) = m(btmp1_, btmp1_)
          end do
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const m__, int* const out__);
      
      SEXP fn_(SEXP _args) {
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != INTSXP) {
          Rf_error("typeof(m) must be 'integer', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = INTEGER(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 3)
          Rf_error("dim(m)[1] must be 3, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 3)
          Rf_error("dim(m)[2] must be 3, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out__len_ = 3;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(m__, out__);
        
        UNPROTECT(1);
        return out;
      }

# diag() preserves integer-backed logical storage in an intermediate

    Code
      fn
    Output
      function(m) {
          declare(type(m = logical(2, 2)))
          d <- diag(m)
          as.integer(d)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(m, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: m(2, 2) ! logical
        integer(c_int), intent(out) :: out_(2)
      
        ! locals
        integer(c_int) :: d(2) ! logical
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          do btmp1_ = 1_c_int, int(2, kind=c_int)
            d(btmp1_) = m(btmp1_, btmp1_)
          end do
        end block
        out_ = d
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const m__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // m
        _args = CDR(_args);
        SEXP m = CAR(_args);
        if (TYPEOF(m) != LGLSXP) {
          Rf_error("typeof(m) must be 'logical', not '%s'", Rf_type2char(TYPEOF(m)));
        }
        const int* const m__ = LOGICAL(m);
        const int* const m__dim_ = ({
        SEXP dim_ = Rf_getAttrib(m, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "m must be a 2D-array, but length(dim(m)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int m__dim_1_ = m__dim_[0];
        const int m__dim_2_ = m__dim_[1];
        
        if (m__dim_1_ != 2)
          Rf_error("dim(m)[1] must be 2, not %0.f",
                    (double)m__dim_1_);
        if (m__dim_2_ != 2)
          Rf_error("dim(m)[2] must be 2, not %0.f",
                    (double)m__dim_2_);
        const R_xlen_t out___len_ = 2;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(m__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# diag() initializes integer-backed logical outputs as integers

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(2)))
          diag(x, 3L, 4L)
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
        integer(c_int), intent(in) :: x(2) ! logical
        integer(c_int), intent(out) :: out_(3, 4) ! logical
        ! manifest end
      
      
        block
          integer(c_int) :: btmp1_
      
          out_ = 0_c_int
          do btmp1_ = 1_c_int, int(3, kind=c_int)
            out_(btmp1_, btmp1_) = x(1_c_int + mod(btmp1_ - 1_c_int, int(2, kind=c_int)))
          end do
        end block
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
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 2)
          Rf_error("length(x) must be 2, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = (3) * (4);
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = 4;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(x__, out___);
        
        UNPROTECT(2);
        return out_;
      }

