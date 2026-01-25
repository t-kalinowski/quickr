# sapply lowers scalar-return closures (named + inline)

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Named local closure lowered to an internal subroutine using host association for captures (no capture arguments).
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- double(length(x))
          f <- function(i) x[i] * 2
          out <- sapply(seq_along(out), f)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0
      
        do tmp1_ = 1_c_int, x__len_
          call f(tmp1_, out(tmp1_))
      
        end do
      
      
        contains
          subroutine f(i, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            real(c_double), intent(out) :: res
      
      
            res = (x(i) * 2.0_c_double)
          end subroutine
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
        const R_xlen_t x__len_);
      
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
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Inline closure lowered to an internal subroutine using host association for captures (no capture arguments).
    Code
      fn
    Output
      function(x, thresh) {
          declare(type(x = double(NA)), type(thresh = double(1)))
          out <- logical(length(x))
          out <- sapply(seq_along(out), function(i) x[i] > thresh)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, thresh, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: thresh
        integer(c_int), intent(out) :: out(x__len_) ! logical
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = .false.
        do tmp1_ = 1_c_int, x__len_
          call closure1_(tmp1_, out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(i, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            integer(c_int), intent(out) :: res ! logical
      
      
            res = (x(i) > thresh)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const double* const thresh__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // thresh
        _args = CDR(_args);
        SEXP thresh = CAR(_args);
        if (TYPEOF(thresh) != REALSXP) {
          Rf_error("typeof(thresh) must be 'double', not '%s'", Rf_type2char(TYPEOF(thresh)));
        }
        const double* const thresh__ = REAL(thresh);
        const R_xlen_t thresh__len_ = Rf_xlength(thresh);
        
        if (thresh__len_ != 1)
          Rf_error("length(thresh) must be 1, not %0.f",
                    (double)thresh__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(
          x__,
          thresh__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# sapply supports integer scalar return

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Integer scalar return produces an integer vector.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- integer(length(x))
          out <- sapply(seq_along(out), function(i) i * 2L)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0
        do tmp1_ = 1_c_int, x__len_
          call closure1_(tmp1_, out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(i, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            integer(c_int), intent(out) :: res
      
      
            res = (i * 2_c_int)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
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
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# sapply supports vector return -> matrix output

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Vector return from a closure simplifies to a rank-2 output.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(m, n)))
          out <- matrix(0, nrow(x), ncol(x))
          out <- sapply(seq_len(ncol(x)), function(j) x[, j] * 2.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        do tmp1_ = 1_c_int, x__dim_2_
          call closure1_(tmp1_, out(:, tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(j, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: j
            real(c_double), intent(out) :: res(:)
      
      
            res = (x(:, j) * 2.0_c_double)
          end subroutine
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
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_);
      
      SEXP fn_(SEXP _args) {
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
        
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out;
      }

# sapply supports logical vector return -> logical matrix output

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Logical vector return simplifies to a logical matrix output.
    Code
      fn
    Output
      function(x, thresh) {
          declare(type(x = double(m, n)), type(thresh = double(1)))
          out <- matrix(FALSE, nrow(x), ncol(x))
          out <- sapply(seq_len(ncol(x)), function(j) x[, j] > thresh)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, thresh, out, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(in) :: thresh
        integer(c_int), intent(out) :: out(x__dim_1_, x__dim_2_) ! logical
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = .false.
        do tmp1_ = 1_c_int, x__dim_2_
          call closure1_(tmp1_, out(:, tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(j, res)
            use iso_c_binding, only: c_int
            implicit none
      
            integer(c_int), intent(in) :: j
            integer(c_int), intent(out) :: res(:) ! logical
      
      
            res = (x(:, j) > thresh)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const double* const thresh__, 
        int* const out__, 
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_);
      
      SEXP fn_(SEXP _args) {
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
        
        // thresh
        _args = CDR(_args);
        SEXP thresh = CAR(_args);
        if (TYPEOF(thresh) != REALSXP) {
          Rf_error("typeof(thresh) must be 'double', not '%s'", Rf_type2char(TYPEOF(thresh)));
        }
        const double* const thresh__ = REAL(thresh);
        const R_xlen_t thresh__len_ = Rf_xlength(thresh);
        
        if (thresh__len_ != 1)
          Rf_error("length(thresh) must be 1, not %0.f",
                    (double)thresh__len_);
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          thresh__,
          out__,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out;
      }

# sapply supports matrix return -> rank-3 output

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Matrix return from a closure simplifies to a rank-3 output.
    Code
      fn
    Output
      function(x, k) {
          declare(type(x = double(m, n)), type(k = integer(1)))
          out <- array(0, dim = c(nrow(x), ncol(x), k))
          out <- sapply(seq_len(k), function(t) x + as.double(t), simplify = "array")
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, k, out, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        integer(c_int), intent(in) :: k
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_, k)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        do tmp1_ = 1_c_int, k
          call closure1_(tmp1_, out(:, :, tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(t, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: t
            real(c_double), intent(out) :: res(:, :)
      
      
            res = (x + real(t, kind=c_double))
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const int* const k__, 
        double* const out__, 
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_);
      
      SEXP fn_(SEXP _args) {
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
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_) * (Rf_asInteger(k));
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          _dim[2] = Rf_asInteger(k);
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          k__,
          out__,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out;
      }

# sapply supports slice returns for higher-rank captures

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Matrix slice x[,,t] returned into a rank-3 result.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(a, b, k)))
          out <- array(0, dim = dim(x))
          out <- sapply(
            seq_len(dim(x)[3]),
            function(t) x[,, t] + 1.0,
            simplify = "array"
          )
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_, x__dim_3_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
        integer(c_int), intent(in), value :: x__dim_3_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_, x__dim_3_)
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_, x__dim_3_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        block
          integer(c_int) :: btmp1_(3)
      
          btmp1_ = shape(x)
          do tmp1_ = 1_c_int, x__dim_3_
            call closure1_(tmp1_, out(:, :, tmp1_))
      
          end do
      
        end block
      
        contains
          subroutine closure1_(t, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: t
            real(c_double), intent(out) :: res(:, :)
      
      
            res = (x(:, :, t) + 1.0_c_double)
          end subroutine
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
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_, 
        const R_len_t x__dim_3_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 3) Rf_error(
          "x must be a 3D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        const int x__dim_3_ = x__dim_[2];
        
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_) * (x__dim_3_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          _dim[2] = x__dim_3_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_,
          x__dim_3_);
        
        UNPROTECT(2);
        return out;
      }

---

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Rank-3 slice x[,,,t] returned into a rank-4 result.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(a, b, c, k)))
          out <- array(0, dim = dim(x))
          out <- sapply(
            seq_len(dim(x)[4]),
            function(t) x[,,, t] * 2.0,
            simplify = "array"
          )
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_, x__dim_3_, x__dim_4_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
        integer(c_int), intent(in), value :: x__dim_3_
        integer(c_int), intent(in), value :: x__dim_4_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_, x__dim_3_, x__dim_4_)
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_, x__dim_3_, x__dim_4_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        block
          integer(c_int) :: btmp1_(4)
      
          btmp1_ = shape(x)
          do tmp1_ = 1_c_int, x__dim_4_
            call closure1_(tmp1_, out(:, :, :, tmp1_))
      
          end do
      
        end block
      
        contains
          subroutine closure1_(t, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: t
            real(c_double), intent(out) :: res(:, :, :)
      
      
            res = (x(:, :, :, t) * 2.0_c_double)
          end subroutine
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
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_, 
        const R_len_t x__dim_3_, 
        const R_len_t x__dim_4_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 4) Rf_error(
          "x must be a 4D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        const int x__dim_3_ = x__dim_[2];
        const int x__dim_4_ = x__dim_[3];
        
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_) * (x__dim_3_) * (x__dim_4_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 4));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          _dim[2] = x__dim_3_;
          _dim[3] = x__dim_4_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_,
          x__dim_3_,
          x__dim_4_);
        
        UNPROTECT(2);
        return out;
      }

# sapply supports rank-3 return -> rank-4 output

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Rank-3 return from a closure simplifies to a rank-4 output.
    Code
      fn
    Output
      function(x, k) {
          declare(type(x = double(a, b, c)), type(k = integer(1)))
          out <- array(0, dim = c(dim(x), k))
          out <- sapply(seq_len(k), function(t) x + as.double(t), simplify = "array")
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, k, out, x__dim_1_, x__dim_2_, x__dim_3_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
        integer(c_int), intent(in), value :: x__dim_3_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_, x__dim_3_)
        integer(c_int), intent(in) :: k
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_, x__dim_3_, k)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        do tmp1_ = 1_c_int, k
          call closure1_(tmp1_, out(:, :, :, tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(t, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: t
            real(c_double), intent(out) :: res(:, :, :)
      
      
            res = (x + real(t, kind=c_double))
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const int* const k__, 
        double* const out__, 
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_, 
        const R_len_t x__dim_3_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const int* const x__dim_ = ({
        SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
        if (Rf_length(dim_) != 3) Rf_error(
          "x must be a 3D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        const int x__dim_3_ = x__dim_[2];
        
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
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_) * (x__dim_3_) * (Rf_asInteger(k));
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 4));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          _dim[2] = x__dim_3_;
          _dim[3] = Rf_asInteger(k);
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          k__,
          out__,
          x__dim_1_,
          x__dim_2_,
          x__dim_3_);
        
        UNPROTECT(2);
        return out;
      }

# closures can capture and index rank>1 arrays with x[i] linear indexing

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Linear indexing x[i] on a rank-2 capture uses computed subscripts.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(3, 4)))
          out <- double(length(x))
          out <- sapply(seq_along(out), function(i) x[i] + 1.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(3, 4)
        real(c_double), intent(out) :: out(12)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0
        do tmp1_ = 1_c_int, 12_c_int
          call closure1_(tmp1_, out(tmp1_))
      
        end do
      
      
        contains
          subroutine closure1_(i, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: i
            real(c_double), intent(out) :: res
      
      
      res = (x((mod((int(i, kind=c_int) - 1_c_int), size(x, 1)) + 1_c_int), (((int(i, kind=c_int) - 1_c_int) / size(x, 1)) + 1_c_int)) +&
          & 1.0_c_double)
          end subroutine
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const double* const x__, double* const out__);
      
      SEXP fn_(SEXP _args) {
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
        
        if (x__dim_1_ != 3)
          Rf_error("dim(x)[1] must be 3, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 4)
          Rf_error("dim(x)[2] must be 4, not %0.f",
                    (double)x__dim_2_);
        const R_xlen_t out__len_ = 12;
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__);
        
        UNPROTECT(1);
        return out;
      }

# sapply preserves R semantics when output is also captured

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: When the output is captured by the closure, quickr uses a hidden temp to avoid in-place updates during the loop.
    Code
      fn
    Output
      function(x) {
          declare(type(x = double(m, n)))
          out <- matrix(0, nrow(x), ncol(x))
          out <- x
          out <- sapply(seq_len(ncol(out)), function(j) out[, j] + 1.0)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out(x__dim_1_, x__dim_2_)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = 0.0_c_double
        out = x
        block
          real(c_double), allocatable :: btmp1_(:, :)
      
          allocate(btmp1_(x__dim_1_, x__dim_2_))
          do tmp1_ = 1_c_int, x__dim_2_
            call closure1_(tmp1_, btmp1_(:, tmp1_))
      
          end do
      
      
          out = btmp1_
        end block
      
        contains
          subroutine closure1_(j, res)
            use iso_c_binding, only: c_double, c_int
            implicit none
      
            integer(c_int), intent(in) :: j
            real(c_double), intent(out) :: res(:)
      
      
            res = (out(:, j) + 1.0_c_double)
          end subroutine
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
        const R_len_t x__dim_1_, 
        const R_len_t x__dim_2_);
      
      SEXP fn_(SEXP _args) {
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
        
        const R_xlen_t out__len_ = (x__dim_1_) * (x__dim_2_);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out, _dim_sexp);
        }
        
        fn(
          x__,
          out__,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out;
      }

