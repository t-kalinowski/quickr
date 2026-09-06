# double unary intrinsics

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- sin(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = sin(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- cos(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = cos(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- tan(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = tan(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- tanh(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = tanh(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- asin(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = asin(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- acos(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = acos(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- atan(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = atan(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- sqrt(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = sqrt(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- exp(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = exp(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- log(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = log(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- log10(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = log10(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- floor(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = (aint(x) - merge(1.0_c_double, 0.0_c_double, (x < aint(x))))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- ceiling(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = (aint(x) + merge(1.0_c_double, 0.0_c_double, (x > aint(x))))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = double(NA)))
          out <- abs(x)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = abs(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# integer unary intrinsics

    Code
      fn
    Output
      function (x) 
      {
          declare(type(x = integer(NA)))
          out <- abs(x)
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
        ! manifest end
      
      
        out = abs(x)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# complex unary intrinsics

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- sin(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = sin(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- cos(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = cos(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- tan(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = tan(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- tanh(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = tanh(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- asin(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = asin(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- acos(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = acos(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- atan(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = atan(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- sqrt(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = sqrt(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- exp(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = exp(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- log(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = log(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- log10(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = (log(z) / log(10.0_c_double))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- Re(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        real(c_double), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = real(z, kind=c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        double* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- Im(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        real(c_double), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = aimag(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        double* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- Mod(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        real(c_double), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = abs(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        double* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- Arg(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double, c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        real(c_double), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = atan2(aimag(z), real(z))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        double* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function (z) 
      {
          declare(type(z = complex(NA)))
          out <- Conj(z)
          out
      }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(z, out, z__len_) bind(c)
        use iso_c_binding, only: c_double_complex, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: z__len_
      
        ! args
        complex(c_double_complex), intent(in) :: z(z__len_)
        complex(c_double_complex), intent(out) :: out(z__len_)
        ! manifest end
      
      
        out = conjg(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const Rcomplex* const z__,
        Rcomplex* const out__,
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", Rf_type2char(TYPEOF(z)));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){z__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

# Re() on a double argument keeps double precision

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          out <- Re(x)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = real(x, kind=c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
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
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# logical not local used as ifelse mask compiles and runs

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(NA)))
          y <- !x
          out <- ifelse(y, 1L, 0L)
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
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out(x__len_)
      
        ! locals
        logical, allocatable :: y(:) ! logical
        logical, allocatable :: tmp1_(:) ! logical
        integer(c_int), allocatable :: tmp2_(:)
        ! manifest end
      
        allocate(y(x__len_))
        allocate(tmp1_(x__len_))
      
      
        y = (.not. (x/=0))
        tmp1_ = y
        if (.not. allocated(tmp2_)) allocate(tmp2_(size(tmp1_, 1)))
        if (any(tmp1_)) then
          where (tmp1_) tmp2_ = 1_c_int
        end if
        if (any(.not. tmp1_)) then
          where (.not. tmp1_) tmp2_ = 0_c_int
        end if
        out = tmp2_
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      #ifndef QUICKR_RETURN_LENGTH_DEFINED
      #define QUICKR_RETURN_LENGTH_DEFINED
      static R_xlen_t quickr_return_length(const double *dims, int rank) {
        int empty = 0;
        for (int i = 0; i < rank; ++i) {
          if (!R_FINITE(dims[i]))
            Rf_error("return dimensions must be finite");
          if (dims[i] < 0)
            Rf_error("return dimensions must be non-negative");
          if (dims[i] > (rank > 1 ? 2147483647.0 : (double)R_XLEN_T_MAX))
            Rf_error("return dimensions exceed the supported range");
          if ((R_xlen_t)dims[i] == 0) empty = 1;
        }
        if (empty) return 0;
        R_xlen_t length = 1;
        for (int i = 0; i < rank; ++i) {
          R_xlen_t extent = (R_xlen_t)dims[i];
          if (extent > R_XLEN_T_MAX / length)
            Rf_error("return length exceeds R's vector limit");
          length *= extent;
        }
        return length;
      }
      #endif
      extern void fn(
        const int* const x__,
        int* const out__,
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != LGLSXP) {
          Rf_error("typeof(x) must be 'logical', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const int* const x__ = LOGICAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = quickr_return_length((const double[]){x__len_}, 1);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

