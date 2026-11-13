# codegen: fixed cbind promotes mixed inputs and broadcasts scalars

    Code
      fn
    Output
      function(A, v, s) {
          declare(type(A = double(2, 2)), type(v = integer(2)), type(s = double(1)))
          cbind(A, v, s, deparse.level = 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, v, s, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(2, 2)
        integer(c_int), intent(in) :: v(2)
        real(c_double), intent(in) :: s
        real(c_double), intent(out) :: out_(2, 4)
        ! manifest end
      
      
        out_ = reshape([A, reshape(real(v, kind=c_double), [int(2), 1]), reshape(spread(s, 1, int(2)), [int(2), 1])], [int(2), int(4)])
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const A__,
        const int* const v__,
        const double* const s__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // v
        _args = CDR(_args);
        SEXP v = CAR(_args);
        if (TYPEOF(v) != INTSXP) {
          Rf_error("typeof(v) must be 'integer', not '%s'", Rf_type2char(TYPEOF(v)));
        }
        const int* const v__ = INTEGER(v);
        const R_xlen_t v__len_ = Rf_xlength(v);
        
        // s
        _args = CDR(_args);
        SEXP s = CAR(_args);
        if (TYPEOF(s) != REALSXP) {
          Rf_error("typeof(s) must be 'double', not '%s'", Rf_type2char(TYPEOF(s)));
        }
        const double* const s__ = REAL(s);
        const R_xlen_t s__len_ = Rf_xlength(s);
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 2)
          Rf_error("dim(A)[2] must be 2, not %0.f",
                    (double)A__dim_2_);
        if (v__len_ != 2)
          Rf_error("length(v) must be 2, not %0.f",
                    (double)v__len_);
        if (s__len_ != 1)
          Rf_error("length(s) must be 1, not %0.f",
                    (double)s__len_);
        const R_xlen_t out___len_ = (2) * (4);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 4;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          A__,
          v__,
          s__,
          out___);
        
        UNPROTECT(2);
        return out_;
      }

# codegen: fixed rbind writes matrix rows and scalar fills

    Code
      fn
    Output
      function(A, v) {
          declare(type(A = double(2, 3)), type(v = double(3)))
          rbind(A, v, 1, deparse.level = 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, v, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(2, 3)
        real(c_double), intent(in) :: v(3)
        real(c_double), intent(out) :: out_(4, 3)
        ! manifest end
      
      
      out_ = transpose(reshape([transpose(A), transpose(reshape(v, [1, int(3)])), transpose(reshape(spread(1.0_c_double, 1, int(3)), [1,&
      & int(3)]))], [int(3), int(4)]))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const A__,
        const double* const v__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // v
        _args = CDR(_args);
        SEXP v = CAR(_args);
        if (TYPEOF(v) != REALSXP) {
          Rf_error("typeof(v) must be 'double', not '%s'", Rf_type2char(TYPEOF(v)));
        }
        const double* const v__ = REAL(v);
        const R_xlen_t v__len_ = Rf_xlength(v);
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 3)
          Rf_error("dim(A)[2] must be 3, not %0.f",
                    (double)A__dim_2_);
        if (v__len_ != 3)
          Rf_error("length(v) must be 3, not %0.f",
                    (double)v__len_);
        const R_xlen_t out___len_ = (4) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 4;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(A__, v__, out___);
        
        UNPROTECT(2);
        return out_;
      }

# codegen: scalar-only binds retain their matrix shapes

    Code
      fn
    Output
      function() {
          columns <- cbind(1, 2, deparse.level = 0)
          rows <- rbind(1, 2, deparse.level = 0)
          list(columns = columns, rows = rows)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(columns, rows) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: columns(1, 2)
        real(c_double), intent(out) :: rows(2, 1)
        ! manifest end
      
      columns = reshape([reshape(spread(1.0_c_double, 1, int(1)), [int(1), 1]), reshape(spread(2.0_c_double, 1, int(1)), [int(1), 1])],&
      & [int(1), int(2)])
      rows = transpose(reshape([transpose(reshape(spread(1.0_c_double, 1, int(1)), [1, int(1)])), transpose(reshape(spread(2.0_c_double,&
      & 1, int(1)), [1, int(1)]))], [int(1), int(2)]))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const columns__, double* const rows__);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t columns__len_ = (1) * (2);
        SEXP columns = PROTECT(Rf_allocVector(REALSXP, columns__len_));
        double* columns__ = REAL(columns);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 1;
          _dim[1] = 2;
          Rf_dimgets(columns, _dim_sexp);
        }
        const R_xlen_t rows__len_ = (2) * (1);
        SEXP rows = PROTECT(Rf_allocVector(REALSXP, rows__len_));
        double* rows__ = REAL(rows);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 1;
          Rf_dimgets(rows, _dim_sexp);
        }
        
        fn(columns__, rows__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, columns);
        SET_VECTOR_ELT(_ans, 1, rows);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("columns"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("rows"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(6);
        return _ans;
      }

# codegen: dynamic cbind ignores NULL and checks common lengths

    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n)), type(y = double(m)))
          cbind(NULL, x, y, 1, NULL, deparse.level = 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__len_, y__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: y__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: y(y__len_)
        real(c_double), intent(out) :: out_(x__len_, 3)
        ! manifest end
      
      
        if (size(x, kind=c_ptrdiff_t) /= size(y, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
      out_ = reshape([reshape(x, [int(x__len_), 1]), reshape(y, [int(x__len_), 1]), reshape(spread(1.0_c_double, 1, int(x__len_)),&
      & [int(x__len_), 1])], [int(x__len_), int(3)])
      
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
        const double* const x__,
        const double* const y__,
        double* const out___,
        const R_xlen_t x__len_,
        const R_xlen_t y__len_,
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        const R_xlen_t out___len_ = (x__len_) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          out___,
          x__len_,
          y__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# codegen: rbind checks independent NA column extents

    Code
      fn
    Output
      function(A, B) {
          declare(type(A = double(2, NA)), type(B = double(1, NA)))
          rbind(A, B, deparse.level = 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, B, out_, A__dim_2_, B__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_2_
        integer(c_int), intent(in), value :: B__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(2, A__dim_2_)
        real(c_double), intent(in) :: B(1, B__dim_2_)
        real(c_double), intent(out) :: out_(3, A__dim_2_)
        ! manifest end
      
      
        if (size(A, 2, kind=c_ptrdiff_t) /= size(B, 2, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("rbind() requires inputs with matching dimensions")
          return
        end if
        out_ = transpose(reshape([transpose(A), transpose(B)], [int(A__dim_2_), int(3)]))
      
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
        const double* const A__,
        const double* const B__,
        double* const out___,
        const R_len_t A__dim_2_,
        const R_len_t B__dim_2_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // B
        _args = CDR(_args);
        SEXP B = CAR(_args);
        if (TYPEOF(B) != REALSXP) {
          Rf_error("typeof(B) must be 'double', not '%s'", Rf_type2char(TYPEOF(B)));
        }
        const double* const B__ = REAL(B);
        const int* const B__dim_ = ({
        SEXP dim_ = Rf_getAttrib(B, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "B must be a 2D-array, but length(dim(B)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int B__dim_1_ = B__dim_[0];
        const int B__dim_2_ = B__dim_[1];
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (B__dim_1_ != 1)
          Rf_error("dim(B)[1] must be 1, not %0.f",
                    (double)B__dim_1_);
        const R_xlen_t out___len_ = (3) * (A__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 3;
          _dim[1] = A__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          B__,
          out___,
          A__dim_2_,
          B__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# codegen: cbind checks a dynamic extent against a fixed extent

    Code
      fn
    Output
      function(A, B) {
          declare(type(A = double(NA, 2)), type(B = double(3, 1)))
          cbind(A, B, deparse.level = 0)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, B, out_, A__dim_1_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: A__dim_1_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: A(A__dim_1_, 2)
        real(c_double), intent(in) :: B(3, 1)
        real(c_double), intent(out) :: out_(A__dim_1_, 3)
        ! manifest end
      
      
        if (size(A, 1, kind=c_ptrdiff_t) /= 3) then
          call quickr_set_error_msg("cbind() requires inputs with matching dimensions")
          return
        end if
        out_ = reshape([A, B], [int(A__dim_1_), int(3)])
      
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
        const double* const A__,
        const double* const B__,
        double* const out___,
        const R_len_t A__dim_1_,
        char* quickr_err_msg);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // B
        _args = CDR(_args);
        SEXP B = CAR(_args);
        if (TYPEOF(B) != REALSXP) {
          Rf_error("typeof(B) must be 'double', not '%s'", Rf_type2char(TYPEOF(B)));
        }
        const double* const B__ = REAL(B);
        const int* const B__dim_ = ({
        SEXP dim_ = Rf_getAttrib(B, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "B must be a 2D-array, but length(dim(B)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int B__dim_1_ = B__dim_[0];
        const int B__dim_2_ = B__dim_[1];
        
        if (A__dim_2_ != 2)
          Rf_error("dim(A)[2] must be 2, not %0.f",
                    (double)A__dim_2_);
        if (B__dim_1_ != 3)
          Rf_error("dim(B)[1] must be 3, not %0.f",
                    (double)B__dim_1_);
        if (B__dim_2_ != 1)
          Rf_error("dim(B)[2] must be 1, not %0.f",
                    (double)B__dim_2_);
        const R_xlen_t out___len_ = (A__dim_1_) * (3);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = A__dim_1_;
          _dim[1] = 3;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          A__,
          B__,
          out___,
          A__dim_1_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# codegen: input shape proofs remove elementwise error machinery

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Shared extents are checked in C; derived operands retain that proof and need no Fortran shape or nonempty guard.
    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(n, m)), type(y = double(n, m)))
          (x + 1) + (y * 2)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(in) :: y(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out_(x__dim_1_, x__dim_2_)
        ! manifest end
      
      
        out_ = (((x + 1.0_c_double)) + ((y * 2.0_c_double)))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const double* const y__,
        double* const out___,
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const int* const y__dim_ = ({
        SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "y must be a 2D-array, but length(dim(y)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int y__dim_1_ = y__dim_[0];
        const int y__dim_2_ = y__dim_[1];
        
        if (x__dim_1_ != y__dim_1_)
          Rf_error("dim(y)[1] must equal dim(x)[1],"
                   " but are %0.f and %0.f",
                    (double)y__dim_1_, (double)x__dim_1_);
        if (x__dim_2_ != y__dim_2_)
          Rf_error("dim(y)[2] must equal dim(x)[2],"
                   " but are %0.f and %0.f",
                    (double)y__dim_2_, (double)x__dim_2_);
        const R_xlen_t out___len_ = (x__dim_1_) * (x__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          x__,
          y__,
          out___,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(2);
        return out_;
      }

# codegen: independent matrix shapes retain equality guards

    Code
      cat("# Snapshot note: ", note, "\n", sep = "")
    Output
      # Snapshot note: Each stored extent is compared at runtime; matching empty axes remain valid.
    Code
      fn
    Output
      function(x, y) {
          declare(type(x = double(NA, NA)), type(y = double(NA, NA)))
          x + y
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, y, out_, x__dim_1_, x__dim_2_, y__dim_1_, y__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
        integer(c_int), intent(in), value :: y__dim_1_
        integer(c_int), intent(in), value :: y__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(in) :: y(y__dim_1_, y__dim_2_)
        real(c_double), intent(out) :: out_(x__dim_1_, x__dim_2_)
        ! manifest end
      
      
        if (size(x, 1, kind=c_ptrdiff_t) /= size(y, 1, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
          return
        end if
        if (size(x, 2, kind=c_ptrdiff_t) /= size(y, 2, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("elementwise matrix operations require matching dimensions")
          return
        end if
        out_ = (x + y)
      
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
        const double* const x__,
        const double* const y__,
        double* const out___,
        const R_len_t x__dim_1_,
        const R_len_t x__dim_2_,
        const R_len_t y__dim_1_,
        const R_len_t y__dim_2_,
        char* quickr_err_msg);
      
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
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != REALSXP) {
          Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
        }
        const double* const y__ = REAL(y);
        const int* const y__dim_ = ({
        SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "y must be a 2D-array, but length(dim(y)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int y__dim_1_ = y__dim_[0];
        const int y__dim_2_ = y__dim_[1];
        
        const R_xlen_t out___len_ = (x__dim_1_) * (x__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          y__,
          out___,
          x__dim_1_,
          x__dim_2_,
          y__dim_1_,
          y__dim_2_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# codegen: vector-matrix broadcasting uses the row extent

    Code
      fn
    Output
      function(x, A) {
          declare(type(x = double(n)), type(A = double(n, m)))
          x + A
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, A, out_, A__dim_2_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_int), intent(in), value :: A__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: A(x__len_, A__dim_2_)
        real(c_double), intent(out) :: out_(x__len_, A__dim_2_)
        ! manifest end
      
      
        out_ = (reshape(x, [int(x__len_), int(A__dim_2_)], pad = x) + A)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const double* const A__,
        double* const out___,
        const R_len_t A__dim_2_,
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
        
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        if (x__len_ != A__dim_1_)
          Rf_error("dim(A)[1] must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)A__dim_1_, (double)x__len_);
        const R_xlen_t out___len_ = (x__len_) * (A__dim_2_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__len_;
          _dim[1] = A__dim_2_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          x__,
          A__,
          out___,
          A__dim_2_,
          x__len_);
        
        UNPROTECT(2);
        return out_;
      }

# codegen: scalar broadcasts preserve empty rank-three arrays

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA, NA, NA)))
          x + 1
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__dim_1_, x__dim_2_, x__dim_3_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
        integer(c_int), intent(in), value :: x__dim_3_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_, x__dim_3_)
        real(c_double), intent(out) :: out_(x__dim_1_, x__dim_2_, x__dim_3_)
        ! manifest end
      
      
        out_ = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = (x__dim_1_) * (x__dim_2_) * (x__dim_3_);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = x__dim_1_;
          _dim[1] = x__dim_2_;
          _dim[2] = x__dim_3_;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        fn(
          x__,
          out___,
          x__dim_1_,
          x__dim_2_,
          x__dim_3_);
        
        UNPROTECT(2);
        return out_;
      }

# codegen: known empty results preserve numeric and logical modes

    Code
      fn
    Output
      function() {
          values <- numeric(0) + 1
          comparison <- 1 < numeric(0)
          list(values = values, comparison = comparison)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(values, comparison) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(out) :: values(0)
        integer(c_int), intent(out) :: comparison(0) ! logical
        ! manifest end
      
        block
          real(c_double) :: btmp1_(0)
      
          btmp1_ = 0.0_c_double
          values = (btmp1_ + 1.0_c_double)
        end block
        block
          real(c_double) :: btmp1_(0)
      
          btmp1_ = 0.0_c_double
          comparison = (1.0_c_double < btmp1_)
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(double* const values__, int* const comparison__);
      
      SEXP fn_(SEXP _args) {
        
        const R_xlen_t values__len_ = 0;
        SEXP values = PROTECT(Rf_allocVector(REALSXP, values__len_));
        double* values__ = REAL(values);
        const R_xlen_t comparison__len_ = 0;
        SEXP comparison = PROTECT(Rf_allocVector(LGLSXP, comparison__len_));
        int* comparison__ = LOGICAL(comparison);
        
        fn(values__, comparison__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, values);
        SET_VECTOR_ELT(_ans, 1, comparison);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("values"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("comparison"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(4);
        return _ans;
      }

# codegen: a known empty operand checks an independent length

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(n)))
          numeric(0) + x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(0)
        ! manifest end
      
      
        block
          real(c_double) :: btmp1_(0)
      
          btmp1_ = 0.0_c_double
          if (0 /= size(x, kind=c_ptrdiff_t)) then
      call quickr_set_error_msg("elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not&
      & supported")
            return
          end if
          out_ = (btmp1_ + x)
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
        const double* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = 0;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(
          x__,
          out___,
          x__len_,
          quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(1);
        return out_;
      }

# codegen: concatenation flattens matrix inputs column-major

    Code
      fn
    Output
      function(A, x) {
          declare(type(A = double(2, 2)), type(x = double(3)))
          c(A, x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, x, out_) bind(c)
        use iso_c_binding, only: c_double
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: A(2, 2)
        real(c_double), intent(in) :: x(3)
        real(c_double), intent(out) :: out_(7)
        ! manifest end
      
      
        out_ = [ reshape(A, [4]), x ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const A__,
        const double* const x__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != REALSXP) {
          Rf_error("typeof(A) must be 'double', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const double* const A__ = REAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 2)
          Rf_error("dim(A)[2] must be 2, not %0.f",
                    (double)A__dim_2_);
        if (x__len_ != 3)
          Rf_error("length(x) must be 3, not %0.f",
                    (double)x__len_);
        const R_xlen_t out___len_ = 7;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(A__, x__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: flattening logical storage drops dimensions

    Code
      fn
    Output
      function(A) {
          declare(type(A = logical(2, 3)))
          as.integer(A)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(A, out_) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: A(2, 3) ! logical
        integer(c_int), intent(out) :: out_(6)
        ! manifest end
      
      
        out_ = reshape(A, [6])
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(const int* const A__, int* const out___);
      
      SEXP fn_(SEXP _args) {
        // A
        _args = CDR(_args);
        SEXP A = CAR(_args);
        if (TYPEOF(A) != LGLSXP) {
          Rf_error("typeof(A) must be 'logical', not '%s'", Rf_type2char(TYPEOF(A)));
        }
        const int* const A__ = LOGICAL(A);
        const int* const A__dim_ = ({
        SEXP dim_ = Rf_getAttrib(A, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "A must be a 2D-array, but length(dim(A)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int A__dim_1_ = A__dim_[0];
        const int A__dim_2_ = A__dim_[1];
        
        if (A__dim_1_ != 2)
          Rf_error("dim(A)[1] must be 2, not %0.f",
                    (double)A__dim_1_);
        if (A__dim_2_ != 3)
          Rf_error("dim(A)[2] must be 3, not %0.f",
                    (double)A__dim_2_);
        const R_xlen_t out___len_ = 6;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(A__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: array reshaping converts and reshapes nonscalar data

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(2, 3, 4)))
          array(as.double(x), dim = c(2, 3, 4))
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        integer(c_int), intent(in) :: x(2, 3, 4)
        real(c_double), intent(out) :: out_(2, 3, 4)
        ! manifest end
      
      
      if ((int(2, kind=c_ptrdiff_t) * int(3, kind=c_ptrdiff_t) * int(4, kind=c_ptrdiff_t)) > size(reshape(real(x, kind=c_double), [24]),&
      & kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("array() reshape does not support recycling (data shorter than prod(dim))")
          return
        end if
        out_ = reshape([reshape(real(x, kind=c_double), [24])], int([2, 3, 4]))
      
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
        double* const out___,
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
        if (Rf_length(dim_) != 3) Rf_error(
          "x must be a 3D-array, but length(dim(x)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int x__dim_1_ = x__dim_[0];
        const int x__dim_2_ = x__dim_[1];
        const int x__dim_3_ = x__dim_[2];
        
        if (x__dim_1_ != 2)
          Rf_error("dim(x)[1] must be 2, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 3)
          Rf_error("dim(x)[2] must be 3, not %0.f",
                    (double)x__dim_2_);
        if (x__dim_3_ != 4)
          Rf_error("dim(x)[3] must be 4, not %0.f",
                    (double)x__dim_3_);
        const R_xlen_t out___len_ = (2) * (3) * (4);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 3));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 2;
          _dim[1] = 3;
          _dim[2] = 4;
          Rf_dimgets(out_, _dim_sexp);
        }
        
        char quickr_err_msg[256];
        quickr_err_msg[0] = '\0';
        
        
        fn(x__, out___, quickr_err_msg);
        if (quickr_err_msg[0] != '\0') {
          Rf_error("%s", quickr_err_msg);
        }
        
        UNPROTECT(2);
        return out_;
      }

# codegen: reversing an expression captures it before slicing

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          rev(x + 1)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        block
          real(c_double), allocatable :: btmp1_(:)
      
          allocate(btmp1_(x__len_))
          btmp1_ = (x + 1.0_c_double)
          out_ = btmp1_(size(btmp1_):1:-1)
        end block
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        double* const out___,
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
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: reversing logical inputs preserves integer storage

    Code
      fn
    Output
      function(x) {
          declare(type(x = logical(NA)))
          rev(x)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, out_, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_) ! logical
        integer(c_int), intent(out) :: out_(x__len_) ! logical
        ! manifest end
      
      
        out_ = x(size(x):1:-1)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const x__,
        int* const out___,
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
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(LGLSXP, out___len_));
        int* out___ = LOGICAL(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: dynamic sequences lower ascending and descending steps

    Code
      fn
    Output
      function(start, end) {
          declare(type(start = integer(1)), type(end = integer(1)))
          out <- sum(seq(start, end))
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(start, end, out) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: start
        integer(c_int), intent(in) :: end
        integer(c_int), intent(out) :: out
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out = sum([ (tmp1_, tmp1_ = start, end, sign(1, end-start)) ])
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const start__,
        const int* const end__,
        int* const out__);
      
      SEXP fn_(SEXP _args) {
        // start
        _args = CDR(_args);
        SEXP start = CAR(_args);
        if (TYPEOF(start) != INTSXP) {
          Rf_error("typeof(start) must be 'integer', not '%s'", Rf_type2char(TYPEOF(start)));
        }
        const int* const start__ = INTEGER(start);
        const R_xlen_t start__len_ = Rf_xlength(start);
        
        // end
        _args = CDR(_args);
        SEXP end = CAR(_args);
        if (TYPEOF(end) != INTSXP) {
          Rf_error("typeof(end) must be 'integer', not '%s'", Rf_type2char(TYPEOF(end)));
        }
        const int* const end__ = INTEGER(end);
        const R_xlen_t end__len_ = Rf_xlength(end);
        
        if (start__len_ != 1)
          Rf_error("length(start) must be 1, not %0.f",
                    (double)start__len_);
        if (end__len_ != 1)
          Rf_error("length(end) must be 1, not %0.f",
                    (double)end__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(start__, end__, out__);
        
        UNPROTECT(1);
        return out;
      }

# codegen: dynamic sequence subscripts form array sections

    Code
      fn
    Output
      function(x, start, end) {
          declare(
            type(x = double(5)),
            type(start = integer(1)),
            type(end = integer(1))
          )
          out <- x[seq(start, end)]
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, start, end, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(5)
        integer(c_int), intent(in) :: start
        integer(c_int), intent(in) :: end
        real(c_double), intent(out) :: out((abs((end - start)) + 1))
        ! manifest end
      
      
        out = x(start:end:sign(1, end-start))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const start__,
        const int* const end__,
        double* const out__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // start
        _args = CDR(_args);
        SEXP start = CAR(_args);
        if (TYPEOF(start) != INTSXP) {
          Rf_error("typeof(start) must be 'integer', not '%s'", Rf_type2char(TYPEOF(start)));
        }
        const int* const start__ = INTEGER(start);
        const R_xlen_t start__len_ = Rf_xlength(start);
        
        // end
        _args = CDR(_args);
        SEXP end = CAR(_args);
        if (TYPEOF(end) != INTSXP) {
          Rf_error("typeof(end) must be 'integer', not '%s'", Rf_type2char(TYPEOF(end)));
        }
        const int* const end__ = INTEGER(end);
        const R_xlen_t end__len_ = Rf_xlength(end);
        
        if (x__len_ != 5)
          Rf_error("length(x) must be 5, not %0.f",
                    (double)x__len_);
        if (start__len_ != 1)
          Rf_error("length(start) must be 1, not %0.f",
                    (double)start__len_);
        if (end__len_ != 1)
          Rf_error("length(end) must be 1, not %0.f",
                    (double)end__len_);
        const int _as_int_end = Rf_asInteger(end);
        const int _as_int_start = Rf_asInteger(start);
        const R_xlen_t out__len_ = ((((_as_int_end - _as_int_start)) < 0 ? -((_as_int_end - _as_int_start)) : ((_as_int_end - _as_int_start))) + 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          start__,
          end__,
          out__);
        
        UNPROTECT(1);
        return out;
      }

# codegen: repeated subscripts support a runtime count

    Code
      fn
    Output
      function(x, n) {
          declare(type(x = double(5)), type(n = integer(1)))
          x[rep.int(2L, n)]
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, n, out_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(5)
        integer(c_int), intent(in) :: n
        real(c_double), intent(out) :: out_(n)
      
        ! locals
        integer(c_int) :: tmp1_
        ! manifest end
      
      
        out_ = x([(2_c_int, tmp1_=1, int(n, kind=c_int))])
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const n__,
        double* const out___);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // n
        _args = CDR(_args);
        SEXP n = CAR(_args);
        if (TYPEOF(n) != INTSXP) {
          Rf_error("typeof(n) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n)));
        }
        const int* const n__ = INTEGER(n);
        const R_xlen_t n__len_ = Rf_xlength(n);
        
        if (x__len_ != 5)
          Rf_error("length(x) must be 5, not %0.f",
                    (double)x__len_);
        if (n__len_ != 1)
          Rf_error("length(n) must be 1, not %0.f",
                    (double)n__len_);
        const int _as_int_n = Rf_asInteger(n);
        const R_xlen_t out___len_ = _as_int_n;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(x__, n__, out___);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: integer vector subscripts gather values

    Code
      fn
    Output
      function(x, i) {
          declare(type(x = double(n)), type(i = integer(k)))
          x[i]
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, i, out_, i__len_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: i__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: i(i__len_)
        real(c_double), intent(out) :: out_(i__len_)
        ! manifest end
      
      
        out_ = x(i)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const i__,
        double* const out___,
        const R_xlen_t i__len_,
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
        
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        const R_xlen_t out___len_ = i__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          i__,
          out___,
          i__len_,
          x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: matrix linear subscripts follow column-major storage

    Code
      fn
    Output
      function(x, i) {
          declare(type(x = double(n, m)), type(i = integer(1)))
          x[i]
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, i, out_, x__dim_1_, x__dim_2_) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        integer(c_int), intent(in) :: i
        real(c_double), intent(out) :: out_
        ! manifest end
      
      
        out_ = x((mod((int(i, kind=c_int) - 1_c_int), size(x, 1)) + 1_c_int), (((int(i, kind=c_int) - 1_c_int) / size(x, 1)) + 1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        const int* const i__,
        double* const out___,
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
        
        // i
        _args = CDR(_args);
        SEXP i = CAR(_args);
        if (TYPEOF(i) != INTSXP) {
          Rf_error("typeof(i) must be 'integer', not '%s'", Rf_type2char(TYPEOF(i)));
        }
        const int* const i__ = INTEGER(i);
        const R_xlen_t i__len_ = Rf_xlength(i);
        
        if (i__len_ != 1)
          Rf_error("length(i) must be 1, not %0.f",
                    (double)i__len_);
        const R_xlen_t out___len_ = (1);
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          x__,
          i__,
          out___,
          x__dim_1_,
          x__dim_2_);
        
        UNPROTECT(1);
        return out_;
      }

# codegen: length-one ranges preserve or drop result rank

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(2, 3)))
          kept <- x[1:1, 1:3, drop = FALSE]
          dropped <- x[1:1, 1:3]
          list(kept = kept, dropped = dropped)
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, kept, dropped) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: x(2, 3)
        real(c_double), intent(out) :: kept(1, 3)
        real(c_double), intent(out) :: dropped(3)
        ! manifest end
      
      
        kept = x(1_c_int:1_c_int:sign(1, 1_c_int-1_c_int), 1_c_int:3_c_int:sign(1, 3_c_int-1_c_int))
        dropped = x(1_c_int, 1_c_int:3_c_int:sign(1, 3_c_int-1_c_int))
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__,
        double* const kept__,
        double* const dropped__);
      
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
        
        if (x__dim_1_ != 2)
          Rf_error("dim(x)[1] must be 2, not %0.f",
                    (double)x__dim_1_);
        if (x__dim_2_ != 3)
          Rf_error("dim(x)[2] must be 3, not %0.f",
                    (double)x__dim_2_);
        const R_xlen_t kept__len_ = (1) * (3);
        SEXP kept = PROTECT(Rf_allocVector(REALSXP, kept__len_));
        double* kept__ = REAL(kept);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = 1;
          _dim[1] = 3;
          Rf_dimgets(kept, _dim_sexp);
        }
        const R_xlen_t dropped__len_ = 3;
        SEXP dropped = PROTECT(Rf_allocVector(REALSXP, dropped__len_));
        double* dropped__ = REAL(dropped);
        
        fn(x__, kept__, dropped__);
        
        SEXP _ans = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(_ans, 0, kept);
        SET_VECTOR_ELT(_ans, 1, dropped);
        SEXP _names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(_names, 0, Rf_mkChar("kept"));
        SET_STRING_ELT(_names, 1, Rf_mkChar("dropped"));
        Rf_setAttrib(_ans, R_NamesSymbol, _names);
        UNPROTECT(5);
        return _ans;
      }

