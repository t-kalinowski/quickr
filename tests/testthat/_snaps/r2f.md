# add1

    Code
      slow_add1
    Output
      function(x) {
          declare(type(x = double(NA)))
          x <- x + 1
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_add1(x, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in out) :: x(x__len_)
        ! manifest end
      
      
        x = (x + 1.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_add1(double* const x__, const R_xlen_t x__len_);
      
      SEXP slow_add1_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        
        slow_add1(x__, x__len_);
        
        return x;
      }

# add2

    Code
      slow_add2
    Output
      function(x, y) {
          declare(type(x = integer(n)), type(y = integer(n)))
          out <- x + y
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_add2(x, y, out, x__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(in) :: y(x__len_)
        integer(c_int), intent(out) :: out(x__len_)
        ! manifest end
      
      
        out = (x + y)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_add2(
        const int* const x__, 
        const int* const y__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP slow_add2_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // y
        _args = CDR(_args);
        SEXP y = CAR(_args);
        if (TYPEOF(y) != INTSXP) {
          Rf_error("typeof(y) must be 'integer', not '%s'", R_typeToChar(y));
        }
        const int* const y__ = INTEGER(y);
        const R_xlen_t y__len_ = Rf_xlength(y);
        
        if (x__len_ != y__len_)
          Rf_error("length(y) must equal length(x),"
                   " but are %0.f and %0.f",
                    (double)y__len_, (double)x__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        slow_add2(
          x__,
          y__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# convolve

    Code
      slow_convolve
    Output
      function(a, b) {
          declare(type(a = double(NA)))
          declare(type(b = double(NA)))
      
          ab <- double(length(a) + length(b) - 1)
          for (i in seq_along(a)) {
            for (j in seq_along(b)) {
              ab[i + j - 1] = ab[i + j - 1] + a[i] * b[j]
            }
          }
          ab
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine slow_convolve(a, b, ab, a__len_, b__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
        integer(c_ptrdiff_t), intent(in), value :: b__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(b__len_)
        real(c_double), intent(out) :: ab(((a__len_ + b__len_) - 1))
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: j
        ! manifest end
      
      
      
        ab = 0
        do i = 1, size(a)
          do j = 1, size(b)
            ab(((i + j) - 1_c_int)) = (ab(((i + j) - 1_c_int)) + (a(i) * b(j)))
          end do
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void slow_convolve(
        const double* const a__, 
        const double* const b__, 
        double* const ab__, 
        const R_xlen_t a__len_, 
        const R_xlen_t b__len_);
      
      SEXP slow_convolve_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        const R_xlen_t ab__len_ = ((a__len_ + b__len_) - 1);
        SEXP ab = PROTECT(Rf_allocVector(REALSXP, ab__len_));
        double* ab__ = REAL(ab);
        
        slow_convolve(
          a__,
          b__,
          ab__,
          a__len_,
          b__len_);
        
        UNPROTECT(1);
        return ab;
      }

# which.max

    Code
      r2f(fn)
    Output
      subroutine fn(a, out, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        integer(c_int), intent(out) :: out
        ! manifest end
      
      
        out = maxloc(a, 1)
      end subroutine
      
      @r: function (a)
        {
            declare(type(a = double(NA)))
            out <- which.max(a)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const double* const a__,
          int* const out__,
          const R_xlen_t a__len_);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != REALSXP) {
            Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
          }
          const double* const a__ = REAL(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          const R_xlen_t out__len_ = (1);
          SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
          int* out__ = INTEGER(out);
        
          fn(a__, out__, a__len_);
        
          UNPROTECT(1);
          return out;
        }

---

    Code
      r2f(fn)
    Output
      subroutine fn(a, out, a__len_) bind(c)
        use iso_c_binding, only: c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        integer(c_int), intent(in) :: a(a__len_) ! logical
        integer(c_int), intent(out) :: out
        ! manifest end
      
      
        out = findloc((a/=0), .true., 1)
      end subroutine
      
      @r: function (a)
        {
            declare(type(a = logical(NA)))
            out <- which.max(a)
            out
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void fn(
          const int* const a__,
          int* const out__,
          const R_xlen_t a__len_);
        
        SEXP fn_(SEXP _args) {
          // a
          _args = CDR(_args);
          SEXP a = CAR(_args);
          if (TYPEOF(a) != LGLSXP) {
            Rf_error("typeof(a) must be 'logical', not '%s'", R_typeToChar(a));
          }
          const int* const a__ = LOGICAL(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          const R_xlen_t out__len_ = (1);
          SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
          int* out__ = INTEGER(out);
        
          fn(a__, out__, a__len_);
        
          UNPROTECT(1);
          return out;
        }

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
            Rf_error("typeof(a) must be 'integer', not '%s'", R_typeToChar(a));
          }
          const int* const a__ = INTEGER(a);
          const R_xlen_t a__len_ = Rf_xlength(a);
        
          // b
          _args = CDR(_args);
          SEXP b = CAR(_args);
          if (TYPEOF(b) != INTSXP) {
            Rf_error("typeof(b) must be 'integer', not '%s'", R_typeToChar(b));
          }
          const int* const b__ = INTEGER(b);
          const R_xlen_t b__len_ = Rf_xlength(b);
        
          if (a__len_ != 1)
            Rf_error("length(a) must be 1, not %0.f",
                      (double)a__len_);
          if (b__len_ != 1)
            Rf_error("length(b) must be 1, not %0.f",
                      (double)b__len_);
          const R_xlen_t out__len_ = (Rf_asInteger(a)) * (Rf_asInteger(b));
          SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
          double* out__ = REAL(out);
          {
            const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
            int* const _dim = INTEGER(_dim_sexp);
            _dim[0] = Rf_asInteger(a);
            _dim[1] = Rf_asInteger(b);
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
            Rf_error("typeof(a1) must be 'double', not '%s'", R_typeToChar(a1));
          }
          const double* const a1__ = REAL(a1);
          const R_xlen_t a1__len_ = Rf_xlength(a1);
        
          // a2
          _args = CDR(_args);
          SEXP a2 = CAR(_args);
          if (TYPEOF(a2) != REALSXP) {
            Rf_error("typeof(a2) must be 'double', not '%s'", R_typeToChar(a2));
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
          Rf_error("typeof(a1) must be 'double', not '%s'", R_typeToChar(a1));
        }
        const double* const a1__ = REAL(a1);
        const R_xlen_t a1__len_ = Rf_xlength(a1);
        
        // a2
        _args = CDR(_args);
        SEXP a2 = CAR(_args);
        if (TYPEOF(a2) != REALSXP) {
          Rf_error("typeof(a2) must be 'double', not '%s'", R_typeToChar(a2));
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

# viterbi

    Code
      viterbi
    Output
      function(
          observations,
          states,
          initial_probs,
          transition_probs,
          emission_probs
        ) {
          declare({
            type(observations = integer(num_steps))
            type(states = integer(num_states))
            type(initial_probs = double(num_states))
            type(transition_probs = double(num_states, num_states))
            type(emission_probs = double(num_states, num_obs))
          })
      
          num_states <- length(states)
          num_steps <- length(observations)
      
          # Trellis matrices for probabilities and backtracking
          trellis <- matrix(0, nrow = length(states), ncol = length(observations))
          backpointer <- matrix(
            0L,
            nrow = length(states),
            ncol = length(observations)
          )
      
          # print(backpointer)
          # print(trellis)
          # Fortran("write (*,*) backpointer")
      
          # Initialization step
          trellis[, 1] <- initial_probs * emission_probs[, observations[1]]
      
          # print(backpointer)
          # print(trellis)
      
          # Recursion step
          for (step in 2:num_steps) {
            for (current_state in 1:num_states) {
              probabilities <- trellis[, step - 1] * transition_probs[, current_state]
              trellis[current_state, step] <- max(probabilities) *
                emission_probs[current_state, observations[step]]
              backpointer[current_state, step] <- which.max(probabilities)
            }
          }
      
          # print(backpointer)
          # print(trellis)
      
          # Backtracking to find the most likely path
          path <- integer(length(observations))
          # print(path)
          path[num_steps] <- which.max(trellis[, num_steps])
          # print(path)
          for (step in seq((num_steps - 1), 1)) {
            # print(backpointer[path[step + 1], step + 1])
            path[step] <- backpointer[path[step + 1], step + 1]
            # print(step)
          }
      
          # print(path)
      
          # print(states)
          # Return the most likely path
          out <- states[path]
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine viterbi(observations, states, initial_probs, transition_probs, emission_probs, out, emission_probs__dim_2_, &
      observations__len_, states__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: observations__len_
        integer(c_ptrdiff_t), intent(in), value :: states__len_
        integer(c_int), intent(in), value :: emission_probs__dim_2_
      
        ! args
        integer(c_int), intent(in) :: observations(observations__len_)
        integer(c_int), intent(in) :: states(states__len_)
        real(c_double), intent(in) :: initial_probs(states__len_)
        real(c_double), intent(in) :: transition_probs(states__len_, states__len_)
        real(c_double), intent(in) :: emission_probs(states__len_, emission_probs__dim_2_)
        integer(c_int), intent(out) :: out(observations__len_)
      
        ! locals
        integer(c_int) :: current_state
        integer(c_int) :: num_steps
        integer(c_int) :: step
        integer(c_int) :: backpointer(states__len_, observations__len_)
        real(c_double) :: trellis(states__len_, observations__len_)
        integer(c_int) :: num_states
        integer(c_int) :: path(observations__len_)
        real(c_double) :: probabilities(states__len_)
        ! manifest end
      
      
        num_states = size(states)
        num_steps = size(observations)
        trellis = 0.0_c_double
        backpointer = 0_c_int
        trellis(:, 1_c_int) = (initial_probs * emission_probs(:, observations(1_c_int)))
        do step = 2_c_int, num_steps, sign(1, num_steps-2_c_int)
          do current_state = 1_c_int, num_states, sign(1, num_states-1_c_int)
            probabilities = (trellis(:, (step - 1_c_int)) * transition_probs(:, current_state))
            trellis(current_state, step) = (maxval(probabilities) * emission_probs(current_state, observations(step)))
            backpointer(current_state, step) = maxloc(probabilities, 1)
          end do
        end do
        path = 0
        path(num_steps) = maxloc(trellis(:, num_steps), 1)
        do step = (num_steps - 1_c_int), 1_c_int, sign(1, 1_c_int-(num_steps - 1_c_int))
          path(step) = backpointer(path((step + 1_c_int)), (step + 1_c_int))
        end do
        out = states(path)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void viterbi(
        const int* const observations__, 
        const int* const states__, 
        const double* const initial_probs__, 
        const double* const transition_probs__, 
        const double* const emission_probs__, 
        int* const out__, 
        const R_len_t emission_probs__dim_2_, 
        const R_xlen_t observations__len_, 
        const R_xlen_t states__len_);
      
      SEXP viterbi_(SEXP _args) {
        // observations
        _args = CDR(_args);
        SEXP observations = CAR(_args);
        if (TYPEOF(observations) != INTSXP) {
          Rf_error("typeof(observations) must be 'integer', not '%s'", R_typeToChar(observations));
        }
        const int* const observations__ = INTEGER(observations);
        const R_xlen_t observations__len_ = Rf_xlength(observations);
        
        // states
        _args = CDR(_args);
        SEXP states = CAR(_args);
        if (TYPEOF(states) != INTSXP) {
          Rf_error("typeof(states) must be 'integer', not '%s'", R_typeToChar(states));
        }
        const int* const states__ = INTEGER(states);
        const R_xlen_t states__len_ = Rf_xlength(states);
        
        // initial_probs
        _args = CDR(_args);
        SEXP initial_probs = CAR(_args);
        if (TYPEOF(initial_probs) != REALSXP) {
          Rf_error("typeof(initial_probs) must be 'double', not '%s'", R_typeToChar(initial_probs));
        }
        const double* const initial_probs__ = REAL(initial_probs);
        const R_xlen_t initial_probs__len_ = Rf_xlength(initial_probs);
        
        // transition_probs
        _args = CDR(_args);
        SEXP transition_probs = CAR(_args);
        if (TYPEOF(transition_probs) != REALSXP) {
          Rf_error("typeof(transition_probs) must be 'double', not '%s'", R_typeToChar(transition_probs));
        }
        const double* const transition_probs__ = REAL(transition_probs);
        const int* const transition_probs__dim_ = ({
        SEXP dim_ = Rf_getAttrib(transition_probs, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "transition_probs must be a 2D-array, but length(dim(transition_probs)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int transition_probs__dim_1_ = transition_probs__dim_[0];
        const int transition_probs__dim_2_ = transition_probs__dim_[1];
        
        // emission_probs
        _args = CDR(_args);
        SEXP emission_probs = CAR(_args);
        if (TYPEOF(emission_probs) != REALSXP) {
          Rf_error("typeof(emission_probs) must be 'double', not '%s'", R_typeToChar(emission_probs));
        }
        const double* const emission_probs__ = REAL(emission_probs);
        const int* const emission_probs__dim_ = ({
        SEXP dim_ = Rf_getAttrib(emission_probs, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "emission_probs must be a 2D-array, but length(dim(emission_probs)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int emission_probs__dim_1_ = emission_probs__dim_[0];
        const int emission_probs__dim_2_ = emission_probs__dim_[1];
        
        if (states__len_ != initial_probs__len_)
          Rf_error("length(initial_probs) must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)initial_probs__len_, (double)states__len_);
        if (states__len_ != transition_probs__dim_1_)
          Rf_error("dim(transition_probs)[1] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)transition_probs__dim_1_, (double)states__len_);
        if (states__len_ != transition_probs__dim_2_)
          Rf_error("dim(transition_probs)[2] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)transition_probs__dim_2_, (double)states__len_);
        if (states__len_ != emission_probs__dim_1_)
          Rf_error("dim(emission_probs)[1] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)emission_probs__dim_1_, (double)states__len_);
        const R_xlen_t out__len_ = observations__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        viterbi(
          observations__,
          states__,
          initial_probs__,
          transition_probs__,
          emission_probs__,
          out__,
          emission_probs__dim_2_,
          observations__len_,
          states__len_);
        
        UNPROTECT(1);
        return out;
      }

# viterbi2

    Code
      viterbi
    Output
      function(
          observations,
          states,
          initial_probs,
          transition_probs,
          emission_probs
        ) {
          declare(
            type(observations = integer(num_steps)),
            type(states = integer(num_states)),
            type(initial_probs = double(num_states)),
            type(transition_probs = double(num_states, num_states)),
            type(emission_probs = double(num_states, num_obs)),
          )
      
          trellis <- matrix(0, nrow = length(states), ncol = length(observations))
          backpointer <- matrix(
            0L,
            nrow = length(states),
            ncol = length(observations)
          )
          trellis[, 1] <- initial_probs * emission_probs[, observations[1]]
      
          for (step in 2:length(observations)) {
            for (current_state in 1:length(states)) {
              probabilities <- trellis[, step - 1] * transition_probs[, current_state]
              trellis[current_state, step] <- max(probabilities) *
                emission_probs[current_state, observations[step]]
              backpointer[current_state, step] <- which.max(probabilities)
            }
          }
      
          path <- integer(length(observations))
          path[length(observations)] <- which.max(trellis[, length(observations)])
          for (step in seq(length(observations) - 1, 1)) {
            path[step] <- backpointer[path[step + 1], step + 1]
          }
      
          out <- states[path]
          out
        }
      <environment: 0x0>
    Code
      cat(fsub <- r2f(viterbi))
    Output
      subroutine viterbi(observations, states, initial_probs, transition_probs, emission_probs, out, emission_probs__dim_2_, &
      observations__len_, states__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: observations__len_
        integer(c_ptrdiff_t), intent(in), value :: states__len_
        integer(c_int), intent(in), value :: emission_probs__dim_2_
      
        ! args
        integer(c_int), intent(in) :: observations(observations__len_)
        integer(c_int), intent(in) :: states(states__len_)
        real(c_double), intent(in) :: initial_probs(states__len_)
        real(c_double), intent(in) :: transition_probs(states__len_, states__len_)
        real(c_double), intent(in) :: emission_probs(states__len_, emission_probs__dim_2_)
        integer(c_int), intent(out) :: out(observations__len_)
      
        ! locals
        integer(c_int) :: current_state
        integer(c_int) :: step
        integer(c_int) :: backpointer(states__len_, observations__len_)
        real(c_double) :: trellis(states__len_, observations__len_)
        integer(c_int) :: path(observations__len_)
        real(c_double) :: probabilities(states__len_)
        ! manifest end
      
      
        trellis = 0.0_c_double
        backpointer = 0_c_int
        trellis(:, 1_c_int) = (initial_probs * emission_probs(:, observations(1_c_int)))
        do step = 2_c_int, size(observations), sign(1, size(observations)-2_c_int)
          do current_state = 1_c_int, size(states), sign(1, size(states)-1_c_int)
            probabilities = (trellis(:, (step - 1_c_int)) * transition_probs(:, current_state))
            trellis(current_state, step) = (maxval(probabilities) * emission_probs(current_state, observations(step)))
            backpointer(current_state, step) = maxloc(probabilities, 1)
          end do
        end do
        path = 0
        path(size(observations)) = maxloc(trellis(:, size(observations)), 1)
        do step = (size(observations) - 1_c_int), 1_c_int, sign(1, 1_c_int-(size(observations) - 1_c_int))
          path(step) = backpointer(path((step + 1_c_int)), (step + 1_c_int))
        end do
        out = states(path)
      end subroutine
    Code
      cat(make_c_bridge(fsub))
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void viterbi(
        const int* const observations__, 
        const int* const states__, 
        const double* const initial_probs__, 
        const double* const transition_probs__, 
        const double* const emission_probs__, 
        int* const out__, 
        const R_len_t emission_probs__dim_2_, 
        const R_xlen_t observations__len_, 
        const R_xlen_t states__len_);
      
      SEXP viterbi_(SEXP _args) {
        // observations
        _args = CDR(_args);
        SEXP observations = CAR(_args);
        if (TYPEOF(observations) != INTSXP) {
          Rf_error("typeof(observations) must be 'integer', not '%s'", R_typeToChar(observations));
        }
        const int* const observations__ = INTEGER(observations);
        const R_xlen_t observations__len_ = Rf_xlength(observations);
        
        // states
        _args = CDR(_args);
        SEXP states = CAR(_args);
        if (TYPEOF(states) != INTSXP) {
          Rf_error("typeof(states) must be 'integer', not '%s'", R_typeToChar(states));
        }
        const int* const states__ = INTEGER(states);
        const R_xlen_t states__len_ = Rf_xlength(states);
        
        // initial_probs
        _args = CDR(_args);
        SEXP initial_probs = CAR(_args);
        if (TYPEOF(initial_probs) != REALSXP) {
          Rf_error("typeof(initial_probs) must be 'double', not '%s'", R_typeToChar(initial_probs));
        }
        const double* const initial_probs__ = REAL(initial_probs);
        const R_xlen_t initial_probs__len_ = Rf_xlength(initial_probs);
        
        // transition_probs
        _args = CDR(_args);
        SEXP transition_probs = CAR(_args);
        if (TYPEOF(transition_probs) != REALSXP) {
          Rf_error("typeof(transition_probs) must be 'double', not '%s'", R_typeToChar(transition_probs));
        }
        const double* const transition_probs__ = REAL(transition_probs);
        const int* const transition_probs__dim_ = ({
        SEXP dim_ = Rf_getAttrib(transition_probs, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "transition_probs must be a 2D-array, but length(dim(transition_probs)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int transition_probs__dim_1_ = transition_probs__dim_[0];
        const int transition_probs__dim_2_ = transition_probs__dim_[1];
        
        // emission_probs
        _args = CDR(_args);
        SEXP emission_probs = CAR(_args);
        if (TYPEOF(emission_probs) != REALSXP) {
          Rf_error("typeof(emission_probs) must be 'double', not '%s'", R_typeToChar(emission_probs));
        }
        const double* const emission_probs__ = REAL(emission_probs);
        const int* const emission_probs__dim_ = ({
        SEXP dim_ = Rf_getAttrib(emission_probs, R_DimSymbol);
        if (Rf_length(dim_) != 2) Rf_error(
          "emission_probs must be a 2D-array, but length(dim(emission_probs)) is %i",
          (int) Rf_length(dim_));
        INTEGER(dim_);});
        const int emission_probs__dim_1_ = emission_probs__dim_[0];
        const int emission_probs__dim_2_ = emission_probs__dim_[1];
        
        if (states__len_ != initial_probs__len_)
          Rf_error("length(initial_probs) must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)initial_probs__len_, (double)states__len_);
        if (states__len_ != transition_probs__dim_1_)
          Rf_error("dim(transition_probs)[1] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)transition_probs__dim_1_, (double)states__len_);
        if (states__len_ != transition_probs__dim_2_)
          Rf_error("dim(transition_probs)[2] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)transition_probs__dim_2_, (double)states__len_);
        if (states__len_ != emission_probs__dim_1_)
          Rf_error("dim(emission_probs)[1] must equal length(states),"
                   " but are %0.f and %0.f",
                    (double)emission_probs__dim_1_, (double)states__len_);
        const R_xlen_t out__len_ = observations__len_;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        viterbi(
          observations__,
          states__,
          initial_probs__,
          transition_probs__,
          emission_probs__,
          out__,
          emission_probs__dim_2_,
          observations__len_,
          states__len_);
        
        UNPROTECT(1);
        return out;
      }

# heat diffusion

    Code
      diffuse_heat
    Output
      function(nx, ny, dx, dy, dt, k, steps) {
          declare(
            type(nx = integer(1)),
            type(ny = integer(1)),
            type(dx = integer(1)),
            type(dy = integer(1)),
            type(dt = double(1)),
            type(k = double(1)),
            type(steps = integer(1))
          )
      
          # Initialize temperature grid
          temp <- matrix(0, nx, ny)
          temp[nx / 2, ny / 2] <- 100 # Initial heat source in the center
      
          # Boundary conditions
          # apply_boundary_conditions <- function(temp) {
          #   temp[1, ] <- 0
          #   temp[nx, ] <- 0
          #   temp[, 1] <- 0
          #   temp[, ny] <- 0
          #   temp
          # }
          #
          # # Update step using finite differences
          # update_temperature <- function(temp, k, dx, dy, dt) {
          #   temp_new <- temp
          #   for (i in 2:(nx - 1)) {
          #     for (j in 2:(ny - 1)) {
          #       temp_new[i, j] <- temp[i, j] + k * dt * ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) / dx ^
          #                                                  2 +
          #                                                  (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2)
          #     }
          #   }
          #   temp_new
          # }
      
          # Time stepping
          for (step in seq_len(steps)) {
            # temp <- apply_boundary_conditions(temp)
            # temp <- update_temperature(temp, k, dx, dy, dt)
      
            temp[1, ] <- 0
            temp[nx, ] <- 0
            temp[, 1] <- 0
            temp[, ny] <- 0
      
            temp_new <- temp
            for (i in 2:(nx - 1)) {
              for (j in 2:(ny - 1)) {
                temp_new[i, j] <- temp[i, j] +
                  k *
                    dt *
                    ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) /
                      dx^2 +
                      (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy^2)
              }
            }
            temp <- temp_new
          }
      
          temp
      
          # Plot the final temperature distribution
          # image(temp, col = heat.colors(100), main = "Heat Diffusion")
        }
      <environment: 0x0>
    Code
      cat(fsub <- r2f(diffuse_heat))
    Output
      subroutine diffuse_heat(nx, ny, dx, dy, dt, k, steps, temp) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in) :: nx
        integer(c_int), intent(in) :: ny
        integer(c_int), intent(in) :: dx
        integer(c_int), intent(in) :: dy
        real(c_double), intent(in) :: dt
        real(c_double), intent(in) :: k
        integer(c_int), intent(in) :: steps
        real(c_double), intent(out) :: temp(nx, ny)
      
        ! locals
        real(c_double) :: temp_new(nx, ny)
        integer(c_int) :: step
        integer(c_int) :: i
        integer(c_int) :: j
        ! manifest end
      
      
        temp = 0.0_c_double
      temp(int((real(nx, kind=c_double) / real(2_c_int, kind=c_double)), kind=c_ptrdiff_t), int((real(ny, kind=c_double) / real(2_c_int, &
      kind=c_double)), kind=c_ptrdiff_t)) = 100.0_c_double
        do step = 1, steps
          temp(1_c_int, :) = 0.0_c_double
          temp(nx, :) = 0.0_c_double
          temp(:, 1_c_int) = 0.0_c_double
          temp(:, ny) = 0.0_c_double
          temp_new = temp
          do i = 2_c_int, (nx - 1_c_int), sign(1, (nx - 1_c_int)-2_c_int)
            do j = 2_c_int, (ny - 1_c_int), sign(1, (ny - 1_c_int)-2_c_int)
      temp_new(i, j) = (temp(i, j) + ((k * dt) * ((((temp((i + 1_c_int), j) - (2.0_c_double * temp(i, j))) + temp((i - 1_c_int), j)) / &
      (dx ** 2.0_c_double)) + (((temp(i, (j + 1_c_int)) - (2.0_c_double * temp(i, j))) + temp(i, (j - 1_c_int))) / (dy ** &
      2.0_c_double)))))
            end do
          end do
          temp = temp_new
        end do
      end subroutine
    Code
      cat(make_c_bridge(fsub))
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void diffuse_heat(
        const int* const nx__, 
        const int* const ny__, 
        const int* const dx__, 
        const int* const dy__, 
        const double* const dt__, 
        const double* const k__, 
        const int* const steps__, 
        double* const temp__);
      
      SEXP diffuse_heat_(SEXP _args) {
        // nx
        _args = CDR(_args);
        SEXP nx = CAR(_args);
        if (TYPEOF(nx) != INTSXP) {
          Rf_error("typeof(nx) must be 'integer', not '%s'", R_typeToChar(nx));
        }
        const int* const nx__ = INTEGER(nx);
        const R_xlen_t nx__len_ = Rf_xlength(nx);
        
        // ny
        _args = CDR(_args);
        SEXP ny = CAR(_args);
        if (TYPEOF(ny) != INTSXP) {
          Rf_error("typeof(ny) must be 'integer', not '%s'", R_typeToChar(ny));
        }
        const int* const ny__ = INTEGER(ny);
        const R_xlen_t ny__len_ = Rf_xlength(ny);
        
        // dx
        _args = CDR(_args);
        SEXP dx = CAR(_args);
        if (TYPEOF(dx) != INTSXP) {
          Rf_error("typeof(dx) must be 'integer', not '%s'", R_typeToChar(dx));
        }
        const int* const dx__ = INTEGER(dx);
        const R_xlen_t dx__len_ = Rf_xlength(dx);
        
        // dy
        _args = CDR(_args);
        SEXP dy = CAR(_args);
        if (TYPEOF(dy) != INTSXP) {
          Rf_error("typeof(dy) must be 'integer', not '%s'", R_typeToChar(dy));
        }
        const int* const dy__ = INTEGER(dy);
        const R_xlen_t dy__len_ = Rf_xlength(dy);
        
        // dt
        _args = CDR(_args);
        SEXP dt = CAR(_args);
        if (TYPEOF(dt) != REALSXP) {
          Rf_error("typeof(dt) must be 'double', not '%s'", R_typeToChar(dt));
        }
        const double* const dt__ = REAL(dt);
        const R_xlen_t dt__len_ = Rf_xlength(dt);
        
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != REALSXP) {
          Rf_error("typeof(k) must be 'double', not '%s'", R_typeToChar(k));
        }
        const double* const k__ = REAL(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        // steps
        _args = CDR(_args);
        SEXP steps = CAR(_args);
        if (TYPEOF(steps) != INTSXP) {
          Rf_error("typeof(steps) must be 'integer', not '%s'", R_typeToChar(steps));
        }
        const int* const steps__ = INTEGER(steps);
        const R_xlen_t steps__len_ = Rf_xlength(steps);
        
        if (nx__len_ != 1)
          Rf_error("length(nx) must be 1, not %0.f",
                    (double)nx__len_);
        if (ny__len_ != 1)
          Rf_error("length(ny) must be 1, not %0.f",
                    (double)ny__len_);
        if (dx__len_ != 1)
          Rf_error("length(dx) must be 1, not %0.f",
                    (double)dx__len_);
        if (dy__len_ != 1)
          Rf_error("length(dy) must be 1, not %0.f",
                    (double)dy__len_);
        if (dt__len_ != 1)
          Rf_error("length(dt) must be 1, not %0.f",
                    (double)dt__len_);
        if (k__len_ != 1)
          Rf_error("length(k) must be 1, not %0.f",
                    (double)k__len_);
        if (steps__len_ != 1)
          Rf_error("length(steps) must be 1, not %0.f",
                    (double)steps__len_);
        const R_xlen_t temp__len_ = (Rf_asInteger(nx)) * (Rf_asInteger(ny));
        SEXP temp = PROTECT(Rf_allocVector(REALSXP, temp__len_));
        double* temp__ = REAL(temp);
        {
          const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
          int* const _dim = INTEGER(_dim_sexp);
          _dim[0] = Rf_asInteger(nx);
          _dim[1] = Rf_asInteger(ny);
          Rf_dimgets(temp, _dim_sexp);
        }
        
        diffuse_heat(
          nx__,
          ny__,
          dx__,
          dy__,
          dt__,
          k__,
          steps__,
          temp__);
        
        UNPROTECT(2);
        return temp;
      }

# hoist mask

    Code
      fn
    Output
      function(x) {
          declare(type(x = double(NA)))
          out <- max(x)
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
        real(c_double), intent(out) :: out
        ! manifest end
      
      
        out = maxval(x)
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
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = (1);
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
      function(x) {
          declare(type(x = double(NA)))
          out <- max(x[x >= 0])
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
        real(c_double), intent(out) :: out
        ! manifest end
      
      
        out = maxval(x, mask = (x >= 0_c_int))
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
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(x__, out__, x__len_);
        
        UNPROTECT(1);
        return out;
      }

# which.max/which.min

    Code
      fn
    Output
      function(lgl1, int1, dbl1) {
          declare(type(lgl1 = logical(NA)))
          declare(type(int1 = integer(NA)))
          declare(type(dbl1 = double(NA)))
          out <- c(
            which.min(lgl1),
            which.min(int1),
            which.min(dbl1),
            which.max(lgl1),
            which.max(int1),
            which.max(dbl1),
            which.max(dbl1[dbl1 < 0])
          )
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(lgl1, int1, dbl1, out, dbl1__len_, int1__len_, lgl1__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: lgl1__len_
        integer(c_ptrdiff_t), intent(in), value :: int1__len_
        integer(c_ptrdiff_t), intent(in), value :: dbl1__len_
      
        ! args
        integer(c_int), intent(in) :: lgl1(lgl1__len_) ! logical
        integer(c_int), intent(in) :: int1(int1__len_)
        real(c_double), intent(in) :: dbl1(dbl1__len_)
        integer(c_int), intent(out) :: out(7)
        ! manifest end
      
      
      
      
      out = [ findloc((lgl1/=0), .false., 1), minloc(int1, 1), minloc(dbl1, 1), findloc((lgl1/=0), .true., 1), maxloc(int1, 1), &
      maxloc(dbl1, 1), maxloc(pack(dbl1, (dbl1 < 0_c_int)), 1) ]
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const int* const lgl1__, 
        const int* const int1__, 
        const double* const dbl1__, 
        int* const out__, 
        const R_xlen_t dbl1__len_, 
        const R_xlen_t int1__len_, 
        const R_xlen_t lgl1__len_);
      
      SEXP fn_(SEXP _args) {
        // lgl1
        _args = CDR(_args);
        SEXP lgl1 = CAR(_args);
        if (TYPEOF(lgl1) != LGLSXP) {
          Rf_error("typeof(lgl1) must be 'logical', not '%s'", R_typeToChar(lgl1));
        }
        const int* const lgl1__ = LOGICAL(lgl1);
        const R_xlen_t lgl1__len_ = Rf_xlength(lgl1);
        
        // int1
        _args = CDR(_args);
        SEXP int1 = CAR(_args);
        if (TYPEOF(int1) != INTSXP) {
          Rf_error("typeof(int1) must be 'integer', not '%s'", R_typeToChar(int1));
        }
        const int* const int1__ = INTEGER(int1);
        const R_xlen_t int1__len_ = Rf_xlength(int1);
        
        // dbl1
        _args = CDR(_args);
        SEXP dbl1 = CAR(_args);
        if (TYPEOF(dbl1) != REALSXP) {
          Rf_error("typeof(dbl1) must be 'double', not '%s'", R_typeToChar(dbl1));
        }
        const double* const dbl1__ = REAL(dbl1);
        const R_xlen_t dbl1__len_ = Rf_xlength(dbl1);
        
        const R_xlen_t out__len_ = 7;
        SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
        int* out__ = INTEGER(out);
        
        fn(
          lgl1__,
          int1__,
          dbl1__,
          out__,
          dbl1__len_,
          int1__len_,
          lgl1__len_);
        
        UNPROTECT(1);
        return out;
      }

# roll_mean

    Code
      fn
    Output
      function(x, weights, normalize = TRUE) {
          declare(
            type(x = double(NA)),
            type(weights = double(NA)),
            type(normalize = logical(1))
          )
          out <- double(length(x) - length(weights) + 1)
          n <- length(weights)
          if (normalize) {
            weights <- weights / sum(weights) * length(weights)
          }
      
          for (i in seq_along(out)) {
            out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
          }
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, weights, normalize, out, weights__len_, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
        integer(c_ptrdiff_t), intent(in), value :: weights__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in out) :: weights(weights__len_)
        integer(c_int), intent(in) :: normalize ! logical
        real(c_double), intent(out) :: out(((x__len_ - weights__len_) + 1))
      
        ! locals
        integer(c_int) :: i
        integer(c_int) :: n
        ! manifest end
      
      
        out = 0
        n = size(weights)
        if ((normalize/=0)) then
          weights = ((weights / sum(weights)) * size(weights))
        end if
        do i = 1, size(out)
          out(i) = (sum((x(i:((i + n) - 1_c_int):sign(1, ((i + n) - 1_c_int)-i)) * weights)) / real(size(weights), kind=c_double))
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        double* const weights__, 
        const int* const normalize__, 
        double* const out__, 
        const R_xlen_t weights__len_, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // weights
        _args = CDR(_args);
        SEXP weights = CAR(_args);
        if (TYPEOF(weights) != REALSXP) {
          Rf_error("typeof(weights) must be 'double', not '%s'", R_typeToChar(weights));
        }
        weights = Rf_duplicate(weights);
        SETCAR(_args, weights);
        double* const weights__ = REAL(weights);
        const R_xlen_t weights__len_ = Rf_xlength(weights);
        
        // normalize
        _args = CDR(_args);
        SEXP normalize = CAR(_args);
        if (TYPEOF(normalize) != LGLSXP) {
          Rf_error("typeof(normalize) must be 'logical', not '%s'", R_typeToChar(normalize));
        }
        const int* const normalize__ = LOGICAL(normalize);
        const R_xlen_t normalize__len_ = Rf_xlength(normalize);
        
        if (normalize__len_ != 1)
          Rf_error("length(normalize) must be 1, not %0.f",
                    (double)normalize__len_);
        const R_xlen_t out__len_ = ((x__len_ - weights__len_) + 1);
        SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
        double* out__ = REAL(out);
        
        fn(
          x__,
          weights__,
          normalize__,
          out__,
          weights__len_,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# between

    Code
      fn
    Output
      function(x, left, right) {
          declare({
            type(x = double(n))
            type(left = double(1))
            type(right = double(1))
          })
          out <- x >= left & x <= right
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x, left, right, out, x__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: x__len_
      
        ! args
        real(c_double), intent(in) :: x(x__len_)
        real(c_double), intent(in) :: left
        real(c_double), intent(in) :: right
        integer(c_int), intent(out) :: out(x__len_) ! logical
        ! manifest end
      
      
        out = (x >= left) .and. (x <= right)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const x__, 
        const double* const left__, 
        const double* const right__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
        }
        const double* const x__ = REAL(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        // left
        _args = CDR(_args);
        SEXP left = CAR(_args);
        if (TYPEOF(left) != REALSXP) {
          Rf_error("typeof(left) must be 'double', not '%s'", R_typeToChar(left));
        }
        const double* const left__ = REAL(left);
        const R_xlen_t left__len_ = Rf_xlength(left);
        
        // right
        _args = CDR(_args);
        SEXP right = CAR(_args);
        if (TYPEOF(right) != REALSXP) {
          Rf_error("typeof(right) must be 'double', not '%s'", R_typeToChar(right));
        }
        const double* const right__ = REAL(right);
        const R_xlen_t right__len_ = Rf_xlength(right);
        
        if (left__len_ != 1)
          Rf_error("length(left) must be 1, not %0.f",
                    (double)left__len_);
        if (right__len_ != 1)
          Rf_error("length(right) must be 1, not %0.f",
                    (double)right__len_);
        const R_xlen_t out__len_ = x__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(
          x__,
          left__,
          right__,
          out__,
          x__len_);
        
        UNPROTECT(1);
        return out;
      }

# size constraint

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n + 1)))
          a = sum(b)
          a
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in out) :: a(a__len_)
        real(c_double), intent(in) :: b((a__len_ + 1))
        ! manifest end
      
      
        a = sum(b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        double* const a__, 
        const double* const b__, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        a = Rf_duplicate(a);
        SETCAR(_args, a);
        double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        {
          const R_xlen_t expected = (a__len_ + 1);
          if (b__len_ != expected)
            Rf_error("length(b) must equal (length(a) + 1),"
                     " but are %0.f and %0.f",
                      (double)b__len_, (double)expected);
        }
        
        fn(a__, b__, a__len_);
        
        return a;
      }

# logical ops

    Code
      fn
    Output
      function(a, b) {
          declare(
            type(a = double(1)),
            type(b = double(1))
          )
      
          delta <- a - b
          if (delta < 0) {
            delta <- (-1) * delta
          }
      
          a_gt_b <- a > b
          b_gt_a <- b > a
          delta_lt_3 <- delta <= 3
      
          out <- (a_gt_b || b_gt_a) && delta_lt_3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
      
        ! locals
        logical :: b_gt_a ! logical
        real(c_double) :: delta
        logical :: a_gt_b ! logical
        logical :: delta_lt_3 ! logical
        ! manifest end
      
      
        delta = (a - b)
        if ((delta < 0.0_c_double)) then
          delta = (-1.0_c_double * delta)
        end if
        a_gt_b = (a > b)
        b_gt_a = (b > a)
        delta_lt_3 = (delta <= 3.0_c_double)
        out = a_gt_b .or. b_gt_a .and. delta_lt_3
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        int* const out__);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare({
            type(a = double(1))
            type(b = double(1))
          })
      
          delta <- abs(a - b)
          out <- (a != b) & (delta <= 3)
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
      
        ! locals
        real(c_double) :: delta
        ! manifest end
      
      
        delta = abs((a - b))
        out = (a /= b) .and. (delta <= 3.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        int* const out__);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(1)), type(b = double(1)))
          out <- (a != b) && abs(a - b) <= 3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out) bind(c)
        use iso_c_binding, only: c_double, c_int
        implicit none
      
        ! manifest start
        ! args
        real(c_double), intent(in) :: a
        real(c_double), intent(in) :: b
        integer(c_int), intent(out) :: out ! logical
        ! manifest end
      
      
        out = (a /= b) .and. (abs((a - b)) <= 3.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        int* const out__);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != 1)
          Rf_error("length(a) must be 1, not %0.f",
                    (double)a__len_);
        if (b__len_ != 1)
          Rf_error("length(b) must be 1, not %0.f",
                    (double)b__len_);
        const R_xlen_t out__len_ = (1);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(a__, b__, out__);
        
        UNPROTECT(1);
        return out;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          out <- (a != b) & abs(a - b) <= 3
          out
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        integer(c_int), intent(out) :: out(a__len_) ! logical
        ! manifest end
      
      
        out = (a /= b) .and. (abs((a - b)) <= 3.0_c_double)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        int* const out__, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out__len_ = a__len_;
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, out__len_));
        int* out__ = LOGICAL(out);
        
        fn(
          a__,
          b__,
          out__,
          a__len_);
        
        UNPROTECT(1);
        return out;
      }

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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
        out = floor(x)
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
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
        out = ceiling(x)
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
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const double* const x__, 
        double* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != REALSXP) {
          Rf_error("typeof(x) must be 'double', not '%s'", R_typeToChar(x));
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
      
      
      extern void fn(
        const int* const x__, 
        int* const out__, 
        const R_xlen_t x__len_);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out__len_ = x__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
        out = real(z)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const Rcomplex* const z__, 
        double* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        double* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        double* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        double* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
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
      
      
      extern void fn(
        const Rcomplex* const z__, 
        Rcomplex* const out__, 
        const R_xlen_t z__len_);
      
      SEXP fn_(SEXP _args) {
        // z
        _args = CDR(_args);
        SEXP z = CAR(_args);
        if (TYPEOF(z) != CPLXSXP) {
          Rf_error("typeof(z) must be 'complex', not '%s'", R_typeToChar(z));
        }
        const Rcomplex* const z__ = COMPLEX(z);
        const R_xlen_t z__len_ = Rf_xlength(z);
        
        const R_xlen_t out__len_ = z__len_;
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, out__len_));
        Rcomplex* out__ = COMPLEX(out);
        
        fn(z__, out__, z__len_);
        
        UNPROTECT(1);
        return out;
      }

# repeat/break

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          repeat {
            if (x >= 5L) {
              break
            }
            x <- x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do
          if ((x >= 5_c_int)) then
            exit
          end if
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# repeat + next

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          repeat {
            x <- x + 1L
            if (x < 0L) {
              next
            }
            if (x >= 5L) break
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do
          x = (x + 1_c_int)
          if ((x < 0_c_int)) then
            cycle
          end if
          if ((x >= 5_c_int)) then
            exit
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# break/for

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          for (i in 1:10) {
            x = x + 1L
            if (x >= 5L) {
              break
            }
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
      
        ! locals
        integer(c_int) :: i
        ! manifest end
      
      
        do i = 1_c_int, 10_c_int, sign(1, 10_c_int-1_c_int)
          x = (x + 1_c_int)
          if ((x >= 5_c_int)) then
            exit
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (x < 5L) {
            x = x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while ((x < 5_c_int))
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while + next

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (x < 5L) {
            x <- x + 1L
            if (x < 0L) next
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while ((x < 5_c_int))
          x = (x + 1_c_int)
          if ((x < 0_c_int)) then
            cycle
          end if
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# while + break

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(1)))
          while (TRUE) {
            if (x >= 5L) {
              break
            }
            x <- x + 1L
          }
          x
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(x) bind(c)
        use iso_c_binding, only: c_int
        implicit none
      
        ! manifest start
        ! args
        integer(c_int), intent(in out) :: x
        ! manifest end
      
      
        do while (.true.)
          if ((x >= 5_c_int)) then
            exit
          end if
          x = (x + 1_c_int)
        end do
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(int* const x__);
      
      SEXP fn_(SEXP _args) {
        // x
        _args = CDR(_args);
        SEXP x = CAR(_args);
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        x = Rf_duplicate(x);
        SETCAR(_args, x);
        int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        if (x__len_ != 1)
          Rf_error("length(x) must be 1, not %0.f",
                    (double)x__len_);
        
        fn(x__);
        
        return x;
      }

# expr return value

    Code
      fn
    Output
      function(x) {
          declare(type(x = integer(NA)))
          x + 1L
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
        integer(c_int), intent(in) :: x(x__len_)
        integer(c_int), intent(out) :: out_(x__len_)
        ! manifest end
      
      
        out_ = (x + 1_c_int)
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
        if (TYPEOF(x) != INTSXP) {
          Rf_error("typeof(x) must be 'integer', not '%s'", R_typeToChar(x));
        }
        const int* const x__ = INTEGER(x);
        const R_xlen_t x__len_ = Rf_xlength(x);
        
        const R_xlen_t out___len_ = x__len_;
        SEXP out_ = PROTECT(Rf_allocVector(INTSXP, out___len_));
        int* out___ = INTEGER(out_);
        
        fn(x__, out___, x__len_);
        
        UNPROTECT(1);
        return out_;
      }

# %% and %/%

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          a %% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = modulo(a, b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

---

    Code
      fn
    Output
      function(a, b) {
          declare(type(a = double(n)), type(b = double(n)))
          a %/% b
        }
      <environment: 0x0>
    Code
      cat(fsub)
    Output
      subroutine fn(a, b, out_, a__len_) bind(c)
        use iso_c_binding, only: c_double, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_ptrdiff_t), intent(in), value :: a__len_
      
        ! args
        real(c_double), intent(in) :: a(a__len_)
        real(c_double), intent(in) :: b(a__len_)
        real(c_double), intent(out) :: out_(a__len_)
        ! manifest end
      
      
        out_ = floor(a / b)
      end subroutine
    Code
      cat(cwrapper)
    Output
      #define R_NO_REMAP
      #include <R.h>
      #include <Rinternals.h>
      
      
      extern void fn(
        const double* const a__, 
        const double* const b__, 
        double* const out___, 
        const R_xlen_t a__len_);
      
      SEXP fn_(SEXP _args) {
        // a
        _args = CDR(_args);
        SEXP a = CAR(_args);
        if (TYPEOF(a) != REALSXP) {
          Rf_error("typeof(a) must be 'double', not '%s'", R_typeToChar(a));
        }
        const double* const a__ = REAL(a);
        const R_xlen_t a__len_ = Rf_xlength(a);
        
        // b
        _args = CDR(_args);
        SEXP b = CAR(_args);
        if (TYPEOF(b) != REALSXP) {
          Rf_error("typeof(b) must be 'double', not '%s'", R_typeToChar(b));
        }
        const double* const b__ = REAL(b);
        const R_xlen_t b__len_ = Rf_xlength(b);
        
        if (a__len_ != b__len_)
          Rf_error("length(b) must equal length(a),"
                   " but are %0.f and %0.f",
                    (double)b__len_, (double)a__len_);
        const R_xlen_t out___len_ = a__len_;
        SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
        double* out___ = REAL(out_);
        
        fn(
          a__,
          b__,
          out___,
          a__len_);
        
        UNPROTECT(1);
        return out_;
      }

