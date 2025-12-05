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
          Rf_error("typeof(observations) must be 'integer', not '%s'", Rf_type2char(TYPEOF(observations)));
        }
        const int* const observations__ = INTEGER(observations);
        const R_xlen_t observations__len_ = Rf_xlength(observations);
        
        // states
        _args = CDR(_args);
        SEXP states = CAR(_args);
        if (TYPEOF(states) != INTSXP) {
          Rf_error("typeof(states) must be 'integer', not '%s'", Rf_type2char(TYPEOF(states)));
        }
        const int* const states__ = INTEGER(states);
        const R_xlen_t states__len_ = Rf_xlength(states);
        
        // initial_probs
        _args = CDR(_args);
        SEXP initial_probs = CAR(_args);
        if (TYPEOF(initial_probs) != REALSXP) {
          Rf_error("typeof(initial_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(initial_probs)));
        }
        const double* const initial_probs__ = REAL(initial_probs);
        const R_xlen_t initial_probs__len_ = Rf_xlength(initial_probs);
        
        // transition_probs
        _args = CDR(_args);
        SEXP transition_probs = CAR(_args);
        if (TYPEOF(transition_probs) != REALSXP) {
          Rf_error("typeof(transition_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(transition_probs)));
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
          Rf_error("typeof(emission_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(emission_probs)));
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
          Rf_error("typeof(observations) must be 'integer', not '%s'", Rf_type2char(TYPEOF(observations)));
        }
        const int* const observations__ = INTEGER(observations);
        const R_xlen_t observations__len_ = Rf_xlength(observations);
        
        // states
        _args = CDR(_args);
        SEXP states = CAR(_args);
        if (TYPEOF(states) != INTSXP) {
          Rf_error("typeof(states) must be 'integer', not '%s'", Rf_type2char(TYPEOF(states)));
        }
        const int* const states__ = INTEGER(states);
        const R_xlen_t states__len_ = Rf_xlength(states);
        
        // initial_probs
        _args = CDR(_args);
        SEXP initial_probs = CAR(_args);
        if (TYPEOF(initial_probs) != REALSXP) {
          Rf_error("typeof(initial_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(initial_probs)));
        }
        const double* const initial_probs__ = REAL(initial_probs);
        const R_xlen_t initial_probs__len_ = Rf_xlength(initial_probs);
        
        // transition_probs
        _args = CDR(_args);
        SEXP transition_probs = CAR(_args);
        if (TYPEOF(transition_probs) != REALSXP) {
          Rf_error("typeof(transition_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(transition_probs)));
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
          Rf_error("typeof(emission_probs) must be 'double', not '%s'", Rf_type2char(TYPEOF(emission_probs)));
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

