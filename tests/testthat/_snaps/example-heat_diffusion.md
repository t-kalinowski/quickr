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
        integer(c_int) :: step
        real(c_double) :: temp_new(nx, ny)
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
          do i = 2_c_int, ((nx - 1_c_int)), sign(1, ((nx - 1_c_int))-2_c_int)
            do j = 2_c_int, ((ny - 1_c_int)), sign(1, ((ny - 1_c_int))-2_c_int)
      temp_new(i, j) = (temp(i, j) + ((k * dt) * ((((((temp((i + 1_c_int), j) - (2.0_c_double * temp(i, j))) + temp((i - 1_c_int), j))) &
      / (dx ** 2.0_c_double)) + ((((temp(i, (j + 1_c_int)) - (2.0_c_double * temp(i, j))) + temp(i, (j - 1_c_int)))) / (dy ** &
      2.0_c_double))))))
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
          Rf_error("typeof(nx) must be 'integer', not '%s'", Rf_type2char(TYPEOF(nx)));
        }
        const int* const nx__ = INTEGER(nx);
        const R_xlen_t nx__len_ = Rf_xlength(nx);
        
        // ny
        _args = CDR(_args);
        SEXP ny = CAR(_args);
        if (TYPEOF(ny) != INTSXP) {
          Rf_error("typeof(ny) must be 'integer', not '%s'", Rf_type2char(TYPEOF(ny)));
        }
        const int* const ny__ = INTEGER(ny);
        const R_xlen_t ny__len_ = Rf_xlength(ny);
        
        // dx
        _args = CDR(_args);
        SEXP dx = CAR(_args);
        if (TYPEOF(dx) != INTSXP) {
          Rf_error("typeof(dx) must be 'integer', not '%s'", Rf_type2char(TYPEOF(dx)));
        }
        const int* const dx__ = INTEGER(dx);
        const R_xlen_t dx__len_ = Rf_xlength(dx);
        
        // dy
        _args = CDR(_args);
        SEXP dy = CAR(_args);
        if (TYPEOF(dy) != INTSXP) {
          Rf_error("typeof(dy) must be 'integer', not '%s'", Rf_type2char(TYPEOF(dy)));
        }
        const int* const dy__ = INTEGER(dy);
        const R_xlen_t dy__len_ = Rf_xlength(dy);
        
        // dt
        _args = CDR(_args);
        SEXP dt = CAR(_args);
        if (TYPEOF(dt) != REALSXP) {
          Rf_error("typeof(dt) must be 'double', not '%s'", Rf_type2char(TYPEOF(dt)));
        }
        const double* const dt__ = REAL(dt);
        const R_xlen_t dt__len_ = Rf_xlength(dt);
        
        // k
        _args = CDR(_args);
        SEXP k = CAR(_args);
        if (TYPEOF(k) != REALSXP) {
          Rf_error("typeof(k) must be 'double', not '%s'", Rf_type2char(TYPEOF(k)));
        }
        const double* const k__ = REAL(k);
        const R_xlen_t k__len_ = Rf_xlength(k);
        
        // steps
        _args = CDR(_args);
        SEXP steps = CAR(_args);
        if (TYPEOF(steps) != INTSXP) {
          Rf_error("typeof(steps) must be 'integer', not '%s'", Rf_type2char(TYPEOF(steps)));
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

