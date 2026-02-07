# r2f-reductions-helpers.R
# Generic helpers for reduction operations.

# Create a hoist context for masking in reduction intrinsics.
# Used by: r2f-reductions.R
create_mask_hoist <- function() {
  .hoisted_mask <- NULL
  .conflict <- FALSE

  try_set <- function(mask) {
    stopifnot(inherits(mask, Fortran), mask@value@mode == "logical")
    mask <- booleanize_logical_as_int(mask)
    # each hoist can only accept one mask.
    if (is.null(.hoisted_mask)) {
      .hoisted_mask <<- mask
      return(TRUE)
    }
    # if the mask is identical, we accept it.
    if (identical(.hoisted_mask, mask)) {
      return(TRUE)
    }
    # can't hoist this mask.
    .conflict <<- TRUE
    FALSE
  }

  get_hoisted <- function() .hoisted_mask
  has_conflict <- function() .conflict

  environment()
}

# Convert a linear 1D index to multi-dimensional subscripts.
# Used by: r2f-reductions.R, r2f-subscript.R, r2f-control-flow.R
linear_subscripts_from_1d <- function(base_name, rank, idx) {
  stopifnot(is_string(base_name), is_wholenumber(rank), inherits(idx, Fortran))
  k <- glue("int({idx}, kind=c_int)")
  tmp <- glue("({k} - 1_c_int)")
  subs <- character(rank)
  if (rank == 1L) {
    subs[[1L]] <- k
    return(subs)
  }

  for (axis in seq_len(rank - 1L)) {
    dim_size <- glue("size({base_name}, {axis})")
    subs[[axis]] <- glue("(mod({tmp}, {dim_size}) + 1_c_int)")
    tmp <- glue("({tmp} / {dim_size})")
  }
  subs[[rank]] <- glue("({tmp} + 1_c_int)")
  subs
}
