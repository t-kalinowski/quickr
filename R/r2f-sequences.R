# r2f-sequences.R
# Handlers for sequence generation: :, seq, seq_len, seq_along

# --- Handlers ---

r2f_handlers[[":"]] <- function(args, scope, ...) {
  seq_like_r2f(":", args, scope, ...)
}


r2f_handlers[["seq"]] <- function(args, scope, ...) {
  seq_like_r2f("seq", args, scope, ...)
}

r2f_handlers[["seq_len"]] <- function(args, scope, ...) {
  seq_like_r2f("seq_len", args, scope, ...)
}

r2f_handlers[["seq_along"]] <- function(args, scope, ...) {
  seq_like_r2f("seq_along", args, scope, ...)
}
