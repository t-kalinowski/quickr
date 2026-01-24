quickr_error_msg_name <- function() "quickr_err_msg"

quickr_error_msg_len <- function() 256L

quickr_error_setter_name <- function() "quickr_set_error_msg"

quickr_error_arg_names <- function() {
  c(quickr_error_msg_name())
}

is_quickr_error_msg <- function(name) {
  identical(name, quickr_error_msg_name())
}

scope_root_for_errors <- function(scope) {
  if (!inherits(scope, "quickr_scope")) {
    return(scope)
  }
  while (
    !identical(attr(scope, "kind", exact = TRUE), "subroutine") &&
      inherits(parent.env(scope), "quickr_scope")
  ) {
    scope <- parent.env(scope)
  }
  scope
}

mark_scope_uses_errors <- function(scope) {
  root <- scope_root_for_errors(scope)
  if (inherits(root, "quickr_scope")) {
    attr(root, "uses_errors") <- TRUE
  }
  invisible(TRUE)
}

scope_uses_errors <- function(scope) {
  root <- scope_root_for_errors(scope)
  isTRUE(attr(root, "uses_errors", TRUE))
}

fortran_string_literal <- function(x) {
  stopifnot(is_string(x))
  escaped <- gsub("\r\n|\r|\n", "\\\\n", x)
  escaped <- gsub("\"", "\"\"", escaped, fixed = TRUE)
  paste0("\"", escaped, "\"")
}

check_quickr_error_message_continuable <- function(msg) {
  stopifnot(is_string(msg))
  if (grepl("[ \t]", msg)) {
    return(invisible(TRUE))
  }
  msg_literal <- fortran_string_literal(msg)
  line_len <- nchar(glue(
    "&{quickr_error_setter_name()}( {msg_literal} )"
  ))
  if (line_len > 132L) {
    stop(
      "Error message is too long to fit in a single Fortran line without spaces.",
      " Add spaces to allow line continuations.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

quickr_error_manifest_lines <- function() {
  msg_name <- quickr_error_msg_name()
  len_val <- quickr_error_msg_len()

  glue("character(kind=c_char), intent(inout) :: {msg_name}({len_val})")
}

quickr_error_helper_fortran <- function(openmp = FALSE) {
  msg_name <- quickr_error_msg_name()
  setter <- quickr_error_setter_name()
  len_val <- quickr_error_msg_len()

  glue::trim(str_flatten_lines(
    glue("subroutine {setter}(msg)"),
    if (isTRUE(openmp)) "  use omp_lib, only: omp_in_parallel",
    "  character(len=*), intent(in) :: msg",
    "  integer :: i",
    "  integer :: n",
    if (isTRUE(openmp)) "  !$omp critical (quickr_error)",
    glue("  if ({msg_name}(1) == c_null_char) then"),
    glue("    n = min(len(msg), {len_val} - 1)"),
    glue("    {msg_name}(1:n) = [(msg(i:i), i = 1, n)]"),
    glue("    {msg_name}(n + 1) = c_null_char"),
    "  end if",
    if (isTRUE(openmp)) "  !$omp end critical (quickr_error)",
    if (isTRUE(openmp)) "  if (omp_in_parallel()) then",
    if (isTRUE(openmp)) "    !$omp cancel parallel",
    if (isTRUE(openmp)) "  end if",
    glue("end subroutine {setter}")
  ))
}

quickr_error_fortran_lines <- function(message = NULL) {
  msg <- message %||% "quickr error"
  stopifnot(is_string(msg))
  if (!nzchar(msg)) {
    msg <- "quickr error"
  }
  check_quickr_error_message_continuable(msg)
  msg_literal <- fortran_string_literal(msg)
  c(glue("call {quickr_error_setter_name()}({msg_literal})"), "return")
}

quickr_error_return_if_set <- function(scope) {
  if (isTRUE(scope_uses_errors(scope))) {
    glue("if ({quickr_error_msg_name()}(1) /= c_null_char) return")
  } else {
    ""
  }
}
