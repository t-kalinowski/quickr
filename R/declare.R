#' Declare quickr annotations
#'
#' `declare()` marks declarations (e.g., `type(x = double(n))`) for use by
#' `quickr`'s compiler. It is a runtime no-op and does not evaluate its
#' arguments.
#'
#' `declare()` is provided by the base package starting in R 4.4.0; for older
#' versions of R, quickr exports a compatible backport.
#'
#' @param ... Declarations, typically calls like `type(x = double(n))`.
#' @returns `NULL`, invisibly.
#' @rawNamespace if (getRversion() < "4.4.0") export(declare)
#' @keywords internal
declare <- function(...) invisible()
