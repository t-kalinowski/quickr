#' Declare quickr annotations
#'
#' `declare()` marks declarations (e.g., `type(x = double(n))`) for use by
#' `quickr`'s compiler. It is a runtime no-op and does not evaluate its
#' arguments.
#'
#' `declare()` is provided by the base package starting in R 4.4.0; for older
#' versions of R, quickr exports a compatible backport.
#'
#' `declare(parallel())` or `declare(omp())` applies an OpenMP `parallel do`
#' directive to the next `for` loop or `sapply()` assignment. `for` loops can
#' iterate values over symbols or index iterables (`1:n`, `seq_len()`,
#' `seq_along()`, `seq()`), and `sapply()` iterates over vector inputs with a
#' known length.
#'
#' Control the number of OpenMP threads using environment variables such as
#' `OMP_NUM_THREADS` (threads per region), `OMP_THREAD_LIMIT` (global cap),
#' and `OMP_DYNAMIC` (disable/enable runtime adjustment). Set them before
#' calling a compiled function, e.g. `Sys.setenv(OMP_NUM_THREADS = "4")`.
#'
#' When an error is raised inside a parallel loop, quickr cancels the
#' remaining iterations via OpenMP cancellation, which the OpenMP runtime
#' only honors when `OMP_CANCELLATION=true` is set before the runtime first
#' initializes in the process. quickr sets it when the package loads (unless
#' already set), but this has no effect if another package initialized the
#' OpenMP runtime first. Early exit is best-effort either way: the error
#' message is always recorded correctly; without cancellation the remaining
#' iterations simply run to completion before the error is raised.
#'
#' @param ... Declarations, typically calls like `type(x = double(n))`.
#' @returns `NULL`, invisibly.
#' @rawNamespace if (getRversion() < "4.4.0") export(declare)
#' @keywords internal
declare <- function(...) invisible()
