# # ' @export
# `@.default` <- function(x, name) {
#   if (isS4(x))
#     methods::slot(x, name)
#   else
#     attr(x, name, TRUE)
# }
#
# # ' @export
# `@<-.default` <- function(x, name, value) {
#   if (isS4(x))
#     methods::`slot<-`(x, name, value = value)
#   else
#     `attr<-`(x, name, value)
# }
#
# # ' @importFrom utils .AtNames findMatches
# .AtNames.default <- function(x, pattern = "") {
#   if (isS4(x))
#     findMatches(pattern, methods::slotNames(x))
#   else
#     findMatches(pattern, names(attributes(x)))
# }
#
# on_load_register_.AtNames.default <- function() {
#   # if we register via NAMESPACE, we get warning
#   # about overwriting utils:::.AtNmaes.default
#   registerS3method(".AtNames", "default", .AtNames.default)
# }

.onLoad <- function(...) {
  if (!nzchar(Sys.getenv("OMP_NUM_THREADS", unset = ""))) {
    cores <- parallel::detectCores(logical = FALSE)
    if (is.na(cores) || cores < 1L) {
      cores <- 1L
    }
    threads <- max(1L, floor(cores * 0.75))
    Sys.setenv(OMP_NUM_THREADS = as.character(threads))
  }
  suppressWarnings({
    S7::methods_register()
    asNamespace("dotty")$dotify()
  })
  # on_load_register_.AtNames.default()
}
