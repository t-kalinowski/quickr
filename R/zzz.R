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
  suppressWarnings({
    S7::methods_register()
    asNamespace("dotty")$dotify()
  })
  # Generated error paths inside OpenMP loops emit `!$omp cancel do`, which
  # the OpenMP runtime treats as a no-op unless the cancel-var ICV is true.
  # The ICV is read from OMP_CANCELLATION when the runtime first initializes
  # in the process, so set it as early as quickr can; this has no effect if
  # another package already started the OpenMP runtime. Error *messages* are
  # recorded correctly either way -- cancellation only enables early exit.
  if (!nzchar(Sys.getenv("OMP_CANCELLATION"))) {
    Sys.setenv(OMP_CANCELLATION = "true")
  }
  # on_load_register_.AtNames.default()
}
