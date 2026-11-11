.onLoad <- function(...) {
  S7::S7_on_load()
  asNamespace("dotty")$dotify()
  # Generated error paths inside OpenMP loops emit `!$omp cancel do`, which
  # the OpenMP runtime treats as a no-op unless the cancel-var ICV is true.
  # The ICV is read from OMP_CANCELLATION when the runtime first initializes
  # in the process, so set it as early as quickr can; this has no effect if
  # another package already started the OpenMP runtime. Error *messages* are
  # recorded correctly either way -- cancellation only enables early exit.
  if (!nzchar(Sys.getenv("OMP_CANCELLATION"))) {
    Sys.setenv(OMP_CANCELLATION = "true")
  }
}

.onUnload <- function(...) {
  S7::S7_on_unload()
}

S7::S7_on_build()
