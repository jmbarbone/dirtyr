.onLoad <- function(libname, pkgname) {
  # packageStartupMessage(
  #   "Warning: This is a developmental package provided without any guarantee\n",
  #   "of functionality or correctness.\n",
  #   "Use at your own risk."
  #   )
}

.onAttach <- function(libname, pkgname) {
  if (is.null(getOption("dirtyr.tz"))) {
    options(dirtyr.tz = Sys.timezone())
  }
}
