.onAttach <- function(libname, pkgname) {
   packageStartupMessage(
      'DSLD: Data Science Looks at Discrimination\n',
      'See vignette("Quick_Start", package = "dsld") and ',
      'vignette("Function_List", package = "dsld").')
}
