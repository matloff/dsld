
# many functions in dsld are wrappers for functions in other packages;
# in order to avoid "package bloat," we instead check for them as needed

# e.g. say a dsld function f() wraps some function in package p; then
# instead of listing p as imported etc. in the dsld DESCRIPTION file, 
# we write the top of f(), getSuggestedLib('p'); this loads p if it is
# installed on the user's machine, otherwise so informs the user

getSuggestedLib <- function(pkgName)
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop(paste0(pkgName, ' not loaded'))


