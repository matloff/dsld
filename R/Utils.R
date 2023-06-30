
# many functions in dsld are wrappers for functions in other packages;
# in order to avoid "package bloat," we instead check for them as needed

# e.g. say a dsld function f() wraps some function in package p; then
# instead of listing p as imported etc. in the dsld DESCRIPTION file, 
# we write the top of f(), getSuggestedLib('p'); this loads p if it is
# installed on the user's machine, otherwise so informs the user

getSuggestedLib <- function(pkgName)
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop(paste0(pkgName, ' not loaded'))

saveFile <- function(filename)
{
   origdev <- dev.cur()
   parts <- strsplit(filename,".",fixed=TRUE)
   nparts <- length(parts[[1]])
   suff <- parts[[1]][nparts]
   if (suff == "pdf") {
       pdf(filename)
   }
   else if (suff == "png") {
       png(filename,bg='white')
   }
   else jpeg(filename)
   devnum <- dev.cur()
   dev.set(origdev)
   dev.copy(which = devnum)
   dev.set(devnum)
   dev.off()
   dev.set(origdev)
}
