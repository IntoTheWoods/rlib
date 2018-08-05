## list the functions associated with a given installed package

list.functions <- function(package.name) {
  ## this first line allows you to pass the function name without quotes
  package.name = as.character(substitute(package.name))
  function.list = paste('package', ':', package.name, sep="")
  x = lsf.str(function.list)
  y = head(x,n=length(x))
  return(y)
}
