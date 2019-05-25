##
## Removing xlsx dependence to allow shinyapps.io to compile this package
##
##

#library(xlsx)
#
#xlsx.write <- function (file, ...)
#{
#  require(xlsx, quietly = TRUE)
#  objects <- list(...)
#  fargs <- as.list(match.call(expand.dots = TRUE))
#  objnames <- as.character(fargs)[-c(1, 2)]
#  nobjects <- length(objects)
#  for (i in 1:nobjects) {
#    if (i == 1)
#      write.xlsx(objects[[i]], file, sheetName = objnames[i])
#    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
#                    append = TRUE)
#  }
#}


