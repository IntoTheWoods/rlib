#' Function to build a package skeleton
#'
#' This function is an example of how to build a package.
#' @param package.name.
#' @keywords package
#' @export
#' @examples
#' package.build.example()

package.build.example = function(package.name="testing") {

  devtools::create(package.name)

  setwd(paste(package.name,"/R",sep=""))
  testing.function = data.frame(lines = 
         c("#' Testing function",
           "#'",
           "#' This is a testing function",
           "#' @param test.word",
           "#' @keywords none",
           "#' @export",
           "#' @examples",
           "#' testing.function()",
           "",
           "testing.function = function(test.word) { print(test.word) }",
           "")
  )
  write.table(testing.function,"testing.function.R",quote=F,col.names=F,row.names=F,sep="\t")
  
  setwd("..")
  devtools::document()
  setwd("..")


}
