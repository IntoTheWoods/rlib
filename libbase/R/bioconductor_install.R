#' Install Bioconductor
#'

bioconductor.install=function() { 
  source("http://bioconductor.org/biocLite.R")
  biocLite()

  biocLite("affy")
  biocLite("S4Vectors")
  biocLite("IRanges")
  biocLite("hgu133plus2cdf")
  biocLite("simpleaffy")
  biocLite("hgu133plus2.db")
  biocLite("limma")
}

