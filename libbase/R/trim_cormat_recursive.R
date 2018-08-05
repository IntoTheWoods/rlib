trim_cormat_recursive = function(data.matrix, cutoff = 0.7, correlation_method = "spearman") {

  if (is.null(ncol(data.matrix)) || ncol(data.matrix) < 2) {
    return(NULL)
  }

  cormat = cor(data.matrix, method=correlation_method)
  
  ## if the mean of the matrix is greater than the cutoff (setting diagonal to cutoff)
  reduced=cormat*(1-diag(dim(cormat)[1]))+cutoff*diag(dim(cormat)[1])
  if (mean(reduced)-sqrt(var(as.numeric(reduced))) >= cutoff) {
    ## reset the diagonal and return
    #reduced = reduced*(1-diag(dim(reduced)[1]))+diag(dim(reduced)[1])
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    return(round(cormat[hc$order, hc$order],2))
  } else {
    ## remove the least coherent sample
    i=which.min(apply(cormat,1,sum))
    data.matrix = data.matrix[-i,-i]
    trim_cormat_recursive(data.matrix, cutoff, correlation_method)
  }
}

