limma.fit = function(df.design, df.contrasts,expr, array.weights=NULL) {

  df.design$condition=factor(df.design$condition)
  design.matrix = model.matrix(~0+df.design$condition)
  colnames(design.matrix)=levels(df.design$condition)

  if(is.null(array.weights)) {
    fit <- limma::lmFit(log2(expr[,df.design$sample]),design.matrix)
  } else {
    fit <- limma::lmFit(log2(expr[,df.design$sample]),design.matrix, weights=array.weights)
  }
 
  ## the contrasts parameter does not accept named contrasts like
  ##contrast.args = paste(df.contrasts$name, df.contrasts$contrast,sep="=")
  contrast.args = as.character(df.contrasts$contrast)
  contrast.matrix <- limma::makeContrasts(contrasts=contrast.args,levels=design.matrix)
  
  fit2 <- limma::contrasts.fit(fit, contrast.matrix)
  fit2 <- limma::eBayes(fit2)

  limma.results <- list() ##"data.frame",number.contrasts
  
  number.contrasts=length(contrast.args)
  for (contrast.coefficient in 1:number.contrasts) {
    limma.results[[ as.character(df.contrasts$contrast[contrast.coefficient]) ]] <- limma::topTable(fit2, coef=contrast.coefficient, number=22120, adjust="fdr")
  }
  ## return value
  limma.results
}

