##
## function: spearman rho p-value
##
cor.pvalue = function(x,y,method="spearman") {
  test=cor.test(x,y,method=method)
  test.stats=c(test$p.value,test$estimate)
  test.estimate.name=names(test$estimate)
  names(test.stats)=c("pvalue",test.estimate.name)
  test.stats
}

