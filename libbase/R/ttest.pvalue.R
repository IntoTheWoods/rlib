## function: t-test
ttest.pvalue = function(x,y) {
  if(sd(x)==0 & sd(y)==0) {
    ttest=NULL
    ttest$p.value=1
  } else {
    ttest = t.test(x,y)
  }
  ttest$p.value
}

