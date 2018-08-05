reorder_hexcodes = function(x,y=c(3,2,1)) {
  code=list()
  code[[1]] = substr(x, 2,3)
  code[[2]] = substr(x, 4,5)
  code[[3]] = substr(x, 6,7)
  
  flipped = substr(x,1,1)
  for (z in y) {
    flipped=paste(flipped,code[[z]],sep="")
  }
  flipped
}
