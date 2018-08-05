long2wide=function(data.long,x,y,value,options=list() ) {
  library(reshape2)
  options[["data"]] = data.long
  options[["formula"]] = paste(x," ~ ",y,sep="")
  options[["value.var"]] = value
  data.wide=do.call(reshape2::dcast,options)
  rownames(data.wide)=data.wide[,x]
  data.wide[,x]=NULL
  data.wide
}

