grc37_to_grc38 = function(df, chain = import.chain("hg19ToHg38.over.chain")) {
  df[,1] = paste("chr",df[,1],sep="")
  gr = bed_to_granges(df)
  result = liftOver(gr,chain)
  chr=gsub("chr","",as.data.frame(seqnames(result))$value)
  pos=as.data.frame(ranges(result))[,c("start","end")]
  
  if (length(df) > 3) {
    cbind(chr,pos,df[4:length(df)])
  } else {
    cbind(chr,pos)
  }
}

