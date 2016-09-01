#' Process cel files using RMA
#'
#' @param cel.path - defaults to ./cel
#' @param result.path - defaults to .
#' @param qc - run quality control metrics and save a pdf report - defaults to FALSE
#' @keywords
#' @export
#' @examples
#' expr.rma()
expr.rma = function(cel.path="./cel",result.path=".",qc=FALSE) {

#  cel.path="./cel"
#  result.path="."
#  qc=FALSE
  
  dir.create(result.path, showWarnings = FALSE)
  output.file=paste(result.path,"/","affymetrix_rma_normalized.txt",sep="")
  metrics.file=paste(result.path,"/","affymetrix_rma_normalized_metrics.pdf",sep="")
  
  library(affy)
  cel.files=list.celfiles(path=cel.path)
  cel.files
  
  raw.data=ReadAffy(filenames=cel.files,celfile.path=cel.path)
  sampleNames(raw.data)=gsub(".CEL","",sampleNames(raw.data))
  
  ##
  ## RMA Normalization
  ##
  es.expr = rma(raw.data)
  m.expr = 2^exprs(es.expr)
  df.expr=as.data.frame(m.expr)
  df.expr=df.expr[order(rownames(df.expr)),]

  write.table(cbind(Names=rownames(df.expr),df.expr),output.file,quote=F,sep="\t",row.names=F)
  
  # report basic qc information
  if(qc==TRUE) {
    library("RColorBrewer")
    usr.col=brewer.pal(9,"Set1")
    mycols=rep(usr.col,3)
    
    pdf(file=metrics.file)
    boxplot(raw.data,col=mycols,las=3,cex.axis=0.5,names=toupper(sampleNames(raw.data)))
    boxplot(log2(df.expr),col=mycols,las=3,cex.axis=0.5,names=colnames(df.expr)) 
    dev.off()
    ##
    ##Quality Control Metrics for Affymetrix Chip Data
    ##
    library("simpleaffy")
    raw.data.qc = qc(raw.data)
    plot(raw.data.qc)
  }
  
  return(df.expr)
}
