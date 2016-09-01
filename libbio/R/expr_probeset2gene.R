#' Probeset Gene Annotation and Selection of Probe with Per Gene Maximum Expression
#'
#' @param df.expr - a data frame containing expression data with probesets as rownames()
#' @param expr.min - a minimum expression threshold, if no value is given defaults to no threshold
#' @keywords
#' @export
#' @examples
#' expr.probeset2gene()
expr.probeset2gene = function(df.expr,expr.min=0) {
  ## read affymetrix probeset/gene database
  ## and order by probeset name
  library("annotate")
  library("hgu133plus2.db")
  df.probeset.gene = select(hgu133plus2.db, rownames(df.expr),c("SYMBOL"))
  df.probeset.gene$SYMBOL[is.na(df.probeset.gene$SYMBOL)] <- "---"
  df.probeset.gene = aggregate(SYMBOL ~ PROBEID, df.probeset.gene, paste, collapse = " ")
  colnames(df.probeset.gene)=c("probeset","gene")  
  rownames(df.probeset.gene)=df.probeset.gene$probeset
  df.probeset.gene.ordered = df.probeset.gene[order(rownames(df.probeset.gene)),]

  ## get the mean expression for each probeset in the data set
  ## and order this list of means by probeset name
  v.probeset.mean=apply(df.expr,1,mean)
  v.probeset.mean.ordered=v.probeset.mean[order(names(v.probeset.mean))]

  ## bind the mean to the list of probesets/genes
  df.probeset=cbind(mean=v.probeset.mean.ordered,df.probeset.gene.ordered)
  ## order this bound list by decreasing order of means for duplication marking
  df.probeset.ordered=df.probeset[order(df.probeset$gene,-df.probeset$mean),]
  ## mark duplicates (marked if previously seen, hence the need for decreasing mean order)
  ## and store into a separate vector
  v.probeset.duplicate=duplicated(df.probeset.ordered$gene)
  ## create a curated list without the duplicated probesets
  ## this list will have one probeset per gene (or cross-gene probeset)
  ## also remove probesets for which no gene is known, i.e. "---"
  ## order the curated list by probeset
  df.probeset.curated=df.probeset.ordered[!v.probeset.duplicate,]
  df.probeset.curated=df.probeset.curated[!df.probeset.curated$gene == "---",]
  df.probeset.curated.ordered=df.probeset.curated[order(rownames(df.probeset.curated)),]

  ## order the expression data frame
  ## curate the expression data frame with the curated probeset list
  ## and set the gene names as the rownames for the expression data frame
  df.expr.ordered=df.expr[order(rownames(df.expr)),]
  df.expr.curated = df.expr[rownames(df.expr) %in% df.probeset.curated.ordered$probeset,]
  rownames(df.expr.curated)=df.probeset.curated.ordered$gene

  ## convert the data frame to a matrix
  m.expr.curated=as.matrix(df.expr.curated)
  expr=m.expr.curated
  
  if (expr.min != 0) {
    min.function=function(x) {if(x<expr.min) {x=expr.min} ; x }
    expr=apply(expr,c(1,2),min.function)
  }

  return(expr)
}
