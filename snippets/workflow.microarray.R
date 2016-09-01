

##
## clear data space and create directories
##
rm(list=ls())

##
## library installation
##
{
library(libbase)
library(ggplot2)
library(RColorBrewer)
library(limma)
#library(igraph)
#library(CausalR)
library(gplots)

#library(RSQLite)
#library(dplyr)
}
##
## logical variables for code execution control
##
{
windows=FALSE
write.file=TRUE
basal=FALSE
bind.data.columns=F
read.data.files=T
write.data.file=T
}


#################################

##
## session preparation
##
{
if(windows) {
  working.dir=""
  libbase::session.prepare(working.dir)
} else {
  working.dir=getwd()
  libbase::session.prepare(working.dir,source.file="~/scratch/workflow.R")
}
#db <- RSQLite::dbConnect(SQLite(sqlite.db))

}

##
## additional user-defined variables
##
{
data.dir="data"
data.file.ext=".tsv"
design.dir="design"
design.file.ext=".tsv"
}


dir.create("limma", showWarnings = FALSE)
dir.create("gsea", showWarnings = FALSE)

##
## perform rma normalization and probeset2gene conversion as necessary
##
expr.file="expr.tsv"
df.expr.file="df.expr.tsv"

if(file.exists(expr.file)) {
  print("reading expr")
  expr=read.table(expr.file,sep="\t",header=T)
  expr=as.matrix(expr)
} else {  
  ## rma normalize all files in cel directory
  if(file.exists(df.expr.file)) {
    print("reading df.expr")
    df.expr=read.table(df.expr.file,sep="\t",header=T)
  } else { 
    print("running RMA normalization")
    df.expr = libbase::expr.rma(qc=TRUE)
    write.table(df.expr,file=df.expr.file,sep="\t",col.names=T,row.names=T,quote=F)
  }
  print("running probeset2gene conversion")
  expr = libbase::expr.probeset2gene(df.expr)
  write.table(expr,file=expr.file,sep="\t",col.names=T,row.names=T,quote=F)
}

##
## hierarchical clustering to check normalization
##
hclust.file="affymetrix_rma_normalized_hclust.pdf"
d=dist(t(expr))
hcgdata=hclust(d)
pdf(file=hclust.file)
palette("default")
plot(hcgdata,main="Normalized Affy Hierarchical Clustering")
dev.off()


##
## limma linear model
##

## read in design table and contrasts
## df.design data frame must contain "sample" and "condition" columns
## df.contrasts must contain "name" and "contrast" columns

df.design=read.table("design.tsv",header=T,sep='\t')
df.contrasts=read.table("contrasts.tsv",header=T,sep='\t')

limma.result=libbase::limma.fit(df.design,df.contrasts,expr)

###################################################################################################################

if(basal) {
limma.result.per.cel = function(cel.name) {
#  df.design$condition=as.character(df.design$condition)
  df.design$condition="other"
  df.design$condition[df.design$sample==cel.name]=cel.name
#  df.design$condition=as.factor(df.design$condition)
  df.contrasts$name=cel.name
  df.contrasts$contrast = paste(cel.name,"-","other",sep="")
  limma.result=limma.fit(df.design, df.contrasts,expr)
  limma.result
}

limma.result.cel=mapply(limma.result.per.cel,as.list(colnames(expr)))

limma.result = c(limma.result, limma.result.cel)
}

###################################################################################################################


my.apply = function(my.function, my.list, my.args) {
  mapply(my.function,names(my.list),my.list,SIMPLIFY=F)
}

##
## write gsea prerank file
##

limma.result.write.gsea =function(contrast.name,limma.result) {
  ## gsea java program doesn't work with hyphens in file names
  contrast.name=gsub("-","_",contrast.name)
  ## t-statistic based
  gsea.rank.file=paste("./gsea/gsea.",contrast.name,".tstat.rnk",sep="")
  gsea.rank=limma.result$t[grep(" ",rownames(limma.result),invert=T)]
  names(gsea.rank)=rownames(limma.result[grep(" ",rownames(limma.result),invert=T),])
  gsea.rank=gsea.rank[order(gsea.rank,decreasing=T)]
  write.table(cbind(Names=names(gsea.rank),gsea.rank),file=gsea.rank.file,sep="\t",col.names=T,row.names=F,quote=F)
  ## logFC based
  gsea.rank.file=paste("./gsea/gsea.",contrast.name,".logFC.rnk",sep="")
  gsea.rank=limma.result$logFC[grep(" ",rownames(limma.result),invert=T)]
  names(gsea.rank)=rownames(limma.result[grep(" ",rownames(limma.result),invert=T),])
  gsea.rank=gsea.rank[order(gsea.rank,decreasing=T)]
  write.table(cbind(Names=names(gsea.rank),gsea.rank),file=gsea.rank.file,sep="\t",col.names=T,row.names=F,quote=F)
}

if(write.file) {
  my.apply(limma.result.write.gsea,limma.result)
}

##
## write limma file
##

limma.result.write.limma =function(contrast.name,limma.result) {
  limres.file=paste("./limma/limma.result.",contrast.name,".tsv",sep="")
  write.table(limma.result,file=limres.file,sep="\t",col.names=T,row.names=T,quote=F)
}

if(write.file) {
  my.apply(limma.result.write.limma,limma.result)
}

##
## get significant results and build heatmaps
##


limma.result.get.sig = function(contrast.name, limma.result, pvalue.cutoff=0.05) {
  limma.result.sig=limma.result[limma.result$adj.P.Val<=pvalue.cutoff,]
  limma.result.sig=limma.result.sig[order(limma.result.sig$t,decreasing=T),]
  limma.result.sig
}

limma.result.sig=my.apply(limma.result.get.sig,limma.result)

limma.result.build.heatmap = function(contrast.name, limma.result) {
  if(length(limma.result[,1])>1) {
    heatmap(expr[rownames(limma.result),df.design$sample],
            Colv=NA,
            Rowv=NA,
            col=greenred(75),
            main=contrast.name,
            revC=T
    )
  }
}


heatmap.file="limma_contrast_heatmaps.pdf"
pdf(file=heatmap.file)
palette("default")

my.apply(limma.result.build.heatmap,limma.result.sig)

dev.off()

