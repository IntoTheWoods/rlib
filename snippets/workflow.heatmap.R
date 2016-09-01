library(libbase)

{ ## config

script.config =  list(
  
  project.dir                = "~"
  ,
  libraries                  = c("gplots","RColorBrewer")
  ,
  source.file                = "scripts/workflow.combined.R"
  ,
  dbname                     = "database.rds.sqlite"
  ,
  data.tables                = list(signal="limma_logfc", fdr="limma_fdr")
  #data.tables                = list(fdr="gsea_fdr_tstat_msigdb.v5.0.symbols",  signal="gsea_nes_tstat_msigdb.v5.0.symbols")
  ,
  fdr.cutoff                 = 0.05
  #fdr.cutoff                 = 0.25
  ,
  row.filter.files           = c("row_filters_gene.tsv","row_filters_gsea.tsv")
  ,
  db.pragma                  = F
  ,
  column.annotations         = F
  ,
  annotate.columns           = T
  ,
  write.pdf                  = F
  ,
  cluster.rows               = T
  ,
  cluster.columns            = F
  ,
  pdf.file                   = "heatmap.pdf"
  ,
  heatmap.labels.num         = 150
  ,
  page.size                  = 32 ## rows per heatmap
  ,
  files=list()
  ,
  data.list=list()
  ,
  data=list()
  ,
  row.filters=list()
  ,
  data.heatmap=list()
  
)

} ## config

libbase::initialize.workspace()

{

design = sql.query('
                   
                   select *
                   from contrasts_annotated
                   where 1=1

                   order by model, type

                   ')








for (data.table.name in names(data.tables)) {
  data.table=data.tables[[data.table.name]]
  data[[data.table.name]]=sql.query(paste("select row_names, ",paste(paste("\"",design$contrast,"\"",sep=""),collapse=",")," from ",r2sql(data.table)))
} ## load data.tables

} ## load design and data tables

data[["signal"]][is.na(data[["signal"]])] <- 0
data[["fdr"]][is.na(data[["fdr"]])] <- 1
data[["signal"]][data[["signal"]]==-20] = 0
data[["signal"]][data[["signal"]]==20] = 0

data[["signed 1-fdr"]]=sign(data[["signal"]])*(1-data[["fdr"]])
data.filter=data[[1]]

if (annotate.columns) {

annotation.type=(factor(design$type,labels=brewer.palette(length(levels(factor(design$type))),"Set3")))
annotation.gic50 <- colorRampPalette(c('red','blue'))(10)[as.numeric(cut(log10(as.numeric(design$gic50)),breaks = 10))]

column.annotations=t(matrix(c(
  as.character(annotation.type),
  annotation.gic50
),nrow=2,byrow=T))

colnames(column.annotations) = c("Type","gIC50")

legend.text=toupper(c(levels(factor(design$type)),""))
legend.colors=c(levels(annotation.type),"#FFFFFF")


} ## add column color annotations

{
##
## row filters
##

if (TRUE) {  

  num.columns = length(colnames(data[["fdr"]]))
  my.data=as.data.frame(data[["signed 1-fdr"]])
  
  my.freqs = c(0.01,0.2, 0.3, 0.35,0.4,0.45,0.5,0.6,0.7,0.8,0.9, 1)
  #my.freqs = c(0.2, 0.3,0.4, 0.5,0.75,1)
  #my.freqs = c(1)
  my.freqs = c(0.3)
  #my.freqs = c(0.01)

  for (my.freq in my.freqs) {
    for (my.sign in c(1,-1)) {
    my.subset = ifelse(my.sign == 1, "up","dn")
      row.filter.name = paste(my.freq,my.subset, sep="")
      row.filters[[row.filter.name]] = apply(ifelse(my.data*my.sign > (1-fdr.cutoff), 1, 0), 1, sum) >= my.freq*num.columns 
    }
  }
  
} ## FDR significant genes

if (FALSE) {  

  for (row.filter.file in row.filter.files) {
    # Read in the data
    x <- scan(row.filter.file, what="", sep="\n")
    # Separate elements by one or more whitepace
    y <- strsplit(x, "\t")
    # Extract the first vector element and set it as the list element name
    names(y) <- toupper(sapply(y, `[[`, 1))
    #names(y) <- sapply(y, function(x) x[[1]]) # same as above
    # Remove the first vector element from each list element
    y <- lapply(y, `[`, -1)
    ## Pull out the second vector from each list element
    z <- lapply(y, `[`,  1)
    y <- lapply(y, `[`, -1)
    #y <- lapply(y, function(x) x[-1]) # same as above
    
    for (custom in names(y[as.logical(z)])) {
      row.filters[[custom]]=rownames(data.filter) %in% y[[custom]]
      #row.filters[[custom]]=match(rownames(data.filter), y[[custom]])
    }
  }
  
} ## row filters from tsv files

if (FALSE) {
  ## paged fdr cutoffs
  ## mean of fdr
  fdr.ranking = (apply(data.filter,1,mean,na.rm=T))
  #fdr.ranking = (apply(data.filter,1,min,na.rm=T))
  #fdr.ranking = (apply(data.filter,1,max,na.rm=T))
  
  ## set zero values to randomly assigned negative counter to avoid pagination issues
  fdr.ranking[fdr.ranking == 0 ] = rev(-seq(1:sum(fdr.ranking == 0)))
  
  cutoffs=sort(fdr.ranking)[seq(page.size,length(data.filter[,1]),page.size)]
  cutoffs=cutoffs[1:(length(cutoffs[cutoffs<fdr.cutoff])+1)]
  cutoffs[length(cutoffs)]=fdr.cutoff
  
  row.filters[[paste("FDR <",min(round(cutoffs[1],3),fdr.cutoff),sep="")]]=fdr.ranking <= min(cutoffs[1],fdr.cutoff)
  counter=2
  for (cutoff in cutoffs[-1]) {
    row.filters[[paste("FDR = ",round(cutoffs[counter-1],3),"-",min(round(cutoffs[counter],3),fdr.cutoff),sep="")]]=
      fdr.ranking >= cutoffs[counter-1] &
      fdr.ranking <  min(cutoffs[counter],fdr.cutoff)
    counter=counter+1
  }
} ## paged average fdr cutoffs

if (FALSE) {  
  
  row.filters[["XYZ"]] = rep(T,length(rownames(data.filter)))      &
    grepl("XYZ",rownames(data.filter))                  &
    rep(T,length(rownames(data.filter)))
  
} ## extra filters


} ## gsea row filters

## remove row filters with zero rows
row.filters=row.filters[names(row.filters)[!(lapply(row.filters,sum)==0)]]


items = c("signal","signed 1-fdr")
#items = c("signed 1-fdr")

{
##
## plot heatmaps for each row.filter to pdf
##

if (write.pdf == TRUE) {
  pdf(file=pdf.file,width=10,height=7.5, paper="USr")
  palette("default")
}  ## plot to pdf

for (row.filter.num in 1:length(row.filters)) {
    row.filter = row.filters[[row.filter.num]]
    row.filter.name = names(row.filters)[row.filter.num]
    
    for (item in items) {
      
      ## as.data.frame prevents conversion to vector with one column
      data.heatmap[[item]]=as.data.frame(data[[item]][row.filter, ])

      ## colors
      if (item=="signed 1-fdr"){

        my.number=as.integer(fdr.cutoff*100/5)
        my.blues=c(rep("#3F3FFF",my.number),rep("#7F7FFF",my.number),rep("#BFBFFF",3*my.number),rep("#FFFFFF",100-5*my.number))
        my.blues[1]="#0000FF"
        my.reds=rev(reorder_hexcodes(my.blues))
        my.colors=c(my.blues,my.reds)
        
      } else if (item=="fdr") {

        my.number=as.integer(fdr.cutoff*100/5)
        my.blues=c(rep("#3F3FFF",my.number),rep("#7F7FFF",my.number),rep("#BFBFFF",3*my.number),rep("#FFFFFF",100-5*my.number))
        my.blues[1]="#0000FF"
        my.colors=c(my.blues)
        
      } else if (item == "signal") {
        
        my.blues=c(rep("#3F3FFF",0),rep("#7F7FFF",0),rep("#BFBFFF",19),rep("#FFFFFF",1))
        my.blues=c("#0000FF",rep(my.blues,each=100)[-1])
        
        my.reds=c(rep("#FF3F3F",0),rep("#FF7F7F",0),rep("#FFBFBF",19),rep("#FFFFFF",1))
        my.reds=rev(c("#FF0000",rep(my.reds,each=100)[-1]))
        
        #my.colors=c(my.blues,my.reds)
        my.colors=rev(colorRampPalette(c("red","red","white","blue","blue"))(256))
        
      } else {
        my.colors=rev(colorRampPalette(c("red","red","white","white","blue","blue"))(256))
      }
      
      ## heatmap needs to be 2x2
      if (length(data.heatmap[[item]][,1]) > 1 & length(data.heatmap[[item]][1,]) > 1 ) {
        
        heatmap.mod(as.matrix(data.heatmap[[item]]),
                    Rowv=cluster.rows, 
                    Colv=cluster.columns, 
                    dendrogram="none",
                    trace="none", 
                    key=T,
                    col=my.colors,
                    ColSideColors=column.annotations,
                    ColSideColorsSize=2,
                    lmat=rbind(4:3 + 1, c(0, 1), 2:1 + 1),
                    lhei=c(0.7, 0.6, 4), 
                    lwid=c(2,8),
                    labRow = if(length(rownames(data.heatmap[[item]])) > heatmap.labels.num) {NA} else {rownames(data.heatmap[[item]])},
                    margins=c(20,20),
                    cexRow=2,
                    cexCol=1,
                    density.info="none",
                    colsep=if(length(rownames(data.heatmap[[item]])) > 50) {NA} else {0:length(colnames(as.matrix(data.heatmap[[item]])))},
                    rowsep=if(length(rownames(data.heatmap[[item]])) > 50) {NA} else {0:length(rownames(as.matrix(data.heatmap[[item]])))},
                    sepwidth=c(0.00001, 0.00001),
                    sepcolor='black',
                    scale="none"
        )
        
        title(toupper(paste("gene filter: ",row.filter.name,"\nplotting: ",item,sep="")), outer=T, line=-4)
        
        ## trick to create a new plot that fills the entire viewable space before plotting the legend
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        
        if (annotate.columns) {
        legend(
          "topright",
          legend=legend.text, 
          fill=legend.colors,
          border=F,
          bty="o",
          bg="white",
          box.col="white",
          y.intersp = 0.7,
          cex=0.9
        )
        }

      
      }
      
    }  ## draw heatmaps
}  ## for each row.filter, draw heatmaps


} ## plot heatmaps for each row.filter to pdf

if (FALSE) {
##
## clean up
##

if (write.pdf) {
  dev.off()
}
dbDisconnect(db)

} ## clean up

