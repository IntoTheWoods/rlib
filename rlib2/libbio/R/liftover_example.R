gr <- GRanges(seqnames =
              Rle(c("chr1", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
            ranges =
             IRanges(1:10, end = 7:16, names = head(letters, 10)),
           strand =
               Rle(strand(c("-", "+", "*", "+", "-")),
                    + c(1, 2, 2, 3, 2)),
            score = 1:10,
             GC = seq(1, 0, length=10))


#' BED to GRanges
#'
#' This function loads a BED-like file and stores it as a GRanges object.
#' The tab-delimited file must be ordered as 'chr', 'start', 'end', 'id', 'score', 'strand'.
#' The minimal BED file must have the 'chr', 'start', 'end' columns.
#' Any columns after the strand column are ignored.
#' 
#' @param file Location of your file
#' @keywords BED GRanges
#' @export
#' @examples
#' bed_to_granges('my_bed_file.bed')


df = data.frame(chr=c("chr19","chr19"),start=c(6668972,6895866),stop=c(6668972,6895866),strand=c("+","+"),score=c(1,2),id=c(1,2))
df = data.frame(chr=c(19,19),start=c(6668972,6895866),stop=c(6668972,6895866))

bed_to_granges <- function(df){

  if(length(df) > 6){
    df <- df[,-c(7:length(df))]
  }
  
  if(length(df)<3){
    stop("File has less than 3 columns")
  }
  
  header <- c('chr','start','end','id','score','strand')
  names(df) <- header[1:length(names(df))]
  
  if('strand' %in% colnames(df)){
    df$strand <- gsub(pattern="[^+-]+", replacement = '*', x = df$strand)
  }
  
  library("GenomicRanges")
  
  if(length(df)==3){
    gr <- with(df, GRanges(chr, IRanges(start, end)))
  } else if (length(df)==4){
    gr <- with(df, GRanges(chr, IRanges(start, end), id=id))
  } else if (length(df)==5){
    gr <- with(df, GRanges(chr, IRanges(start, end), id=id, score=score))
  } else if (length(df)==6){
    gr <- with(df, GRanges(chr, IRanges(start, end), id=id, score=score, strand=strand))
  }
  return(gr)
}

df = data.frame(chr=c(19,19),start=c(6668972,6895866),stop=c(6668972,6895866))


overlift_bed = function(df, chain = import.chain("hg19ToHg38.over.chain")) {
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


overlift_vcf = function(vcf, chain = import.chain("hg19ToHg38.over.chain")) {
  df = data.frame(chr=paste("chr",vcf[,1],sep=""), start=vcf[,2], end=vcf[,2])
  gr = bed_to_granges(df)
  result = liftOver(gr,chain)
  chr=gsub("chr","",as.data.frame(seqnames(result))$value)
  pos=as.data.frame(ranges(result))[,c("start")]
  
  vcf[,2]=pos
  vcf
}

df = data.frame(chr=c(19,19),start=c(6668972,6895866),stop=c(6668972,6895866))
grc37_to_grc38(df)

vcf = read.table("/data/opt/snpEff/gwas.vcf",sep="\t", header=T, stringsAsFactors = F, comment.char="")
write.table(overlift_vcf(vcf),"/data/opt/snpEff/gwas_38.vcf",sep="\t",quote=F, row.names=F)


