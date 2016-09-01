library(libbase)

{ ## config

script.config =  list(
  
  project.dir                = "~"
  , 
  data.source.dir            = "~"
  ,
  gene.set.dir               = "~"
  ,
  libraries                  = c("doMC","itertools","foreach")
  ,
  source.file                = "scripts/build_database.R"
  ,
  dbname                     = "database.sqlite"
  ,
  ncores                     = 32
  
)

} ## config

libbase::initialize.workspace()

registerDoMC(ncores)

if (FALSE) {
  for (table.name in c("design", "contrasts")) {
    dirs=list.dirs(data.source.dir,recursive=F)
    data.files=list.files(path=dirs,pattern=paste(table.name,".tsv$",sep=""),full.names=T)
    libbase::sql.create.table.rbind(table.name, data.files, include.path=T)
  }
  
  design=sql.query("select * from design")
  contrasts=sql.query("select * from contrasts")
  
  contrasts=cbind(contrasts,t(matrix(unlist(strsplit(contrasts$contrast,"-")),nrow=2)))
  colnames(contrasts) = c("contrast","file_path","condition_2","condition_1")

  design.width = length(colnames(design))
  contrasts.width = length(colnames(contrasts))
  break1 = contrasts.width+1
  break2 = contrasts.width+design.width
  break3 = contrasts.width+2*design.width
  
  dbWriteTable(db,"contrasts",contrasts, overwrite=T)
  
  contrasts=sql.query("select * from (select * from contrasts left join design on contrasts.condition_2 = design.condition) as tmp left join design on tmp.condition_1 = design.condition")
  colnames(contrasts)[break1    :break2]=paste("condition_2_",colnames(contrasts)[break1    :break2],sep="")
  colnames(contrasts)[(break2+1):break3]=paste("condition_1_",colnames(contrasts)[(break2+1):break3],sep="")

  ## remove duplicate conditions
  contrasts$condition_2_sample=NULL
  contrasts$condition_1_sample=NULL
  contrasts=unique(contrasts)
      
  dbWriteTable(db,"contrasts",contrasts, overwrite=T)

} ## build design and contrasts tables

if (FALSE) {
  dirs=paste(list.dirs(data.source.dir,recursive=F),"/limma",sep="")
  data.files=list.files(path=dirs,pattern="limma.result.*.tsv$",full.names=T)
  libbase::sql.create.table.rbind("limma",data.files, include.path=T)
  
  ## add contrast and directory columns to limma
  limma=sql.query("select * from limma", row.names=F)
  limma$contrast=gsub(".*limma.result.|.tsv$","",limma$file_path)
  limma$directory=gsub("/.*","",gsub(paste(data.source.dir,"/",sep=""),"",limma$file_path))
  dbWriteTable(conn=db, name="limma_contrast", value=limma)
  
  ## build wide tables
  dbWriteTable(db, "limma_fdr",long2wide(limma[,c("row_names","contrast","adj_P_Val")], "row_names", "contrast", "adj_P_Val"), overwrite=T)
  dbWriteTable(db, "limma_logfc",long2wide(limma[,c("row_names","contrast","logFC")], "row_names", "contrast", "logFC"), overwrite=T)
  rm(limma)
  
} ## build limma table

if (FALSE) {
  ## we can't use a library function here because the column names require symbol conversions
  
  sub.dir="gsea/contrasts"
  dirs=paste(list.dirs(data.source.dir,recursive=F),sub.dir,sep="/")
  data.files=list.files(path=dirs, pattern="*.txt",full.names=T)
  
  gene.sets = c("c6.all.v5.0.symbols",  "msigdb.v5.0.symbols")
  items = c("fdr","nes")
  stat.methods = c("tstat","logFC")
  
  #items = c("fdr")
  #stat.methods = c("tstat")
  
  for (item in items) {
    for (stat.method in stat.methods) {
      for (gene.set in gene.sets) {
        
        rm(my.table)
        
        file.name=paste(item,"_contrasts_",stat.method,"_",gene.set,".txt", sep="")
        table.name=r2sql(paste("gsea_",item,"_",stat.method,"_",gene.set, sep=""))
        
        data.files.filtered=data.files[grepl(file.name, data.files)]
        data.files.filtered
        
        print("Reading data...")
        for (my.file in data.files.filtered) {
          print(my.file)
          my.data = read.table(my.file,header=T,sep="\t",stringsAsFactors=F, row.names=NULL,quote="")
          ## gsea changes hyphens to underscores in contrast names, we need to convert back
          colnames(my.data) = gsub("_","-",colnames(my.data))
          colnames(my.data) = r2sql(colnames(my.data))
          
          if (exists("my.table")) {
            my.data$set = NULL
            my.table=cbind(my.table, my.data)
          } else {
            my.table = my.data
            #rownames(my.table) = my.table$set
            #my.table$set = NULL
          }
        }
        
        RSQLite::dbWriteTable(db, table.name, my.table, overwrite=T)
      }
    }
  }
} ## build gsea from txt

if (FALSE) {

  gene.set.collections = c("custom001.c6.all.v5.0.symbols",  "c6.all.v5.0.symbols",  "msigdb.v5.0.symbols", "custom002")
  #gene.sets = gsub("\t.*","",readLines(paste(gene.set.dir,"/custom002.gmt",sep="")))
  #stat.methods = c("tstat","logFC")
  
  gene.set.collections=c("custom001.c6.all.v5.0.symbols")
  #gene.sets = c("RAF_UP.V1_DN")
  stat.methods = c("tstat")

  sub.dir="gsea/contrasts"
  dirs=paste(list.dirs(data.source.dir,recursive=F),sub.dir,sep="/")
  data.files=list.files(path=dirs, pattern="*.rnk",full.names=T)
  
  
  if (exists("gsea")) { rm(gsea) }

  gsea <- foreach (stat.method=stat.methods, .combine='rbind') %do% {
    
    data.files.filtered = data.files[grep(stat.method, data.files)]
    
    foreach (data.file=data.files.filtered, .combine='rbind') %dopar% {
      
      gsea.rank = read.table(data.file, header=T,stringsAsFactors=F,quote="",sep="\t")
      
      foreach (gene.set.collection=gene.set.collections, .combine='rbind') %do% {
        
        results.file = paste(gsub(".rnk","",gsub("/gsea\\.","/gsea_results.gsea.",data.file)),"/",gene.set.collection,"/edb/results.edb",sep="")
        
        #print(results.file)
        
        ## read all gene sets
        edb.matrix  = matrix(gsub("^ ","",gsub("  "," ",gsub("=","",unlist(strsplit(gsub("<DTG |/>","",grep("<DTG", readLines(results.file), value=T)),"\""))))),ncol=26,byrow=T)
        ## read selected gene sets
        #edb.matrix  = matrix(gsub("^ ","",gsub("  "," ",gsub("=","",unlist(strsplit(gsub("<DTG |/>","",grep(paste("GENESET=\"gene_sets.gmt#",gene.sets,"\"",sep="",collapse="|"), readLines(results.file), value=T)),"\""))))),ncol=26,byrow=T)
        
        edb.df = as.data.frame(edb.matrix[,seq(2,26,2),drop=F],stringsAsFactors=F)
        colnames(edb.df)=edb.matrix[1,seq(1,26,2)]
          
        edb.df$COLLECTION = gene.set.collection
        edb.df$STAT_METHOD = stat.method
        edb.df$RNK_FILE = data.file
        edb.df$CONTRAST = gsub("_","-",gsub(paste(".",stat.method,".rnk",sep=""), "", gsub(".*gsea.", "", edb.df$RNK_FILE)))
        edb.df$EDB_FILE = results.file
        edb.indices=lapply(strsplit(edb.df$HIT_INDICES," "),as.numeric)
        edb.df$GENE=lapply(edb.indices, function(x) { gsea.rank[x+1,1]})
        edb.df$GENE_RANK_SCORE=lapply(edb.indices, function(x) { gsea.rank[x+1,2]})

        edb.df
        
      }
    }
  }

  gsea$GENESET= gsub(".*#","",gsea$GENESET)
  gsea$GENE =  unlist(lapply(gsea$GENE,paste,collapse=" "))
  gsea$GENE_RANK_SCORE =  unlist(lapply(gsea$GENE_RANK_SCORE,paste,collapse=" "))
  gsea$RND_ES = NULL
  gsea$DIRECTORY = gsub("/.*","",gsub(paste(data.source.dir,"/",sep=""),"",gsea$RNK_FILE))
  gsea$ES=as.numeric(gsea$ES)
  gsea$NES=as.numeric(gsea$NES)
  gsea$NP=as.numeric(gsea$NP)
  gsea$FDR=as.numeric(gsea$FDR)
  gsea$FWER=as.numeric(gsea$FWER)
  gsea$RANK_AT_ES=as.numeric(gsea$RANK_AT_ES)
  gsea$RANK_SCORE_AT_ES=as.numeric(gsea$RANK_SCORE_AT_ES)

  colnames(gsea)=tolower(colnames(gsea))
  
  dbWriteTable(conn = db, name = "gsea", value = gsea, overwrite=T)

  ## write wide tables  
  dbWriteTable(db, "gsea_fdr",long2wide(gsea[,c("geneset","contrast","fdr")], "geneset", "contrast", "fdr"), overwrite=T)
  dbWriteTable(db, "gsea_nes",long2wide(gsea[,c("geneset","contrast","nes")], "geneset", "contrast", "nes"), overwrite=T)
  
  
} ## build gsea from edb

if (FALSE) {

  #print("getting gsea table length...")
  #gsea.length = sql.query("select count(geneset) from gsea", row.names=F)

  
  if (!exists("gsea")) {
    print("reading gsea table...")
    gsea = sql.query("select * from gsea", row.names=F)
    gsea.length = length(gsea[,1])
  }
      
  print("building gsea.genes...")
  gsea.genes <- foreach (my.rows=isplitVector(1:gsea.length,chunks=ncores), .combine='rbind') %dopar% {
    
    ## read only the subset of gsea needed for this chunk
    #gsea=sql.query(paste("select * from gsea limit ",length(my.rows)," offset ",min(my.rows)-1))
  
    foreach (my.row=1:length(my.rows), .combine='rbind') %do% {
      cbind(gsea$contrast[my.row],
            gsea$ranked_list[my.row], 
            gsea$stat_method[my.row], 
            gsea$rnk_file[my.row],
            gsea$edb_file[my.row], 
            gsea$directory[my.row], 
            gsea$collection[my.row], 
            gsea$geneset[my.row], 
            gsea$rank_at_es[my.row], 
            gsea$rank_score_at_es[my.row],
        as.character(unlist(strsplit(gsea$gene[my.row]," "))),
        as.numeric(unlist(strsplit(gsea$hit_indices[my.row]," "))),
        as.numeric(unlist(strsplit(gsea$gene_rank_score[my.row]," "))),
        as.numeric(unlist(strsplit(gsea$es_profile[my.row]," ")))
      )
    }
  }
  
  print("writing tmp RDS...")
  saveRDS(gsea.genes,file="gsea_genes_tmp.RDS")
  
  print("cleaning up gsea.genes...")
  colnames(gsea.genes) = c("contrast","ranked_list","stat_method","rnk_file","edb_file","directory","collection","geneset","rank_at_es","rank_score_at_es","gene","hit_indices","gene_rank_score","es_profile")
  gsea.genes = as.data.frame(gsea.genes, stringsAsFactors=F)
  gsea.genes$rank_at_es = as.integer(gsea.genes$rank_at_es)
  gsea.genes$hit_indices = as.integer(gsea.genes$hit_indices)
  gsea.genes$rank_score_at_es = as.numeric(gsea.genes$rank_score_at_es)
  gsea.genes$gene_rank_score = as.numeric(gsea.genes$gene_rank_score)
  gsea.genes$es_profile = as.numeric(gsea.genes$es_profile)
  
  gsea.genes$leading_edge = ifelse ( (gsea.genes$rank_score_at_es < 0 & gsea.genes$hit_indices >= gsea.genes$rank_at_es ) |
                                           (gsea.genes$rank_score_at_es > 0 & gsea.genes$hit_indices <= gsea.genes$rank_at_es ), sign(gsea.genes$rank_score_at_es), 0)
  
  
  gsea.genes$leading_edge_score = -1*gsea.genes$leading_edge*gsea.genes$gene_rank_score
  
  print("writing gsea_genes table to database...")
  dbWriteTable(conn = db, name = "gsea_genes", value = gsea.genes, overwrite=T)

} ## build leading edge tables

## create indices
# create index index_gsea_genes_geneset on gsea_genes (geneset);
