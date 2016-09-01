sql.create.table = function(file.name, overwrite=F) {
  library(RSQLite)
  
  name  = sub(".txt$|.tsv$|.csv|.xls$","", basename(file.name))
  table.name = r2sql(name)
  
  table.dataframe = read.table(file.name,header=T, sep="\t", quote="", comment.char="", stringsAsFactors=F)
  colnames(table.dataframe) = gsub("\\.","_",colnames(table.dataframe))
  
  if( ! RSQLite::dbExistsTable(db,table.name) | overwrite ) {
    
    ## we need to use a dataframe instead of reading the file directly here because
    ## dbWriteTable does not coerce periods in column names which breaks select queries
    ## see gsub above
    RSQLite::dbWriteTable(conn = db, name = table.name, value = table.dataframe, overwrite=T)
    
    ## BROKEN create index
    ## goal is to not use the grepl statement below and identify the minimum number of columns that give a uniuqe index
    ## up to three columns and index these columns
    table.fields       = RSQLite::dbListFields(db, table.name)
    table.fields.index = table.fields[grepl("Index$|index$", RSQLite::dbListFields(db, table.name))]
    
    table.length = RSQLite::dbGetQuery(db, paste("select count(*) from ",table.name))
    table.schema = sql.schema(table.name)


    ## index indivudual columns
    for (field in table.fields.index) {
      field.length = RSQLite::dbGetQuery(db, paste("select count(*) from ( select distinct ",field," from ",table.name," ) ", sep=""))

      unique.index = ifelse (field.length == table.length, "unique","")
      
      index.name = paste("index",table.name, field, sep="_")
      field.set  = field
      sql = paste("create", unique.index, "index", index.name,  "on", table.name, "(", field.set , ")")
      print(sql)
      RSQLite::dbGetQuery(db, sql)
    }

    #multicolumn index
    #if ( length(table.fields.index) > 1 ) {
    #  sql = paste("create index ", paste("index",table.name,paste(table.fields.index,collapse="_"),sep="_"),  " on ", table.name, " ( ", paste(table.fields.index,collapse=", "), " ) " )
    #  print(sql)
    #  RSQLite::dbGetQuery(db, sql)
    #}

  }
  
  
}
