sql.schema = function(table.name) {
  library(RSQLite)
  table.name = gsub("\\.","_",table.name)
  schema=as.list(RSQLite::dbGetQuery(db, paste("select * from sqlite_master where name = \"",table.name,"\"",sep="")))
  schema.fieldpairs = unlist(strsplit(gsub(" ","=",gsub("^ | $","",gsub(".*\\(|\"|\\n\\)|\\n\\t","",schema$sql))),",|="))
#  schema$fields = schema.fieldpairs[seq(2,length(schema.fieldpairs),2)]
#  names(schema$fields) = schema.fieldpairs[seq(1,length(schema.fieldpairs),2)]
  schema$fields=data.frame(name= schema.fieldpairs[seq(1,length(schema.fieldpairs),2)], value=schema.fieldpairs[seq(2,length(schema.fieldpairs),2)], stringsAsFactors=F)
  schema
}
