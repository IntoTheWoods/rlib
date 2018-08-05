sql.tables.db = function() {

  my.tables=dbGetQuery(parent.frame()$db, "SELECT name FROM sqlite_master WHERE type='table';")$name
  database.list=dbGetQuery(parent.frame()$db, "PRAGMA database_list")$name
  database.list=database.list[database.list!="temp"]
  for (item in database.list) {
    attached.tables = dbGetQuery(parent.frame()$db,paste("select name from ",item,".sqlite_master where type='table';",sep=""))$name
    my.tables=c(my.tables, attached.tables)
    my.tables=c(my.tables,paste(item,".",attached.tables,sep=""))
  }

  ## remove lines where there were no tables in the database, e.g. "main." and return the character vector
  grep("\\.$",unique(sort(my.tables)),invert=T,value=T)

}
