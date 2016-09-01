sql.tables = function() {
  my.tables=dbGetQuery(db, "SELECT name FROM sqlite_master WHERE type='table';")
  for (item in dbattach) {
    item.name =  gsub(".*/","",gsub("\\.sqlite","",item))
    my.tables=c(my.tables, dbGetQuery(db,paste("select name from ",item.name,".sqlite_master where type='table';",sep="")))
  }
  names(my.tables) = c("db",dbattach)
  my.tables
}


