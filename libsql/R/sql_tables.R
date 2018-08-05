sql.tables = function(db=parent.frame()$db) {
  my.tables=c()
  database.list=dbGetQuery(db, "PRAGMA database_list")$name
  database.list=database.list[database.list!="temp"]
  for (item in database.list) {
    my.tables=c(my.tables, dbGetQuery(db,paste("select name from ",item,".sqlite_master where type='table';",sep="")))
  }
  names(my.tables) = database.list
  my.tables
}
