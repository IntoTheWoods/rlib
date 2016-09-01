sql.tables = function() {
  my.tables=dbGetQuery(db, "SELECT name FROM sqlite_master WHERE type='table';")
  for (item in names(dbattach)) {
    my.tables=c(my.tables, dbGetQuery(db,paste("select name from ",item,".sqlite_master where type='table';",sep="")))
  }
  names(my.tables) = c("db",names(dbattach))
  my.tables
}


