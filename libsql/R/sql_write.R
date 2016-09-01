## There is a bug in RSQLite (see: https://github.com/rstats-db/RSQLite/issues/56 )
## that keeps the database locked when open for writing
## This wrapper should allow multiple processes to write to the database without crashing
sql.write = function(...) {
  library(RSQLite)
  repeat {
    db <- RSQLite::dbConnect(SQLite(), dbname=paste(project.dir,dbname,sep="/"))
    result <- try(dbWriteTable(db, ...))
    if(!is(result, "try-error")) break
    dbDisconnect(db)
  }
  dbDisconnect(db)
  result
}

sql.write.flock = function(...) {
   library(flock)
   # Take an exclusive lock
   dbfile = paste(project.dir,dbname,sep="/")
   ll = lock(dbfile)
 
   db <- dbConnect(RSQLite::SQLite(), dbname=dbfile)
   dbWriteTable(db, ...)
   dbDisconnect(db)
 
   # Release the lock
   unlock(ll)
}
