sql.query = function(sql, bind.data=NULL, row.names=T) {

  library(RSQLite)

  ## bind.data if necessary
  if (!is.null(bind.data)) {
    sql = sql.bind(sql, bind.data)
  }

  ## check if db exists and create in memory RSQLite database otherwise
  if (!exists("db") || ! RSQLite::dbIsValid(db) ) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
    temp.db = TRUE
  } else {
    temp.db = FALSE
  }
  ## parse sql to identify table references that only exist as data.frames
  ## store this
  ## don't include ( here because then from will select the first word in subqueries 
  words = unlist(strsplit(sql," |\n|)"))
  words = words[words!=""]
  test.tables=c()
  
  ## allow queries to use attached tables with unique names
  #tables = RSQLite::dbListTables(db)
  database.list=dbGetQuery(db, "PRAGMA database_list")$name
  database.list=database.list[database.list!="temp"]
  tables = c()
  for (database in database.list) {
    tables=c(tables,dbGetQuery(db, paste('SELECT name FROM ',database,'.sqlite_master WHERE type="table"'))$name)
  }

  counter=1
  for (word in words) {
    if (word %in% c("join","JOIN","from","FROM") & ! substr(words[counter+1],1,1) %in% c("(") ) {
      test.object = words[counter+1]
      test.table = r2sql(test.object)
      if(! test.table %in% tables & exists(test.object) && is.data.frame(get(test.object))) {
        #warning(paste("Table ",test.table," exists only as data frame. Storing as temporary table for query."),immediate. = TRUE, call. = FALSE)
        test.tables = c(test.tables,r2sql(test.table))
        RSQLite::dbWriteTable(db, test.table, get(test.object),overwrite=T)
        sql=gsub(test.object,test.table,sql)
      } else if (! test.table %in% tables ) {
        stop(paste("ERROR: test table",test.table,"not found in db"))
        return(NULL)
      }
    }
    counter=counter+1
  }

  ## run query
  result=RSQLite::dbGetQuery(db, sql)

  ## if the first column is called row_names, use this as the data.frame rownames
  if (sum(colnames(result) %in% "row_names") > 0 & row.names) {
    rownames(result) = result$row_names
    result$row_names = NULL
  }

  ## clean up temporary tables
  ## RSQLite does not have temporary table support yet
  for (test.table in unique(test.tables)) {
    RSQLite::dbGetQuery(db, paste("drop table ",test.table))
  }

  ## close temporary database
  if(temp.db) {
    RSQLite::dbDisconnect(db)
  }

  result
}
