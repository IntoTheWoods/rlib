sql.reconnect = function() {
 
  ## remove stale database to prevent database locking if the script doesn't finish successfully
  if (exists("db",envir=parent.frame(),inherits=F)) {
    library(RSQLite)
    RSQLite::dbDisconnect(parent.frame()$db) 
  }
  
  ##
  ## initialize database and sqlite pragma

  if (exists("dbname",envir=parent.frame(),inherits=F)) {

    ## check if absolute or relative path
    if (grepl("^/",parent.frame()$dbname)) {
      full_dbname = parent.frame()$dbname
    } else {
      full_dbname = paste(parent.frame()$project.dir,parent.frame()$dbname,sep="/")
    }

    ## I think <<- assigns to the global env if the variable is not present in the parent env
    ## so we use assign here instead to use the parent env

    assign("db", RSQLite::dbConnect(RSQLite::SQLite(), dbname=full_dbname), envir=parent.frame(), inherits=F)
  
    if (!exists("db.pragma",envir=parent.frame(), inherits=F) || parent.frame()$db.pragma ) {
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA synchronous = OFF;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA journal_mode = OFF;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA locking_mode = EXCLUSIVE;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA temp_store = MEMORY;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA count_changes = OFF;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA PAGE_SIZE = 4096;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA default_cache_size=7000000;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA cache_size=5000000;')
      RSQLite::dbGetQuery(parent.frame()$db, 'PRAGMA compile_options;')
    } 
  }

  if (exists("dbattach",envir=parent.frame(), inherits=F)) {
    if (! exists("dbname",envir=parent.frame(), inherits=F)) {
      assign("db", RSQLite::dbConnect(RSQLite::SQLite(), ":memory:"),envir=parent.frame(), inherits=F)
    }
    for (item in names(dbattach)) {
      RSQLite::dbGetQuery(parent.frame()$db, paste('attach ','"',dbattach[[item]],'"',' as ', item, sep="") )
    }
  }

}


