workspace.initialize = function() {
  ##
  ## initialization 
  ##
  
  ## remove stale database to prevent database locking if the script doesn't finish successfully
  if (exists("db",envir=globalenv())) {
    library(RSQLite)
    RSQLite::dbDisconnect(db) }
  
  ## detach stale script.config
  if (sum(grepl("^script.config$",search())) == 1) {
    detach(script.config)
  }
    
  ## clear Rstudio plots
  ## This prevents the error:
  ## Error in par(op) : invalid value specified for graphical parameter "pin"
    
  if( !is.null(dev.list()["RStudioGD"]) ) { dev.off(dev.list()["RStudioGD"]) }
    
  ## clear data space, attach configuration data, and load libraries
    
  rm(list=ls(globalenv())[which(ls(globalenv()) != "script.config")], envir=globalenv())

  if (exists("script.config",envir=globalenv())) {

    ## quick fix paths depending on OS
    operating.system = Sys.info()['sysname']
    if (operating.system == "Windows") {
      script.config = lapply(script.config,function(x) { if(class(x) == "character") { gsub("~","Z:",x) } else {x} } ) 
    } else if (operating.system == "Linux") {
      script.config = lapply(script.config,function(x) { if(class(x) == "character") { gsub("Z:","~",x) } else {x} } )
    }

    attach(script.config)

    for (item in script.config$libraries) {
      library(item, character.only=T, logical.return=T)
    }  

    ## session preparation
    ## sets project.directory, backs up and copies script file
   
    source.file = paste(project.dir,source.file,sep="/")
    ## define base library for dependencies and destination R script
    lib.dir=system.file(package="libbase") 
    dest.dir = "./scripts"
    dest.bak.extension = "bak"
    session.file = "sessionInfo.txt"
    dest.file=paste(dest.dir,"workflow.R",sep="/")  

    ## change project.directory
    setwd(project.dir)

    ## create scripts directory
    dir.create(file.path(project.dir, dest.dir), showWarnings = FALSE)

    ## copy libbase library to scripts directory
    file.copy(from=lib.dir, to=dest.dir,recursive=T, overwrite=T)

    ## copy workflow.R to scripts directory, creating a backup copy with a uuid
    ## note that all.equal returns TRUE or a vector, NOT T/F
    if (file.exists(dest.file)) {
     if ( ! all.equal(readLines(source.file),readLines(dest.file))[1]==T) {
        uuid=uuid::UUIDgenerate()
        print(uuid)
        file.rename(from=dest.file, to=paste(dest.file,dest.bak.extension,uuid,sep="."))
      }
    }

    file.copy(from=source.file, to=dest.file, overwrite=T)

    ## save R session info
    writeLines(capture.output(sessionInfo()), paste(dest.dir,session.file,sep="/"))

  }

  ##
  ## initialize database and sqlite pragma
  
  if (exists("dbname",envir=globalenv())) {

    library(RSQLite)
    db <<- RSQLite::dbConnect(SQLite(), dbname=paste(project.dir,dbname,sep="/"))
  
    if (!exists("db.pragma",envir=globalenv()) || db.pragma ) {
      dbGetQuery(db, 'PRAGMA synchronous = OFF;')
      dbGetQuery(db, 'PRAGMA journal_mode = OFF;')
      dbGetQuery(db, 'PRAGMA locking_mode = EXCLUSIVE;')
      dbGetQuery(db, 'PRAGMA temp_store = MEMORY;')
      dbGetQuery(db, 'PRAGMA count_changes = OFF;')
      dbGetQuery(db, 'PRAGMA PAGE_SIZE = 4096;')
      dbGetQuery(db, 'PRAGMA default_cache_size=7000000;')
      dbGetQuery(db, 'PRAGMA cache_size=5000000;')
      dbGetQuery(db, 'PRAGMA compile_options;')
    } 
  }

  if (exists("db",envir=globalenv()) && exists("dbattach",envir=globalenv())) {
    for (item in dbattach) {
      item.name =  gsub(".*/","",gsub("\\.sqlite","",item))
      dbGetQuery(db, paste('attach ','"',item,'"',' as ', item.name, sep="") )
    }
  }
  
}
