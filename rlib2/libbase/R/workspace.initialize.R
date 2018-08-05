workspace.initialize = function(clear_graphics=T) {
  ##
  ## initialization 
  ##
 
  ## remove stale database to prevent database locking if the script doesn't finish successfully
  if (exists("db",envir=parent.frame(),inherits=F)) {
    library(RSQLite)
    RSQLite::dbDisconnect(parent.frame()$db) 
  }
  
  ## detach stale script.config
  ## should be unnecessary with attach.local
  #if (sum(grepl("^script.config$",search())) == 1) {
  #  detach(script.config)
  #}
    
  ## shut down stale graphics device
  ## clear Rstudio plots and/or turn off pdf device in case it is re-opened
  ## This prevents the error:
  ## Error in par(op) : invalid value specified for graphical parameter "pin"
  #if( !is.null(dev.list()["RStudioGD"]) ) { dev.off(dev.list()["RStudioGD"]) }
  #tryCatch({
  #  dev.off()
  #}, warning = function(war) {
  #  print(paste("GRAPHICS_WARNING:  ",war))
  #}, error = function(err) {
  #  print(paste("GRAPHICS_ERROR: ",err))
  #})
  if (names(dev.cur()) != "null device" & clear_graphics) {
    dev.off()
  }
    
  ## clear data space, attach configuration data locally, and load libraries
    
  rm(list=ls(parent.frame())[which(ls(parent.frame()) != "script.config")], envir=parent.frame())

  if (exists("script.config",envir=parent.frame(),inherits=F)) {

    ## quick fix paths depending on OS
    operating.system = Sys.info()['sysname']
    if (operating.system == "Windows") {
      script.config = lapply(parent.frame()$script.config,function(x) { if(class(x) == "character") { gsub("~","Z:",x) } else {x} } ) 
    } else if (operating.system == "Linux") {
      script.config = lapply(parent.frame()$script.config,function(x) { if(class(x) == "character") { gsub("Z:","~",x) } else {x} } )
    }

    libbase::attach.local(parent.frame()$script.config, envir=parent.frame())
    libbase::attach.local(parent.frame()$script.config)

    ## load packages
    for (item in libraries) {
      library(item, character.only=T, logical.return=T)
    }  

    ## session preparation
    ## sets project.directory, backs up and copies script file
   
    source.file = paste(project.dir,source.file,sep="/")
    ## define base library for dependencies and destination R script
    lib.dir=system.file(package="libbase") 
    dest.dir = "./scripts/bak"
    dest.bak.extension = "bak"
    session.file = "sessionInfo.txt"
    dest.file=paste(dest.dir,"workflow.R",sep="/")  

    ## change project.directory
    setwd(project.dir)

    ## create scripts directory
    dir.create(file.path(project.dir, dest.dir), showWarnings = FALSE, recursive=T)

    ## copy libbase library to scripts directory
    file.copy(from=lib.dir, to=dest.dir, recursive=T, overwrite=T)

    ## copy workflow.R to scripts directory, creating a backup copy with a uuid
    ## note that all.equal returns TRUE or a vector, NOT T/F
    if (file.exists(dest.file)) {
     if ( ! all.equal(readLines(source.file),readLines(dest.file))[1]==T) {
        uuid=uuid::UUIDgenerate()
        file.rename(from=dest.file, to=paste(dest.file,dest.bak.extension,uuid,sep="."))
      }
    }

    file.copy(from=source.file, to=dest.file, overwrite=T)

    ## save R session info
    writeLines(capture.output(sessionInfo()), paste(dest.dir,session.file,sep="/"))

  }

  ##
  ## initialize database and sqlite pragma

  if (exists("dbname",envir=parent.frame(),inherits=F)) {

    ## check if absolute or relative path
    if (grepl("^/",dbname)) {
      full_dbname = dbname
    } else {
      full_dbname = paste(project.dir,dbname,sep="/")
    }

    ## I think <<- assigns to the global env if the variable is not present in the parent env
    ## so we use assign here instead to use the parent env

    assign("db", RSQLite::dbConnect(RSQLite::SQLite(), dbname=full_dbname), envir=parent.frame(), inherits=F)

    if (!exists("db.pragma",envir=parent.frame(), inherits=F) || db.pragma ) {
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

  ## load SQL extensions
  if ( exists("db",envir=parent.frame(), inherits=F) && RSQLite::dbIsValid(parent.frame()$db) ) {
    ## enables the "REGEXP" keyword in sqlite
    ## sudo apt-get install sqlite3-pcre
    RSQLite::dbGetQuery(parent.frame()$db, "SELECT load_extension('/usr/lib/sqlite3/pcre.so')")
    ## enables various math functions such as LOG2
    ## from sqlite3.org: extension-functions.c
    RSQLite::dbGetQuery(parent.frame()$db, "SELECT load_extension('/usr/lib/sqlite3/libsqlitefunctions.so')")
  }

  ##
  ## read RDS files
  if (exists("rds_files", envir=parent.frame(), inherits=F)) {
    for (rds_name in names(rds_files)) {
      print(paste0("loading ",rds_name,"..."))
      assign(rds_name, readRDS(rds_files[[rds_name]]), envir=parent.frame(), inherits=F)
    }
  }

  ## set up graphics device
  if (exists("plot_pdf",envir=parent.frame(), inherits=F) && plot_pdf == T) {
    print_options = list (
      file=paste0("output",gsub(".*/workflow|.R$","",source.file),".pdf"),
      width=11,
      height=8.5,
      paper="USr"
    )
    if (exists("pdf_options",envir=parent.frame(), inherits=F)) {
      print_options = replace(print_options, names(pdf_options), pdf_options)
    }
    do.call(pdf,print_options, envir=parent.frame())
  }
  
}
