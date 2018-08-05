workspace.finalize = function(disconnect=T) {
  ##
  ## clean up workspace after analysis is complete
  ##
  
  ## close and write to pdf file if selected as device
  if (exists("plot_pdf",envir=globalenv()) && plot_pdf && names(dev.cur())=="pdf") {
    dev.off()
  }

  ## remove stale database to prevent database locking if the script doesn't finish successfully
  if (exists("db",envir=parent.frame(),inherits=F) && disconnect) {
    library(RSQLite)
    RSQLite::dbDisconnect(parent.frame()$db)
  }

}
