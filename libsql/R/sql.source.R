sql.source = function(file_name) {
  dbDisconnect(db)
  source(file_name, local=new.env())
  sql.reconnect()
}
