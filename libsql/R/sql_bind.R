sql.bind = function(sql,bind.data=list()) {
  if (class(bind.data) != "list") {
    bind.data=list(bind.data)
  }
  for (item in bind.data) {
    item=paste(item,collapse=",")
    sql = sub("\\?",item,sql)
  }
  sql
}


