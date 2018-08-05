sql.bind = function(sql,bind.data=list()) {
  if (class(bind.data) != "list") {
    bind.data=list(bind.data)
  }

  for (item in bind.data) {
    if (class(item)!="numeric") {
      options(useFancyQuotes = F)
      item=dQuote(item)
    }
 
    item=paste(item,collapse=",")
    sql = sub("\\?",item,sql)
  }
  sql
}


