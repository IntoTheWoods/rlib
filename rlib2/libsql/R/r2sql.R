r2sql = function(name) {
  name = gsub("[[:punct:]]|[[:space:]]","_",name)
  name = ifelse(grepl("^[[:digit:]]",name),paste("X__",name,sep=""),name)
  ## check for duplicates and convert periods from make.names back to underscores
  gsub("[[:punct:]]","_",make.names(name, unique=T))
  
}
