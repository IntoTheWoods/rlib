sql.create.table.rbind = function(table.name, data.files, include.path=F) {
  library(RSQLite)
  ## get unique column names across all files
  table.header=character(0)
  print("Reading headers...")
  for (my.file in data.files) {
    print(my.file)
    table.header=c(table.header, colnames(read.table(my.file,sep="\t", header=T, nrows=2, row.names=NULL)))
  }
  table.header = r2sql(unique(table.header))
  if (include.path) {
    table.header = c(table.header, "file_path")
  }
  
  ## create an empty data.frame with columns of class "character" which prevents data loss upon loading
  ## a really convoluted way to get a character data.frame with zero rows
  #my.table= data.frame(matrix(ncol = length(table.header), nrow = 0))
  my.table=do.call(data.frame, list(sapply(data.frame(matrix(ncol = length(table.header), nrow = 0)),as.character), stringsAsFactors=F))
  colnames(my.table) = table.header
  RSQLite::dbWriteTable(db, table.name, my.table, overwrite=T)
  
  ## read individual files, pad missing columns with NA values, and append
  print("Reading data...")
  for (my.file in data.files) {
    my.data = read.table(my.file,header=T,sep="\t",stringsAsFactors=F, row.names=NULL)
    colnames(my.data) = r2sql(colnames(my.data))
    my.data.padded = cbind(my.data, matrix(ncol=sum(!table.header %in% colnames(my.data)),nrow=length(my.data[,1])))
    colnames(my.data.padded)=c(colnames(my.data),table.header[!table.header %in% colnames(my.data)])
    if (include.path) {
      my.data.padded$file_path = my.file
    }
    print(my.file)
    ## as.data.frame prevents the data.frame from being collapsed into a vector with a single column
    RSQLite::dbWriteTable(db, table.name, as.data.frame(my.data.padded[,match(table.header, colnames(my.data.padded))]), append=T)
  }
  
  ## read table and fix type conversions (as.is flag prevents char from being converted to factor)
  print("Type conversions...")
  my.table = sql.query(paste("select * from ",table.name), row.names=F)
  for (i in 1:length(colnames(my.table))) {
    my.table[,i]=type.convert(my.table[,i], as.is=T)
  }
  ## re-save table, now with the correct column classes
  RSQLite::dbWriteTable(db, table.name, my.table, overwrite=T)
  
}
