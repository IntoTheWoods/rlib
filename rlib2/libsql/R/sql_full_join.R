## cannot incorporate sql statements
sql_join = function(table1, table2, key) {

test = sql.query(paste0("select ",table1,".*, ",table2,".* from ",table1," join ",table2," using(",key,") 
                     union all select ",table1,".*, ",table2,".* from ",table1," left join ",table2," using(",key,") where ",table2,".",key," is NULL
                     union all select ",table1,".*, ",table2,".* from ",table2," left join ",table1," using(",key,") where ",table1,".",key," is NULL"))

colnames(test)[duplicated(colnames(test))] = gsub("$","__2",colnames(test)[duplicated(colnames(test))])
test[,key]=ifelse(is.na(test[,key]),test[,paste0(key,"__2")],test[,key])

}


