sql.create.indices = function(table.name, table.fields.index, permute = F, subset = F, individual.only=T) {

    library(gtools)

    ## uuid as field separator
    uuid = "0d843e28-37e8-4e8b-adc9-1bd44cd11af5"

    table.length = RSQLite::dbGetQuery(db, paste("select count(*) from ",table.name))

    sql.indices = dbGetQuery(db, 'select * from sqlite_master where type="index"')

    ## index indivudual columns
    for (field in table.fields.index) {
      field.length = RSQLite::dbGetQuery(db, paste("select count(*) from ( select distinct",field,"from",table.name,")" ))

      create.index = ifelse (field.length == table.length, "create unique index","create index")
      index.name.prefix = ifelse (field.length == table.length,"unique_index","index")
      index.name = paste(index.name.prefix,table.name, field, sep="_")
      field.set  = field
      sql = paste(create.index, index.name,  "on", table.name, "(", field.set , ")")
      if (!(index.name %in% sql.indices$name)) {
        print(sql)
        RSQLite::dbGetQuery(db, sql)
      } else {
        print(paste("Index exists:",index.name))
      }
    }

    ## BROKEN:: DU NOT USE
    if (! individual.only) {
      print("WARNING: INDIVIDUAL.ONLY=F IS BROKEN!!!")
      uuid = "0d843e28-37e8-4e8b-adc9-1bd44cd11af5"
      ## index all pairs
      if ( length(table.fields.index) > 1 ) {
        for (field1 in table.fields.index) {
          for (field2 in table.fields.index) {
            if (field1 != field2) {

              field.length = RSQLite::dbGetQuery(db, paste("select count(*) from ( select distinct ",field1,"||",uuid,"||",field2," from ",table.name," ) ", sep=""))

              create.index = ifelse (field.length == table.length, "create unique index","create index")
              index.name.prefix = ifelse (field.length == table.length,"unique_index","index")
              index.name = paste(index.name.prefix, table.name, field1, field2, sep="_")
              field.set = paste(field1, field2, sep=", ")
              sql = paste(create.index, index.name,  "on", table.name, " ( ", field.set , " ) ", sep="")
              print(sql)
              RSQLite::dbGetQuery(db, sql)
            }
          }
        }
      }
    }


#library(gtools)
#fields = c("a","b")
#fields = c("b","a", "c")

#apply(permutations(n = length(fields), r = length(fields)), 1, function(x) fields[x])
#apply(combinations(n = length(fields), r = length(fields)-1), 1, function(x) fields[x])

}
