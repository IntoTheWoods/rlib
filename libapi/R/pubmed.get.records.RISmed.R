pubmed.get.records.RISmed = function(input) {
  
  ## check if input can be coerced to an integer vector
  if( suppressWarnings(!all(!is.na(as.integer(input)))) ) {
    r <- RISmed::EUtilsSummary(input, type='esearch', db='pubmed', retmax=99999)
    #print(summary(r))
    pmids = RISmed::QueryId(r)
  } else {
    pmids = input
  }
  
  records= RISmed::EUtilsGet(pmids)
  
  data.frame(
    'Title'=ArticleTitle(records),
    'Journal'=Title(records),
    'Year'=YearPubmed(records),
    'Link'=paste('http://www.ncbi.nlm.nih.gov/pubmed/',PMID(records),sep=""),
    'PMID'=PMID(records),
    stringsAsFactors = F
  )
  
}
