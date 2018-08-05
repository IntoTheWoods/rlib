pubmed.get.sorted.records = function(input) {
  
  ## check if input can be coerced to an integer vector
  if( suppressWarnings(!all(!is.na(as.integer(input)))) ) {
    r <- RISmed::EUtilsSummary(input, type='esearch', db='pubmed', retmax=99999)
    #print(summary(r))
    pmids = RISmed::QueryId(r)
  } else {
    pmids = input
  }
  
  records= RISmed::EUtilsGet(pmids)
  ## use loop here instead of sapply to avoid server error
  #record.counts = sapply(pmids, pubmed.get.citation.count)
  record.counts=c()
  for (pmid in pmids) {
    record.counts=c(record.counts,pubmed.get.citation.count(pmid))
  }
  
  pubmed_data <- data.frame(
    'Title'=ArticleTitle(records),
    'Journal'=Title(records),
    'Year'=YearPubmed(records),
    'Link'=paste('http://www.ncbi.nlm.nih.gov/pubmed/',PMID(records),sep=""),
    'PMID'=PMID(records),
    stringsAsFactors = F
  )
  
  pubmed_data = cbind('Citations'=record.counts[match(pubmed_data$PMID,pmids)], pubmed_data)
  pubmed_data[order(pubmed_data$Citations,decreasing=T),]
  
}

