pubmed.get.citation.pmids = function(pmids) {
  ids=paste("&id=",paste(pmids,collapse="&id="),sep="")
  wget_string=paste("wget -qO- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin",
                    ids,
                    "&tool=my_tool&email=my_email@example.com'",sep="")
  s = system(wget_string,intern=TRUE)
  citations=gsub(".*<Id>|</Id>","",grep("</Id>",s[grep("/IdList",s):length(s)],value=T))
  citations
}