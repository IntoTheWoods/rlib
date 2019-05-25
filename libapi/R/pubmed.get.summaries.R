pubmed.get.summaries = function(pmids) {
  library(XML)

  ## divide pmids into groups so as not to exceed the maximum url length
  max.pmids=200
  num.groups = ceiling(length(pmids)/max.pmids)
  pmid.groups = split(pmids,factor(1:length(pmids)%%num.groups))
  
  data.xml = xmlNode(name="pubmed")
  for (pmids in pmid.groups) {
    pmid_string = paste(pmids,collapse=",")
    xml.url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&rettype=abstract&id=",pmid_string,sep="")
    output = xmlRoot(xmlTreeParse(httpGET(xml.url)))
    data.xml=append.xmlNode(data.xml,xmlChildren(output))
    Sys.sleep(0.2)
  }
 
  ## list of xml nodes 
  data <- xmlSApply(data.xml, function(x) x)

  ## convert each xml node to a character vector
  for (i in 1:length(data)) {
    values = as.character(xmlSApply(data[[i]],xmlValue))
    names(values) = as.character(xmlSApply(data[[i]], function(x) xmlAttrs(x)[[1]]))
    data[[i]] = values
  }

  data=as.data.frame.list.mod(data, row.names=NULL)
  data$PmcRefCount = as.numeric(as.character(data$PmcRefCount))
  data$PubYear = substr(data$PubDate,1,4)
  data[order(data$PmcRefCount, decreasing=T),]

}

