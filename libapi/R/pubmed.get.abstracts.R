pubmed.get.abstracts = function(pmids) {
  library(XML)

  ## divide pmids into groups so as not to exceed the maximum url length
  max.pmids=200
  num.pmids=length(pmids)
  num.groups = ceiling(num.pmids/max.pmids)
  pmid.groups = split(pmids,factor(1:num.pmids%%num.groups))
  
  data.xml = xmlNode(name="pubmed")
  i = 0
  for (pmids in pmid.groups) {
    i=i+max.pmids
    print(paste(i,"/",num.pmids,sep=""))
    pmid_string = paste(pmids,collapse=",")
    xml.url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&rettype=abstract&id=",pmid_string,sep="")
    output = xmlRoot(xmlTreeParse(httpGET(xml.url)))
    data.xml=append.xmlNode(data.xml,xmlChildren(output))
    #output = xmlTreeParse(system(paste('wget -qO- --ignore-length "',xml.url,'"',sep=""),intern=T))
  }

  saveRDS(data.xml, "~/data.xml.RDS")

  #journal=sapply(getNodeSet(data.xml,"//Journal/Title"),xmlValue),
  #year=sapply(getNodeSet(data.xml,"//JournalIssue/PubDate/Year"),xmlValue),
  
  data = data.frame(
    pmid=sapply(getNodeSet(data.xml,"//MedlineCitation/PMID"),xmlValue),
    title=sapply(getNodeSet(data.xml,"//ArticleTitle"),xmlValue),
    abstract=as.character(sapply(getNodeSet(data.xml,"//Abstract"),xmlValue))
  )
  
  data
  
}  


