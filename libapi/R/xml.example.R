xml.example = function() {
  library(XML)
  xml.url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&rettype=abstract&id=25081398,27146558"
  xmlfile <- xmlTreeParse(xml.url)
  xmltop = xmlRoot(xmlfile)
  print(xmltop)[1:2]
  data <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
  data <- data.frame(t(data),row.names=NULL)

  xml.attrib = (xmlSApply(xmltop[[1]],xmlAttrs))
  xml.attrib$Id=NULL
  xml.attrib = as.data.frame(xml.attrib)
  colnames(data) = c("Id",as.character(t(xml.attrib[1,])))
  
  data[1:5,1:4]  
  
}

