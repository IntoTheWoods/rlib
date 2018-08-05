pubmed.search = function(input) {
 
 wget_string = paste("wget -qO- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?dbfrom=pubmed&retmax=999999999&term=",input,"' | grep \\<Id\\> | awk -F'<|>' '{print $3}'")
 system(wget_string,intern=T)

}
