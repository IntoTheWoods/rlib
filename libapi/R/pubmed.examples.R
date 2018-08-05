pubmed.examples = function() {
system('wget -qO- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?dbfrom=pubmed&term=cancer&retmax=999999999"')

system('curl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&rettype=abstract&id=25081398"')

system('curl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=abstract&id=25081398"')
system('curl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=text&rettype=abstract&id=25081398"')

}
