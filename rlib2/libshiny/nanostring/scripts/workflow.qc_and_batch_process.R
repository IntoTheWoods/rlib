workingDirectory      <- "/data/opt/rlib/libshiny/nanostring"          ## working directory (e.g. "~")
repositoryLocation    <- "/data/opt/r-script-nano-clin"                                    ## location of the jtx-2011-101/r-script-nano repository (e.g. K:/JTX-2011/101/Code/R)
ZIPpath               <- "."                                                          ## location of the zipped RCC files (e.g. R:/Public/Translational/Nanostring/Human/ZIP)
ZIPpattern            <- "*"                                                               ## ZIP file name pattern to match for test run (e.g. MITRA)
additionalLibraryPath <- "K:/JTX-2011/101/Libraries/R"                                     ## search path for additional libraries
cartridgeType         <- 638                                                               ## previous default was "human_immuneJTX"
scaleCenter           <- 2^10                                                              ## scaling value for normalization, sometimes 2^10.1?


## confirm that you have access to the RCC
if(!dir.exists(ZIPpath) | !dir.exists(repositoryLocation)) {
  cat('Please check the mapping of the zipped RCC files and the r-script-nano repository - if you do not have access we cannot initiate this script')
}

# create a working directory if necessary
if(!dir.exists(workingDirectory)) {
  dir.create(workingDirectory)
  cat(paste0('\nCreating working directory (',workingDirectory,')\n'))
}

setwd(workingDirectory)

if(!dir.exists('ZIP')) {
  dir.create('ZIP')
  cat('\nCreating ZIP directory\n')
}


# Install NanoStringNorm if it isn't installed, without forcing updates
.libPaths(c(additionalLibraryPath, .libPaths()))
missingNSN <- !'NanoStringNorm' %in% rownames(installed.packages())
if(missingNSN) {
  source("https://bioconductor.org/biocLite.R")
  # this should only install NanoStringNorm, and not break other dependencies you have
  biocLite('NanoStringNorm', suppressUpdates = T, suppressAutoUpdate = T)
  cat('Installed the NanoStringNorm library\n')
}

## load NanoStringNorm library
library(NanoStringNorm)

# move to newly created ZIP folder & copy a subset of ZIPs here
cat('Copying ZIP files from R\n')
files <- dir(path = ZIPpath, pattern = ZIPpattern)
file.copy(from = paste(ZIPpath, files, sep = '/'), to = './ZIP')

cat(paste('Loading functions in ',paste(repositoryLocation, 'pipeline.R', sep = '/'),' &\n       ', paste(repositoryLocation, 'Utils.R', sep = '/'), '\n', sep =''))
source(paste(repositoryLocation, 'pipeline.R', sep = '/'))

# source the Utils.R function
source(paste(repositoryLocation, '/Utils.R', sep = ''))

cat('Running NS.QC to QC the files in the ZIP folder\n')
NS.QC(QCpath = './QC/',
      zipPath = './ZIP',
      RCCPath = './RCC/',
      cartridgeType = cartridgeType,
      scaleCenter = scaleCenter,
      MaskSI = F,
      Codesets = paste(repositoryLocation, 'Codesets.R', sep = '/'),
      exclude = NULL)

cat('Running NS.Batch to create expression files based on the files that were analyzed with NS.QC\n')
NS.Batch(BatchPath = 'Batch',
         Files = paste('QC/Nanostring.QCData.', Sys.Date(), '.txt', sep = ''),
         FileColumn = 'FilePath',
         cartridgeType = cartridgeType,
         scaleCenter = scaleCenter,
         MaskSI = F,
         Codesets = paste(repositoryLocation, 'Codesets.R', sep = '/'),
         exclude = NULL,
         fl = T,
         flLevel = .9)

cat('\n All of the code has finished running. Please find your results in ',workingDirectory,' \n')

sink()


