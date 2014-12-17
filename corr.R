corr <- function(directory, threshold = 0) {
    
    # directory should be the file containing all specdata files
    
    nobs <- c() # empty vector to hold the complete cases for each file
    corvec <- c()
  
    specdata_files <- list.files(directory, full.names=TRUE) # make a list of files in "specdata"
    length <- length(specdata_files)
  
    for (i in 1:length) {
        spec <- read.csv(specdata_files[i]) # gather data from specdata
        specnobs <- sum(complete.cases(spec)) # check complete cases
    if (specnobs > threshold) {
        polucor <- cor(spec$nitrate, spec$sulfate, use="pairwise.complete.obs")
        corvec <- c(corvec, polucor)
    }
    nobs <- c(nobs, specnobs)
  }
  corvec
}