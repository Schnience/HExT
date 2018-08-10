# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# extracting the binary/SNP data and sample names from an input nexus file
nex_parse <- function (seqfile, taxdef, missdat, binary) {
    nex <- read.nexus.data(seqfile)                
    if (taxdef) {
        nexnames <- names(nex)                                        
        taxa <- sub(".*?_", "", nexnames)                              
        taxvect <- taxa
        taxnames <- unique(taxa)                                      
    } 
    nextrans <- matrix(unlist(nex), ncol = length(nex), byrow = FALSE)
    if (missdat) {
        nextrans[nextrans == "?"] <- NA
    } else {
        nextrans <- nextrans[rowSums(nextrans == "?") == 0,]
    }            
    colnames(nextrans) <- names(nex)
    if (binary) {      
        if (is.integer(nextrans) == FALSE) {                              
            nextrans <- apply(nextrans, 2, as.integer) 
        }
    } else {
        nextrans[nextrans == "A" | nextrans == "a"] <- 0
        nextrans[nextrans == "C" | nextrans == "c"] <- 1
        nextrans[nextrans == "G" | nextrans == "g"] <- 2
        nextrans[nextrans == "T" | nextrans == "t"] <- 3
        nextrans <- apply(nextrans, 2, as.integer) 
    }
    if (taxdef) {
        nexlist <- list(nextrans, taxvect, taxnames)
    } else {
        nexlist <- list(nextrans)
    }
    return(nexlist)
}