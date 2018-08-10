# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# extracting the binary/SNP data and sample names from the input fasta file
fas_parse <- function (seqfile, taxdef, missdat, binary) {
    if (binary) {
        prefas <- readLines(seqfile)
        fas <- prefas[seq(2, length(prefas), by = 2)] 
        fas <- matrix(unlist(strsplit(fas, "")), nrow = length(fas), ncol = nchar(fas[1]), byrow = TRUE)  
        rownames(fas) <- substring(prefas[seq(1, length(prefas) - 1, by = 2)], 2) 
    } else {
        fas <- read.dna(seqfile, format = "fasta", as.character = TRUE, as.matrix = TRUE)
    } 
    fas <- t(fas)
    if (taxdef) {
        fasnames <- colnames(fas)                                        
        taxa <- sub(".*?_", "", fasnames)                              
        taxvect <- taxa
        taxnames <- unique(taxa)                                      
    }
    if (missdat) {
        fas[fas == "?"] <- NA
    } else {
        fas <- fas[rowSums(fas == "?") == 0,]
    }
    if (binary) {
        if (is.integer(fas) == FALSE) {                              
            fas <- apply(fas, 2, as.integer) 
        }
    } else {
        fas[fas == "A" | fas == "a"] <- 0
        fas[fas == "C" | fas == "c"] <- 1
        fas[fas == "G" | fas == "g"] <- 2
        fas[fas == "T" | fas == "t"] <- 3
        fas <- apply(fas, 2, as.integer) 
    }
    if (taxdef) {
        faslist <- list(fas, taxvect, taxnames)
    } else {
        faslist <- list(fas)
    }
    return(faslist)   
}