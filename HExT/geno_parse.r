# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# extracting the SNP genotype data and sample names from an input text file
geno_parse <- function (seqfile, taxdef, missdat) {
    geno <- read.table(seqfile, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)
    if (taxdef) {
        genonames <- colnames(geno)                                        
        taxa <- sub(".*?_", "", genonames)                              
        taxvect <- taxa
        taxnames <- unique(taxa)                                      
    }
    if (missdat) {
        geno <- apply(geno, 2, function(x) gsub(-1, NA, x))
        geno <- apply(geno, 2, as.integer)
    } else {
        geno <- geno[rowSums(geno == -1) == 0,]
    }
    if (taxdef) {
        genolist <- list(geno, taxvect, taxnames)
    } else {
        genolist <- list(geno)
    }
    return(genolist)   
}