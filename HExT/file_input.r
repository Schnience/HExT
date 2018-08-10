# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# calling input parsing functions on the basis of input file extensions
file_input <- function (seqfile, taxdef, missdat, binary) {
    extension <- file_ext(seqfile)
    nex_ext <- c("nexus", "nex", "nxs")
    fas_ext <- c("fasta", "fas", "fna", "ffn", "frn")
    txt_ext <- c("txt")
    if (extension %in% nex_ext) {
        seqlist <- nex_parse(seqfile, taxdef, missdat, binary)        
    } else if (extension %in% fas_ext) {
        seqlist <- fas_parse(seqfile, taxdef, missdat, binary)
    } else if (extension %in% txt_ext) {
        seqlist <- geno_parse(seqfile, taxdef, missdat)
    } else {
        string1 <- "File format not supported! Please use either NEXUS, FASTA or (genotype) TXT file formats!\n"
        string2 <- "(supported file extensions: .nexus, .nex, .nxs, .fasta, .fas, .fna, .ffn, .frn, .txt)"
        stop(paste(string1, string2))
    }
    return(seqlist)    
}