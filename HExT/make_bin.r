# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# this function converts an integer genotype matrix to four binary matrices
make_bin <- function (charmat) {
    binmat1 <- matrix(as.integer(rep(0, nrow(charmat) * ncol(charmat))), nrow = nrow(charmat), ncol = ncol(charmat))
    rownames(binmat1) <- rownames(charmat)
    colnames(binmat1) <- colnames(charmat)
    binmat2 <- binmat1
    binmat3 <- binmat1
    binmat4 <- binmat1
    for (i in 1:nrow(charmat)) {
        for (j in 1:ncol(charmat)) {
            if (!is.na(charmat[i, j])) {
               if (charmat[i, j] == 2) {
                  binmat1[i, j] <- 1
               } else if (charmat[i, j] == 1) {
                  binmat2[i, j] <- 1
               } else if (charmat[i, j] == 0) {
                  binmat3[i, j] <- 1
               }
            } else {
               binmat4[i, j] <- 1
            } 
        }
    }
    binmats <- list(binmat1, binmat2, binmat3, binmat4)
    return(binmats)
}