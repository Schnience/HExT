# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# binary: the first part of the function calculates the Nei and Li (1979) distance matrix from the input binary character matrix
# after omitting all rows with missing data, whereas the second part calculates the same distance matrix
# after omitting missing data in a pairwise manner to retain the largest possible number of overlapping positions
# genotype: an allele sharing distance matrix is calculated from SNP genotype input data
# SNP: for SNP nucleotide input data, the function also calculates an allele sharing distance matrix
seq_dist <- function (charmat, col_names, resample, binary, gtype, numrow_charmat = NULL, nloci = NULL) {
    if (!gtype) {
       missval <- NULL                                               
       if (any(is.na(charmat)) || any(charmat == "?")) {                                     
          missval <- TRUE                                       
       } else {
          missval <- FALSE
       }
    }
    if (binary) {                                                           
        if (!missval) {         
              if (resample == TRUE) {
                  sample_ind <- sample(ncol(charmat), replace = TRUE)
                  charmat <- charmat[, sample_ind] 
              }
              # this part of the function (next 4 lines) is a modified version of a function by Fiorello Toneatto ([R-sig-genetics] mailing list)
              # (toneattof(at)gmail.com, Tue Sep 22 16:51:11 CEST 2009) and calculates the distance matrix using matrix multiplication, 
              # equivalent to the function part below using pairwise comparison              
              rowsum <- apply(charmat, 1, sum)  
              dimatch <- tcrossprod(charmat)  
              distmat <- 1 - (dimatch + dimatch)/((rowsum - t(t(dimatch) - rowsum)) + dimatch) 
              return(distmat) 
        } else {          
            if (resample == TRUE) {
                sample_ind <- sample(ncol(charmat), replace = TRUE)
                charmat <- charmat[, sample_ind]
            }
            numrow_charmat <- nrow(charmat)
            distmat <- matrix(rep(0,numrow_charmat), numrow_charmat, numrow_charmat)
            for (i in 1:(numrow_charmat - 1)) {  
                for (j in (i + 1):numrow_charmat) {
                    curr_charmat <- charmat[c(i, j),]
                    dimatch <- sum(curr_charmat[1,] * curr_charmat[2,], na.rm = TRUE)
                    dist_curr <- 1 - (dimatch + dimatch) / sum(colSums(curr_charmat), na.rm = TRUE)
                    distmat[j, i] <- dist_curr
                } 
            }
        }
    } else if (gtype) {
        if (resample) {
            sample_ind <- sample(nloci, replace = TRUE)
            charmat <- lapply(charmat, "[", , sample_ind)
        }
        redlist <- list(charmat[[1]], charmat[[2]], charmat[[3]])
        ploci <- tcrossprod(abs(charmat[[4]] - 1))
        IBS2 <- Reduce("+", lapply(redlist, tcrossprod))
        IBS1subtr <- tcrossprod(charmat[[2]], charmat[[4]]) + tcrossprod(charmat[[4]], charmat[[2]])
        IBS1 <- nloci - (tcrossprod(charmat[[2]]) + tcrossprod(abs(charmat[[2]] - 1))) - IBS1subtr
        distmat <- 1 - ((IBS2 + 0.5 * IBS1) / ploci)
    } else {
        if (resample) {
            sample_ind <- sample(ncol(charmat) / 2, replace = TRUE)
            sample_ind_1 <- sample_ind * 2
            sample_ind_2 <- sample_ind_1 - 1
            sample_ind <- c(rbind(sample_ind_2, sample_ind_1)) 
            charmat <- charmat[, sample_ind]
        }
        nalleles <- nloci
        nloci <- nalleles / 2
        distmat <- matrix(rep(0,numrow_charmat), numrow_charmat, numrow_charmat)
        for (i in 1:(numrow_charmat - 1)) {  
            for (j in (i + 1):numrow_charmat) {
                curr_charmat <- charmat[c(i, j),]
                indseq1 <- seq(1, nalleles - 1, by = 2)
                indseq2 <- seq(2, nalleles, by = 2)
                ident1 <- curr_charmat[1, indseq1] == curr_charmat[2, indseq1] 
                ident2 <- curr_charmat[1, indseq2] == curr_charmat[2, indseq2]
                counts1 <- sum(!is.na(ident1[ident1 == "TRUE"]))
                counts2 <- sum(!is.na(ident2[ident2 == "TRUE"]))
                truematch <- ident2[which(ident1 == "TRUE")]
                IBS2 <- sum(!is.na(truematch[truematch == "TRUE"]))
                IBS1 <- (counts1 + counts2) - (IBS2 * 2) 
                dist_curr <- 1 - ((IBS2 + 0.5 * IBS1) / nloci)
                distmat[j, i] <- dist_curr 
            } 
        }
    }    
    rownames(distmat) <- col_names
    colnames(distmat) <- col_names
    return(distmat)
}