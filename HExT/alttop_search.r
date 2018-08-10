# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 07.07.2015

# searching for topologies of bootstrap trees not present in the full tree
# and sorting the thus obtained nodes according to bootstrap support values
alttop_search <- function (allclade_list, fulltree_desc, length_taxnames, directory) {
    alttop_frame <- data.frame(matrix(ncol = 3))
    colnames(alttop_frame) <- c("tree", "bootstrap", "node")
    for (i in 1:(length_taxnames + 1)) {
        for (j in 1:length(allclade_list[[i]])) {
            altnode <- attr(allclade_list[[i]], "labels")[allclade_list[[i]][[j]]]
            altnode <- sort(altnode) 
            l <- 0
            for (k in 1:length(fulltree_desc)) {
                orinode <- sort(fulltree_desc[[k]])
                if (identical(altnode, orinode)) {
                    break
                } else {
                    l <- l + 1
                }     
            }
            if (l == length(fulltree_desc)) {
                if (any(is.na(alttop_frame))) {
                    alttop_frame[1, ] <- c(i, attr(allclade_list[[i]], "number")[j], paste(altnode, collapse = " "))
                } else {      
                    alttop_frame <- rbind(alttop_frame, c(i - 1, attr(allclade_list[[i]], "number")[j], 
										  paste(altnode, collapse = " ")))
                } 
            }
        }
    }
    alttop_frame[, 2] <- as.numeric(alttop_frame[, 2])
    alttop_frame <- alttop_frame[order(alttop_frame[, 2], decreasing = TRUE), ]
    write.table(alttop_frame, file = paste(directory, "/alt_topologies.txt", sep = ""), row.names = FALSE, quote = FALSE,
				sep = ";")
    return(alttop_frame)    
}