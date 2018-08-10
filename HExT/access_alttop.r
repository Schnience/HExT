# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 06.07.2015

# enabling the search for nodes in alternative topologies (i.e. topologies not found in the full tree); 
# for each tree, the bootstrap support as well as the cumulative bootstrap support (sum of bootstrap support 
# of all trees the node was found in) of the specified node and the number of trees the node was found in 
# are then displayed 
access_alttop <- function (taxlist, alttop_frame, fulltree_desc, num_bootstraps) { 
    taxlist <- sort(taxlist)
    taxlist <- paste(taxlist, collapse = " ") 
    frame_mat <- matrix()
    cumulboots <- 0
    nodecount <- 0
    for (i in 1:nrow(alttop_frame)) {
        if (identical(alttop_frame[i, 3], taxlist)) {
            if (any(is.na(frame_mat))) {
                frame_mat[1, 1] <- alttop_frame[i, 2]
                tick_vect <- alttop_frame[i, 1]
            } else {
                frame_mat <- rbind(frame_mat, alttop_frame[i, 2])
                tick_vect <- append(tick_vect, alttop_frame[i, 1])
            } 
            cumulboots <- cumulboots + as.numeric(alttop_frame[n, 2])
            nodecount <- nodecount + 1
        }    
    }   
    if (all(is.na(frame_mat))) {
        for (j in 1:length(fulltree_desc)) {
            if (identical(paste(fulltree_desc[[j]], collapse = " "), taxlist)) {
                cat("    The specified node was found in the full tree.\n")
                break
            }
        }
    } else { 
        if (treecount > 1) {
            string1 <- "trees and"
        } else {
            string1 <- "tree and"
        }
        if (cumulboots > 1) {
            string2 <- "bootstrap trees.\n"
        } else {
            string2 <- "bootstrap tree.\n"
        }
        cat("    The specified node was found in", treecount, string1, cumulboots, string2)
        frame_mat <- as.numeric(frame_mat) * (100 / num_bootstraps)
    }
    return(frame_mat)
}