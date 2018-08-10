# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# determining the descending tips of a specified node
descendants <- function (tree, descendnodes, root, mid, descendtips = NULL) {
    if (root || mid) {
        tipnum <- tree$Nnode + 1
    } else {
		tipnum <- tree$Nnode + 2
    }
    descendnodes_new <- tree$edge[tree$edge[, 1] %in% descendnodes, 2]
    descendtips <- append(descendtips, descendnodes[descendnodes <= tipnum])
    descendtips <- append(descendtips, descendnodes_new[descendnodes_new <= tipnum])
    descendnodes_new <- descendnodes_new[descendnodes_new > tipnum]
    if (length(descendnodes_new) == 0) {
        return(descendtips) 
    } 
    do.call("descendants", list(tree, descendnodes_new, root, mid, descendtips)) 
}   