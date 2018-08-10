# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# function determining sister nodes of excluded (i.e. jackknifed) clades 
sisternodes <- function (fulltree, prevnode, ancestnodes, excl_group, tip_ind_vect, root, mid) {   
    ancestnode <- fulltree$edge[fulltree$edge[, 2] == prevnode, 1]
	ancestnodes <- append(ancestnodes, ancestnode)
    descendnode <- fulltree$edge[fulltree$edge[, 1] == ancestnode, 2]
    descendnode <- descendnode[descendnode != prevnode]
    tip_ind_vect <- descendants(fulltree, descendnode, root, mid)
    tip_vect <- fulltree$tip.label[tip_ind_vect]            
    if (length(tip_vect[!tip_vect %in% excl_group]) == 1) {
        return(ancestnodes)
    } else if (length(tip_vect[!tip_vect %in% excl_group]) >= 2) {
        if (length(fulltree$tip.label[!fulltree$tip.label[!fulltree$tip.label %in% excl_group] %in% tip_vect]) == 0) {
            descendnodes2 <- fulltree$edge[fulltree$edge[, 1] %in% descendnode, 2]
            if (root || mid) {
                tipnum <- fulltree$Nnode + 1
            } else {
		        tipnum <- fulltree$Nnode + 2
            }
            descendnodes2 <- descendnodes2[descendnodes2 > tipnum]
            return(c(descendnode, ancestnodes, descendnodes2))
        } else {
            return(c(descendnode, ancestnodes))
        } 
    } else if (length(ancestnode) == 0 || length(descendnode) == 0) {
        return(NULL) 
    } else {  
        do.call("sisternodes", list(fulltree, ancestnode, ancestnodes, excl_group, tip_ind_vect, root, mid))
    }  
}   