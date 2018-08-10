# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# function to determine nodes that automatically exhibit increased bootstrap support
# due to exclusion of a sister clade (of comparisons of fulltree to jackknife bootstrap trees)
# and those that are lost in the course of jackknifing
# (these nodes are excluded later on)
boot_excl <- function (fulltree, ingroup, length_taxnames, root, mid) { 
    excl_boots <- rep(list(c()), length_taxnames) 
    for (e in 1:length_taxnames) {
        full_tipnum <- length(fulltree$tip.label)
        for (f in 1:length(ingroup[[e]])) {
            tipnum <- which(fulltree$tip.label == ingroup[[e]][f]) 
            tip_ind_vect <- c()
			ancestnodes <- c()
            sisternode <- sisternodes(fulltree, tipnum, ancestnodes, ingroup[[e]], tip_ind_vect, root, mid)
            sisternode <- sisternode - full_tipnum
            sisternode <- sisternode[sisternode > 0] 
            if (length(sisternode) > 0) {            
                excl_boots[[e]] <- append(excl_boots[[e]], as.integer(sisternode))   
            }
        }
    }
    return(excl_boots)
}