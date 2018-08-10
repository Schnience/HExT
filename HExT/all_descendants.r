# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# a function returning sample names of all clades for each input tree
all_descendants <- function (alltrees_list, length_taxnames, root, mid) {
    descend_list <- rep(list(list()), length_taxnames + 1)
    for (t in 1:(length_taxnames + 1)) {
        descend_list[[t]] <- rep(list(c()), alltrees_list[[t]]$Nnode)    
    }
    if (root || mid) { 
        for (u in 1:(length_taxnames + 1)) {
			if ((u == 1) || (!mid)) {
				for (v in (alltrees_list[[u]]$Nnode + 2):((2 * alltrees_list[[u]]$Nnode) + 1)) {
					tip_ind <- descendants(alltrees_list[[u]], v, root, mid)
					descend_list[[u]][[v - alltrees_list[[u]]$Nnode - 1]] <- sort(alltrees_list[[u]]$tip.label[tip_ind])
				}
			} else {
				for (v in (alltrees_list[[u]]$Nnode + 3):((2 * alltrees_list[[u]]$Nnode) + 2)) {
					tip_ind <- descendants(alltrees_list[[u]], v, root, mid = FALSE)
					descend_list[[u]][[v - alltrees_list[[u]]$Nnode - 2]] <- sort(alltrees_list[[u]]$tip.label[tip_ind])
				}
			}
        }
    } else {
        for (u in 1:(length_taxnames + 1)) {
            for (v in (alltrees_list[[u]]$Nnode + 3):((2 * alltrees_list[[u]]$Nnode) + 2)) {
                tip_ind <- descendants(alltrees_list[[u]], v, root, mid)
                descend_list[[u]][[v - alltrees_list[[u]]$Nnode - 2]] <- sort(alltrees_list[[u]]$tip.label[tip_ind])
            }
        }
    }
    return(descend_list) 
}