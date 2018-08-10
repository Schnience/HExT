# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# comparing tree topologies and collecting bootstrap support data 
node_comp <- function (alltrees_list, allclade_list, descend_list, ingroup, length_taxnames, root, mid) {
    allboot_list <- rep(list(list()), length_taxnames + 1)
    for (u in 1:(length_taxnames + 1)) {
        allboot_list[[u]] <- rep(list(c(rep(0, alltrees_list[[u]]$Nnode))), length_taxnames + 1)
    }
    is_rooted <- FALSE
    for (v in 1:(length_taxnames + 1)) {
		if (root || (mid && (v == 1))) {
			  is_rooted <- TRUE
		}
        allboot_list[[v]][[v]] <- prop.clades(alltrees_list[[v]], part = allclade_list[[v]], rooted = is_rooted)
        is_rooted <- FALSE
    } 
    for (w in 1:(length_taxnames + 1)) {
		curr_tree <- alltrees_list[[w]]
		ntips <- length(curr_tree$tip.label)
        for (x in 1:(length_taxnames + 1)) {
			if (w != x) {
                for (y in 1:length(descend_list[[w]])) {
                    if (x != 1) {
                        curr_descend <- descend_list[[w]][[y]][!descend_list[[w]][[y]] %in% ingroup[[x - 1]]]
                    } else {
                        curr_descend <- descend_list[[w]][[y]]
                    }
                    matchnode <- sort(match(curr_descend, attr(allclade_list[[x]], "labels")))
					if (mid && (x != 1)) {
						curr_node <- y + ntips
						alltips <- curr_tree$tip.label[!curr_tree$tip.label %in% ingroup[[x - 1]]]
						alltips <- which(curr_tree$tip.label %in% alltips)
						curr_descend_excl <- curr_descend[!curr_descend %in% ingroup[[x - 1]]]
						posclade <- which(curr_tree$tip.label %in% curr_descend_excl)
						negclade <- curr_tree$tip.label[alltips[!alltips %in% posclade]]
						negclade <- sort(negclade)
						negclade_match <- sort(match(negclade, attr(allclade_list[[x]], "labels")))
					}
					found <- FALSE
                    if (!any(is.na(matchnode))) {
                        for (z in 1:length(allclade_list[[x]])) {
                            if (!any(is.na(matchnode)) && identical(matchnode, allclade_list[[x]][[z]])) { 
                                allboot_list[[w]][[x]][y] <- attr(allclade_list[[x]], "number")[z]
								found <- TRUE
                                break 
                            } 
						}
					}
					if (!root && mid && (x != 1)) {
						if (!any(is.na(negclade))) {
							for (z2 in 1:length(allclade_list[[x]])) {
								if (identical(negclade_match, allclade_list[[x]][[z2]])) {
									if (found) {
										supportsum <- allboot_list[[w]][[x]][y] + attr(allclade_list[[x]], "number")[z2]
										allboot_list[[w]][[x]][y] <- supportsum
									} else {
										allboot_list[[w]][[x]][y] <- attr(allclade_list[[x]], "number")[z2]
									}
									break 
								}
							}
						} 
					}
                }
            }
        }
    }  
    return(allboot_list)    
}