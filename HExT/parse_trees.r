# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# Here, nodes of jackknifed trees and the original tree are compared,
# and BS support information for full and jackknife trees is obtained.
# Subsequently, topologies and analysis/comparison results are stored
# in several ways.
parse_trees <- function (alltrees_list, length_taxnames, root, mid, allclade_list, ingroup, num_bootstraps, directory, horizontal, w_range) {
	descend_list <- all_descendants(alltrees_list, length_taxnames, root, mid)
	allboot_list <- node_comp(alltrees_list, allclade_list, descend_list, ingroup, length_taxnames, root, mid)
	allboot_list <- lapply(allboot_list, function(x) lapply(x, function(x) (x * (100 / num_bootstraps))))
	if (file.exists(paste(directory, "/trees_bootlabels.nwk", sep = ""))) {
		unlink(paste(directory, "/trees_bootlabels.nwk", sep = ""))
	}   
	for (c in 1:(length_taxnames + 1)) {  
		alltrees_list[[c]]$node.label <- allboot_list[[c]][[c]]
		write.tree(alltrees_list[[c]], file = paste(directory, "/trees_bootlabels.nwk", sep = ""), 
				   append = TRUE) 
	}  
	if (file.exists(paste(directory, "/trees_numlabels.nwk", sep = ""))) {
		unlink(paste(directory, "/trees_numlabels.nwk", sep = ""))
	}    
	for (d in 1:(length_taxnames + 1)) {  
		alltrees_list[[d]]$node.label <- seq(1:alltrees_list[[d]]$Nnode)
		write.tree(alltrees_list[[d]], file = paste(directory, "/trees_numlabels.nwk", sep = ""), 
				   append = TRUE) 
	}    
	do.call("write_nodes", list(alltrees_list, allboot_list, directory, root, mid))
	do.call("store_plot", list(allboot_list, num_bootstraps, length_taxnames, horizontal, w_range,
							   ingroup_strings = NULL, directory, boxplots = FALSE))
	excl_boots <- boot_excl(alltrees_list[[1]], ingroup, length_taxnames, root, mid)
	for (e in 1:length_taxnames) {
		allboot_list[[1]][[e + 1]][excl_boots[[e]]] <- NA
	} 
	ingroup_strings <- unlist(lapply(ingroup, function(x) paste(x, sep = " ", collapse = " ")))
	do.call("excl_spec", list(ingroup_strings, directory)) 
	allboot_list <- store_plot(allboot_list, num_bootstraps, length_taxnames, horizontal, w_range,
							   ingroup_strings, directory)
	alttop_frame <- alttop_search(allclade_list, descend_list[[1]], length_taxnames, directory)
	return(list(allboot_list = allboot_list, descend_list = descend_list, ingroup_strings = ingroup_strings, alttop_frame = alttop_frame))
}