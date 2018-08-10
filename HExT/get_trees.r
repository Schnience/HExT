# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# This function performs jackknifing of taxon sets and subsequently calls functions for
# phylogenetic tree construction and functions to obtain tree bipartition information
get_trees <- function (length_taxnames, root, mid, iseq, taxdef, addex, length_groupdiff, addgroups, taxnames, taxvect, gtype, outgroup, cons, binary, directory, threads, num_bootstraps) {
	for (i in 1:(length_taxnames + 1)) { 
		if (root || (mid && (i == 1))) {
			root_spec <- TRUE
		} else {
			root_spec <- FALSE
		}
		iseq2 <- iseq
		if (i != 1) {                                             
			if (taxdef) {
				if ((addex) && (i > (length_groupdiff + 1))) {
					extax <- addgroups[[i - length_groupdiff - 1]]
					if (gtype) {
						extax <- which(rownames(iseq2[[1]]) %in% extax)
					} else {
						extax <- which(rownames(iseq2) %in% extax)
					}
					ingroup[[i - 1]] <- addgroups[[i - length_groupdiff - 1]]
				} else {
					currtax <- taxnames[i - 1]
					extax <- tax_comp(currtax, taxvect)  
					if (gtype) {
						ingroup[[i - 1]] <- rownames(iseq2[[1]])[extax]
					} else {
						ingroup[[i - 1]] <- rownames(iseq2)[extax]
					}
				}
			} else {
				extax <- taxnames[[i - 1]]
				if (gtype) {
					extax <- which(rownames(iseq2[[1]]) %in% extax)
				} else {
					extax <- which(rownames(iseq2) %in% extax)
				}
				ingroup[[i - 1]] <- taxnames[[i - 1]]
			}  
			if (gtype) {
				iseq2 <- lapply(iseq2, "[", -extax, )
			} else {      
				iseq2 <- iseq2[-extax, ]
			}                   
		}
		if (gtype) {
			row_names <- rownames(iseq2[[1]])
			nloci <- ncol(iseq2[[1]]) 
		} else if (!binary) {
			row_names <- rownames(iseq2)
			numrow_charmat <- nrow(iseq2)
			nloci <- ncol(iseq2)
		} else {
			row_names <- rownames(iseq2)
			nloci <- NULL
		}
		tree <- njf(iseq2, row_names, resample = FALSE, outgroup, cons, binary, gtype, 
					root = root_spec, numrow_charmat, nloci, mid)     
		alltrees_list[[i]] <- tree 
		if (i == 1) { 
			if (file.exists(paste(directory, "/", "full_tree.nwk", sep = ""))) {
				unlink(paste(directory, "/", "full_tree.nwk", sep = ""))
			}                                            
			fulltree <- tree   
			tree$node.label <- seq(1:tree$Nnode)
			write.tree(tree, file = paste(directory, "/", "full_tree.nwk", sep = ""), append = TRUE)                                    
		}                                                                    
		boot_trees <- list()
		if (threads == 1) {
			cat("\r                                                                               ")
			for (b in 1:num_bootstraps) {
				cat("\rJackknife set ", i - 1, " of ", length_taxnames, "; bootstrap ", b, " of ", num_bootstraps, sep = "") 
				flush.console()
				boot_trees[[b]] <- njf(iseq2, row_names, resample = TRUE, outgroup, cons, binary, 
				                       gtype, root = root_spec, numrow_charmat, nloci, mid)
			}
		} else {
			if (.Platform$OS.type == "unix") {
				registerDoMC(threads)
			} else {
				nclust <- makeCluster(threads)
				registerDoSNOW(nclust)
			} 
			cat("\rJackknife set ", i - 1, " of ", length_taxnames, sep = "")
			flush.console()               
			boot_trees <- foreach (b = 1:num_bootstraps, .packages = c("ape", "phangorn"), .export = c("njf", "seq_dist")) %dopar% {
				njf(iseq2, row_names, resample = TRUE, outgroup, cons, binary, gtype, root = root_spec, 
				    numrow_charmat, nloci, mid)
			}
			if (.Platform$OS.type != "unix") {
				stopCluster(nclust)
			} 
		}  
		allclade_list[[i]] <- prop.part(boot_trees, check.labels = TRUE)
	}
	return(list(ingroup = ingroup, nloci = nloci, alltrees_list = alltrees_list, fulltree = fulltree, allclade_list = allclade_list))
}