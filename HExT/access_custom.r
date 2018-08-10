# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 16.07.2015

# enabling the specification of custom sets of nodes the bootstrap boxplots of which should 
# be displayed and, if desired, stored along with the underlying bootstrap support data
access_custom <- function (allboot_list, alttop_frame, descend_list, num_bootstraps, length_taxnames, horizontal, w_range, ingroup_strings, directory, to_access, index = 1) {
    if (!to_access) {
        while (1) {
            if (index == 1) {
                cat("CUSTOM -\nDo you want to display bootstrap support of custom sets of nodes?\n")
            } else {
                cat("PROCEED (CUSTOM) -\nDo you want to proceed accessing bootstrap support values\nof custom sets of nodes?\n")
            }
            proceed_char <- readline("(y/n): ")
            if (proceed_char == "y") {
                proceed <- TRUE
                break
            } else if (proceed_char == "n") {
                proceed <- FALSE
                break
            } else {
                cat("Invalid input. Please try again!\n") 
            }
        }
    } else {
        proceed <- TRUE
    }
    if (proceed) {        
        while (1) {
            cat("NODE SPECIFICATION (CUSTOM) -\n")
			#cat("NODE SPECIFICATION (CUSTOM) -\nPlease specify the tips of the node to search for (separate each taxon name\nby an empty space, each taxon group by a comma, or, alternatively, specify the node\nnumber in the full tree after a '#' (e.g. #25); add the number of the tree in\nwhich to search for at the end of the respective taxon group separated by an empty\nspace, or, alternatively, type 'all' for the bootstraps of all trees (i.e. a boxplot\nis shown) (e.g. taxon1 taxon2 taxon3 2,taxon7 taxon8 taxon9 all,#25 all,#29 all)\n(with full tree = 0, jackknife tree 1 = 1, jackknife tree 2 = 2, etc.)\n")
            node_taxa2 <- readline(": ")                                
            if (!is.na(node_taxa2)) {
                break
            } else {
                cat("Missing input. Please try again!\n") 
            }
        } 
        taxlist <- inp_manip(node_taxa2, specify = TRUE)
        length_taxlist <- length(taxlist) 
        treedef <- c()
        nodenumv <- c()
        for (k in 1:length(taxlist)) {
            length_taxlistk <- length(taxlist[[k]])
            treedef[k] <- taxlist[[k]][length_taxlistk]
            taxlist[[k]] <- taxlist[[k]][-length_taxlistk]    
        } 
        node_boots2 <- matrix(rep(NA, ((length_taxnames + 1) * length_taxlist)), nrow = (length_taxnames + 1), ncol = length_taxlist)
        for (l in 1:length_taxlist) {
            node_num <- NA 
            if (substring(taxlist[[l]][1], 1, 1) == "#") {
                node_num <- as.numeric(substring(taxlist[[l]], 2, nchar(taxlist[[l]]))) 
                nodenumv[l] <- node_num
            } else {
                for (lb in 1:(length_taxnames + 1)) {
                    for (lc in 1:length(descend_list[[lb]])) {
                        if (identical(sort(taxlist[[l]]), descend_list[[lb]][[lc]])) {
                            node_num <- lc
                            nodenumv[l] <- node_num
                            break
                        } 
                    }
                    if (!is.na(node_num)) {
                        break
                    }
                }
            } 
            if (treedef[l] == "all") {
                currdef <- 2:(length_taxnames + 1)
            } else { 
                currdef <- as.numeric(treedef[l]) + 1
            } 
            if (!is.na(node_num)) {
                if (substring(taxlist[[l]][1], 1, 1) == "#") {
                   node_boots2[currdef, l] <- allboot_list[[1]][currdef, node_num]
                } else {
                   node_boots2[currdef, l] <- allboot_list[[lb]][currdef, node_num]
                }
            } else { 
                node_boots2[currdef, l] <- access_alttop(taxlist[[l]], alttop_frame, descend_list[[1]], num_bootstraps)    
            }
        }
        if (all(is.na(node_boots2))) {
            cat("The specified node has not been found in any of the trees or bootstrap trees.\n") 
        } else {
            node_boots2 <- rowcol_names(node_boots2)
            tick_vect <- c()
            for (m in 1:length_taxlist) {
                if (treedef[m] == "all") {
                   tick_vect[m] <- paste("#", nodenumv[m], "_all", sep = "")
                } else {
                   tick_vect[m] <- paste("#", nodenumv[m], "_", treedef[m], sep = "")
                }     
            }
            xory_lab <- paste("bootstrap support [% of ", num_bootstraps, " bootstraps]", sep = "")
			if (horizontal) {
				myxlab <- xory_lab
				myylab <- "nodes"
				myaxis <- 2
				xaxt <- NULL
				yaxt <- "n"
			} else {
				myxlab <- "nodes"
				myylab <- xory_lab
				myaxis <- 1
				xaxt <- "n"
				yaxt <- NULL
			}
            bp <- boxplot(x = node_boots2, use.cols = TRUE, main = paste("Custom", index), xlab = myxlab, 
						  ylab = myylab, horizontal = horizontal, range = w_range, xaxt = xaxt, yaxt = yaxt)
            axis(myaxis, at = (1:length(tick_vect)), labels = tick_vect)
            cat("Tick labels: #Node_Tree\n")
            sctl_list <- plot_manip()
            store <- sctl_list[[1]]
            if (store) {
                change <- sctl_list[[2]]
                if (change) {
                    mtitle <- paste(sctl_list[[3]][[1]], collapse = " ") 
                    tlabels <- sctl_list[[3]][[2]]
                } else {
                    mtitle <- paste("Custom", index)
                    tlabels <- tick_vect 
                } 
                for (i in 1:2) {
                    if (i == 1) {
                        pdf(file = paste(directory, "/custom_", index, ".pdf", sep = ""))
                    } else {
                        setEPS()
                        postscript(file = paste(directory, "/custom_", index, ".eps", sep = ""))
                    }
                    bpf <- boxplot(x = node_boots2, use.cols = TRUE, main = mtitle, xlab = myxlab, ylab = myylab, 
								   horizontal = horizontal, range = w_range, xaxt = xaxt, yaxt = yaxt)
                    axis(myaxis, at = (1:length(tlabels)), labels = tlabels)
                    if (length(bpf$group)) {
                        do.call("outlier_sink", list(node_boots2, c(allboot_list[[1]][1, ]), bpf, ingroup_strings, 
													 filename = paste("_custom_", index, sep = "", collapse = ""), 
													 directory, horizontal, w_range, xory_lab, tlabels, nodenumv))
                    }                                                                                                       
                    dev.off()
                }
                write.table(node_boots2, file = paste(directory, "/custom_", index, ".txt", sep = ""), 
							row.names = TRUE, quote = FALSE)    
            }
        }
        do.call("access_custom", list(allboot_list, alttop_frame, descend_list, num_bootstraps, length_taxnames, horizontal, 
									  w_range, ingroup_strings, directory, to_access = FALSE, index + 1))
    }   
}