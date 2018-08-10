# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# writing the descending taxa and bootstrap support values of all nodes of all trees to a txt file
write_nodes <- function (alltrees_list, allboot_list, directory, root, mid) {
    length_alltrees_list <- length(alltrees_list)
    nodetaxa_list <- rep(list(list()), length_alltrees_list)
    for (k in 1:length_alltrees_list) {
        for (l in 1:alltrees_list[[k]]$Nnode) {
            if (root || (mid && (k == 1))) {
                nodetaxa_list[[k]][[l]] <- descendants(alltrees_list[[k]], l + alltrees_list[[k]]$Nnode + 1, root, mid)
            } else {
                nodetaxa_list[[k]][[l]] <- descendants(alltrees_list[[k]], l + alltrees_list[[k]]$Nnode + 2, root, mid = FALSE)
            }
            nodetaxa_list[[k]][[l]] <- alltrees_list[[k]]$tip.label[nodetaxa_list[[k]][[l]]]
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], "tree #", 0)
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], k - 1, 1)
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], "node #", 2)
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], l, 3)
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], "bootstrap: ", 4)
            nodetaxa_list[[k]][[l]] <- append(nodetaxa_list[[k]][[l]], paste(allboot_list[[k]][[k]][l], ";"), 5)
        }
    } 
    if (file.exists(paste(directory, "/nodes.txt", sep = ""))) {
        unlink(paste(directory, "/nodes.txt", sep = ""))
    }
    for (m in 1:length_alltrees_list) {
        lapply(nodetaxa_list[[m]], write, file = paste(directory, "/nodes.txt", sep = ""), 
			   append = TRUE, ncol = (alltrees_list[[1]]$Nnode + 7), sep = " ") 
    }
}