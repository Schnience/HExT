# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 16.07.2015

# function to create and plot bootstrap boxplots and to store these and the underlying bootstrap
# support data in pdf and txt files, respectively
store_plot <- function (allboot_list, num_bootstraps, length_taxnames, horizontal, w_range, ingroup_strings, directory, boxplots = TRUE) {
    allboot_list <- lapply(allboot_list, function(x) unlist(x))
    allboot_list <- lapply(allboot_list, function(x) matrix(x, nrow = (length(x) / (length_taxnames + 1))))
    allboot_list <- lapply(allboot_list, function(x) t(x))
    allboot_list <- lapply(allboot_list, function(x) rowcol_names(x, rows_only = TRUE))
	if (boxplots) {
		xory_lab <- paste("bootstrap support [% of ", num_bootstraps, " bootstraps]", sep = "", collapse = "")
		if (horizontal) {
			myxlab <- xory_lab
			myylab <- "nodes"
		} else {
			myxlab <- "nodes"
			myylab <- xory_lab
		}	
		for (i in 1:2) { 
			if (i == 1) {
				pdf(file = paste(directory, "/boxplots.pdf", sep = ""))
			} else {
				setEPS()
				postscript(file = paste(directory, "/boxplots.eps", sep = ""))
			} 
			bpf <- boxplot(x = allboot_list[[1]][-1, ], use.cols = TRUE, xlab = myxlab, ylab = myylab, 
						   horizontal = horizontal, range = w_range)
			if (length(bpf$group)) {
				dev.off()
				do.call("outlier_sink", list(allboot_list[[1]][-1, ], allboot_list[[1]][1, ], bpf, ingroup_strings, 
											 filename = "", directory, horizontal, w_range, xory_lab))
			}
			if (!length(bpf$group)) {
				dev.off()
			}
		}
	}
    colnames(allboot_list[[1]]) <- paste("node", 1:ncol(allboot_list[[1]]), sep = "") 
    allboot_list_frame <- data.frame(tree = row.names(allboot_list[[1]]))
    allboot_list_frame <- cbind(allboot_list_frame, allboot_list[[1]])
	if (boxplots) {
		write.table(allboot_list_frame, file = paste(directory, "/bootstrap_table.txt", sep = ""), 
					row.names = FALSE, quote = FALSE)
		return(allboot_list)
	} else {
		write.table(allboot_list_frame, file = paste(directory, "/raw_bootstrap_table.txt", sep = ""), 
					row.names = FALSE, quote = FALSE)		
	}                                                          
}