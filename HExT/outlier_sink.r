# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 16.07.2015

# function to store boxplot outlier information in a semicolon-separated text file
# and also in the form of additional boxplots
outlier_sink <- function (boot_mat, oriboots, bp, ingroup_strings, filename = NULL, directory, horizontal, w_range, xory_lab, tlabels = NULL, nodenumv = NULL) {
    whichtree <- c() 
    outlier_frame <- data.frame("node" = NA, "tree" = NA, "bootstrap" = NA, "bootstrap_fulltree" = NA, 
                                "difference" = NA, "jackknife_set" = NA)
    mem_frame <- data.frame("node" = NA, "value" = NA)
    for (j in 1:length(bp$group)) {
        wt_full <- which(boot_mat[, bp$group[j]] == bp$out[j])
		if (!isTRUE(any(apply(mem_frame, 1, function(x) identical(as.numeric(x), c(bp$group[j], bp$out[j])))))) {
			for (k in 1:length(wt_full)) {
				if (filename == "") {
					whichnode <- bp$group[j]
					orinode <- bp$group[j]
				} else {
					whichnode <- substr(as.character(tlabels[bp$group[j]]), 2, nchar(as.character(tlabels[bp$group[j]])))
					orinode <- nodenumv[bp$group[j]]
				}
				if ((j == 1) && (k == 1)) {
					outlier_frame[1, ] <- c(whichnode, wt_full[k], bp$out[j], oriboots[orinode], 
											bp$out[j] - oriboots[orinode], ingroup_strings[wt_full[k]])
				} else {
					outlier_frame[nrow(outlier_frame) + 1, ] <- c(whichnode, wt_full[k], bp$out[j], oriboots[orinode], 
																  bp$out[j] - oriboots[orinode], ingroup_strings[wt_full[k]])
				}
			}
		}	
		if (any(is.na(mem_frame))) {
			mem_frame[1, ] <- c(bp$group[j], bp$out[j])
		} else {
			mem_frame[nrow(mem_frame) + 1, ] <- c(bp$group[j], bp$out[j])
		}
    } 
    outlier_frame <- outlier_frame[with(outlier_frame, order(-as.numeric(difference))), ]
    pastefilename <- paste("outliers", filename, ".txt", sep = "", collapse = "")
	if (length(outlier_frame)) {
		write.table(outlier_frame, file = paste(directory, "/", pastefilename, sep = ""), quote = FALSE, sep = " ; ", row.names = FALSE)
	} else {
		file.create(paste(directory, "/", pastefilename, sep = ""))
	}
	if (filename == "") {
		diff_med <- apply(outlier_frame, 1, function(x) as.numeric(x[3]) - median(boot_mat[, as.numeric(x[1])], na.rm = TRUE))
		irow <- which(diff_med > 0)
		outnodes <- outlier_frame$node[irow]
		outnodes <- as.numeric(outnodes)
		outnodes <- unique(outnodes)
		if (length(outnodes)) {
		if (horizontal) {
			myxlab <- xory_lab
			myylab <- "nodes"
		} else {
			myxlab <- "nodes"
			myylab <- xory_lab
		}
		for (i in 1:2) { 
			if (i == 1) {
				pdf(file = paste(directory, "/upper_outlier_boxplots", filename, ".pdf", sep = ""))
			} else {
				setEPS()
				postscript(file = paste(directory, "/upper_outlier_boxplots", filename, ".eps", sep = ""))
			} 
			if (horizontal) {
				xaxt <- "s"
				yaxt <- "n"
				axside <- 2
			} else {
				xaxt <- "n"
				yaxt <- "s"
				axside <- 1
			}
			boxplot(x = boot_mat[, outnodes], use.cols = TRUE, xlab = myxlab, ylab = myylab, 
			        horizontal = horizontal, range = w_range, xaxt = xaxt, yaxt = yaxt)
			axis(side = axside, at = 1:length(outnodes), labels = outnodes, las = 2)
			dev.off()
		}
		} else {
			file.create(paste(directory, "/upper_outlier_boxplots", filename, ".pdf", sep = ""))
			file.create(paste(directory, "/upper_outlier_boxplots", filename, ".eps", sep = ""))
		}
	} 
}