# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# writing parameter information of HExT runs to a txt file
write_par <- function (seqfile, ogfile = NULL, jackfile = NULL, addjacks = NULL, num_bootstraps, rooting, midpoint, outgroup = NULL, cons_char, taxdef_char, addex_char, missdat_char, binary_char, threads, directory,
					   w_range) {
	if (length(ogfile)) {
		ogpath <- file.path(ogfile)
	} else {
		ogpath <- "none"
	}
	if (length(jackfile)) {
		jackpath <- file.path(jackfile)
	} else {
		jackpath <- "none"
	}
	if (length(addjacks)) {
		addpath <- file.path(addjacks)
	} else {
		addpath <- "none"
	}
	if (!length(outgroup)) {
		outgroup <- "none"
	}
	par_list <- list(
		paste("data file path: ", file.path(seqfile), sep = ""), 
		paste("outgroup file path: ", ogpath, sep = ""),
		paste("jackknife specification file path: ", jackpath, sep = ""),
		paste("additional jackknife specification file path: ", addpath, sep = ""),
		paste("number of boostraps: ", num_bootstraps, sep = ""), 
		paste("outgroup rooting: ", rooting, sep = ""), 
		paste("midpoint rooting: ", midpoint, sep = ""), 
		paste("outgroup: ", paste(outgroup, collapse = " "), sep = ""), 
		paste("consistent outgroup monophyly: ", cons_char, sep = ""), 
		paste("data file jackknifing: ", taxdef_char, sep = ""), 
		paste("additional jackknife sets: ", addex_char, sep = ""), 
		paste("missing data: ", missdat_char, sep = ""), 
		paste("data type: ", binary_char, sep = ""), 
		paste("number of threads: ", threads, sep = ""),
		paste("whisker/outlier range: ", w_range, sep = "")
	)
	lapply(par_list, write, file = paste(directory, "/parameters.txt", sep = ""), append = TRUE, ncolumns = 900)
}