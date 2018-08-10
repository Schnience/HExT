# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# This function accepts input and parameter values from the user in a question/answer interactive way.
interact <- function () {
	#--------------------------------------------------------- INPUT ---------------------------------------------------------------------------------#
	if (file.exists("HExT.RData")) {
		while (1) {
			cat("ANALYSING PREVIOUS RESULTS -\nAnalysis results of a previous session were found in your current\nR working directory. Do you want to search for specific nodes in\nthese results?\n")                                          
			to_access_char <- readline("(y/n): ")
			if (to_access_char == "y") {
				to_access <- TRUE
				break
			} else if (to_access_char == "n") {
				to_access <- FALSE
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		}    
	} else {
		to_access <- FALSE 
	}
	num_bootstraps <- NULL
	rooting <- NULL
	root <- NULL
	midpoint <- NULL
	mid <- NULL
	cons_char <- NULL
	cons <- NULL
	ogfile <- NULL
	jackfile <- NULL
	addjacks <- NULL
	outgroup <- NULL
	taxdef_char <- NULL
	taxdef <- NULL
	addex_char <- NULL
	addex <- NULL
	addgroups <- NULL
	taxnames <- NULL
	missdat_char <- NULL
	missdat <- NULL
	binary_char <- NULL
	binary <- NULL
	gtype <- NULL
	threads <- NULL
	seqfile <- NULL
	if (!to_access) {
		while (1) {
			cat("BOOTSTRAP NUMBER -\nPlease type in the number of bootstrap replications.\n") 
			num_bootstraps <- readline(": ")
			num_bootstraps <- as.numeric(num_bootstraps)
			if (((num_bootstraps %% 1) == 0) && (num_bootstraps > 0)) {
				break
			} else { 
				cat("Error: The number of bootstrap replications has to be a positive integer!\n")
			}
		}
		while (1) {
			cat("ROOTING (OUTGROUP) -\nDo you want to root the tree using an outgroup?\n")
			rooting <- readline("(y/n): ")
			if (rooting == "y") {
				root <- TRUE
				break
			} else if (rooting == "n") {
				root <- FALSE
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		} 
		if (root) {
			midpoint <- "n"
			mid <- FALSE
		} else {
			while (1) {
				cat("ROOTING (MIDPOINT) -\nDo you want to apply midpoint rooting?\n")
				midpoint <- readline("(y/n): ")
				if (midpoint == "y") {
					mid <- TRUE
					break
				} else if (midpoint == "n") {
					mid <- FALSE
					break
				} else {
					cat("Invalid input. Please try again!\n")
				}
			} 
		}
		cons_char <- "no"
		cons <- FALSE
		if ((root) && (!mid)) {
			while (1) {
				cat("OUTGROUP SPECIFICATION -\nPlease specify the outgroup taxa by which the tree is to\nbe rooted. (separate taxon names by empty spaces)\n[type 'file' for input via text file]\n") 
				outgroup <- readline(": ")
				if (outgroup == "file") {
					cat("Please select your outgroup specification file.\n")
					ogfile <- file.choose(new = TRUE)
					outgroup <- paste(readLines(ogfile), collapse = "")
				}
				if (!is.na(outgroup)) {
				   break
				} else {
				   cat("Missing input. Please try again!\n")
				}
			}
			while (1) {
				cat("CONSISTENT OUTGROUP MONOPHYLY -\nPlease note that inconsistent outgroups could cause errors.\nDo you expect your outgroup to be monophyletic across bootstrap trees?\n(if not: the first sample of your outgroup is used to root the tree)\n")
				cons_char <- readline("(y/n): ")
				if (cons_char == "y") {
				   cons <- TRUE
				   break
				} else if (cons_char == "n") {
				   cons <- FALSE
				   break
				} else {
				   cat("Invalid input. Please try again!\n")
				}
			}
		}
		while (1) {
			cat("DATA FILE TAXON-JACKKNIFING -\nPlease specify whether jackknife sets are specified in the data file.\n(if yes: sample names must have the form 'samplename_set')\n")
			taxdef_char <- readline("(y/n): ")
			if (taxdef_char == "y") {
				taxdef <- TRUE
				break
			} else if (taxdef_char == "n") {
				taxdef <- FALSE
				addex <- TRUE
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		}  
		if (taxdef) {
			while (1) {
				cat("ADDITIONAL TAXON-JACKKNIFE SETS -\nDo you want to specify additional jackknife sets?\n")
				addex_char <- readline("(y/n): ")
				if (addex_char == "y") {
					addex <- TRUE
					break
				} else if (addex_char == "n") {
					addex <- FALSE
					break
				} else {
					cat("Invalid input. Please try again!\n")
				}
			}
		} else {
			addex_char <- "n"
		}
		if ((!taxdef) || (addex)) {
			while (1) {
				cat("TAXON-JACKKNIFE SPECIFICATION -\nPlease specify the taxon-jackknife sets \n(separate sample names by empty spaces and sets by commas).\n(e.g. 01_species1 02_species1 03_species1,01_species2 02_species2)\n[type 'file' for input via text file]\n") 
				if ((addex) && (taxdef)) {
					addgroups <- readline(": ")
					if (addgroups == "file") {
						cat("Please select your additional jackknife specification file.\n")
						addjacks <- file.choose(new = TRUE)
						addgroups <- paste(readLines(addjacks), collapse = "")
					}
					if (!is.na(addgroups)) {
						break
					} else {
						cat("Missing input. Please try again!\n")
					}
				} else {                                             
					taxnames <- readline(": ")
					if (taxnames == "file") {
						cat("Please select your jackknife specification file.\n")
						jackfile <- file.choose(new = TRUE)
						taxnames <- paste(readLines(jackfile), collapse = "")
					}
					if (!is.na(taxnames)) {
						break
					} else {
						cat("Missing input. Pleasy try again!\n")
					}
				}
			}
		}
		while (1) {
			cat("MISSING DATA -\nShould missing data be included? (inclusion of missing\ndata may lead to longer computation times)\n") 
			missdat_char <- readline("(y/n): ")
			if (missdat_char == "y") {
				missdat <- TRUE
				break
			} else if (missdat_char == "n") {
				missdat <- FALSE
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		}
		while (1) {
			cat("BINARY, SNP, OR GENOTYPE -\nIs your data of type binary (b), SNP nucleotide (n),\nor SNP genotype (g)?\n") 
			binary_char <- readline("(b/n/g): ")
			if (binary_char == "b") {
				binary <- TRUE
				gtype <- FALSE
				break
			} else if (binary_char == "n") {
				binary <- FALSE
				gtype <- FALSE
				break
			} else if (binary_char == "g") {
				binary <- FALSE
				gtype <- TRUE
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		}
		while (1) {
			cat("THREADS -\nHow many threads do you want to use in parallel for bootstrapping?\n")
			threads <- as.integer(readline(": "))
			if (is.integer(threads)) {
				break
			} else {
				cat("Invalid input. Please try again!\n")
			}
		}
	}
	while (1) {
		cat("BOXPLOT ORIENTATION -\nDo you want the boxplot orientation to be horizontal (h) or vertical (v)?\n")
		horizontal_char <- readline("(h/v): ")
		if (horizontal_char == "h") {
			horizontal <- TRUE
			break
		} else if(horizontal_char == "v") {
			horizontal <- FALSE
			break
		} else {
			cat("Invalid input. Please try again!\n")
		}
	}
	while (1) {
		cat("BOXPLOT WHISKERS/OUTLIER RANGE -\nUp to which multiple of the interquartile range should the boxplot\nwhiskers extend? (with values more extreme than that being outliers)\n[1.5 is the standard value of R]\n")
		w_range <- as.numeric(readline(": "))
		if (is.na(w_range)) {
			cat("Range must be numeric.\n")
		} else {
			break
		}
	}
	if (!to_access) {
		cat("DATA FILE -\nPlease select a data file in nexus, fasta, or text format.\n")
		flush.console() 
		seqfile <- file.choose(new = TRUE)
		directory <- dirname(seqfile)
	}
	return(list(to_access = to_access, num_bootstraps = num_bootstraps, rooting = rooting, root = root, midpoint = midpoint, mid = mid, cons_char = cons_char, cons = cons, 
		        ogfile = ogfile, jackfile = jackfile, addjacks = addjacks, outgroup = outgroup, taxdef_char = taxdef_char, taxdef = taxdef, addex_char = addex_char, 
				addex = addex, addgroups = addgroups, taxnames = taxnames, missdat_char = missdat_char, missdat = missdat, binary_char = binary_char, binary = binary, 
				gtype = gtype, threads = threads, horizontal = horizontal, w_range = w_range, seqfile = seqfile, directory = directory)) 
}