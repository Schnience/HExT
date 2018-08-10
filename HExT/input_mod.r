# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# Here, parsing of the input from the function "interact" takes place.
input_mod <- function (root, outgroup, taxdef, taxnames, addex, addgroups, seqfile, missdat, binary, ogfile, jackfile, addjacks, num_bootstraps, rooting, midpoint, cons_char, taxdef_char,
					   addex_char, missdat_char, binary_char, threads, directory, gtype) {
	taxvect <- NULL
	outgroup_taxa <- NULL
	length_addgroups <- NULL
	length_taxnames <- NULL
	length_groupdiff <- NULL
	if (root) {      
		outgroup <- inp_manip(outgroup)
	}                  
	if (!taxdef) {                                     
		taxnames <- inp_manip(taxnames, specify = TRUE)
	} else if (addex) {
		addgroups <- inp_manip(addgroups, specify = TRUE) 
	}
	seqlist <- file_input(seqfile, taxdef, missdat, binary)
	do.call("write_par", list(seqfile, ogfile, jackfile, addjacks, num_bootstraps, rooting, midpoint, outgroup, cons_char, taxdef_char, 
	                          addex_char, missdat_char, binary_char, threads, directory, w_range))
	iseq <- seqlist[[1]]
	if (taxdef) {
		taxvect <- seqlist[[2]]
		taxnames <- seqlist[[3]]
	}
	fulltaxnum <- ncol(iseq)
	if (root) {                                        
		if (taxdef) {
		   outgroup_taxa <- sub(".*_", "", outgroup)
		   outgroup_taxa <- unique(outgroup_taxa)
		   taxnames <- taxnames[!taxnames %in% outgroup_taxa]
		}
	}                                      
	ingroup <- list()                                       
	trimmed_OTU_nums <- c()
	if ((addex) && (taxdef)) {
		length_addgroups <- length(addgroups) 
		length_taxnames <- length(taxnames) + length_addgroups
		length_groupdiff <- length_taxnames - length_addgroups
	} else {
		length_taxnames <- length(taxnames) 
	}
	alltrees_list <- list()
	allclade_list <- list()
	iseq <- t(iseq)
	exclusions <- c()
	if (gtype) {
		iseq <- make_bin(iseq)
	}
	return(list(outgroup = outgroup, taxnames  = taxnames, addgroups = addgroups, iseq = iseq, taxvect = taxvect, taxnames = taxnames, fulltaxnum = fulltaxnum, 
				outgroup_taxa = outgroup_taxa, ingroup = ingroup, trimmed_OTU_nums = trimmed_OTU_nums, length_addgroups = length_addgroups, length_taxnames = length_taxnames, 
				length_groupdiff = length_groupdiff, alltrees_list = alltrees_list, allclade_list = allclade_list, iseq = iseq, exclusions = exclusions))
}