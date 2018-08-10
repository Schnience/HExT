                                    ################## HExT: Homoplasy Excess Test ##################
									
# written by Kevin Schneider, July 2015
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015
# this program makes extensive use of the R package "ape" 
# Popescu, A. A., Huber, K. T., & Paradis, E. (2012). ape 3.0: New tools for distance-based phylogenetics and evolutionary analysis in R. Bioinformatics, 28(11), 1536-1537.
# Paradis, E., Claude, J., & Strimmer, K. (2004). APE: analyses of phylogenetics and evolution in R language. Bioinformatics, 20(2), 289-290.
# Schliep, K. P. (2011). phangorn: Phylogenetic analysis in R. Bioinformatics, 27(4), 592-593.
# furthermore, the R package "phangorn" and the basic R package "tools" as well as the R packages "doMC" (UNIX), "doSNOW" (Windows) and "foreach" are used  

library(ape)
library(phangorn)
library(tools) 
if (.Platform$OS.type == "unix") {
    library(doMC)
} else { 
    library(doSNOW)
}
library(foreach)

script_directory <- dirname(sys.frame(1)$ofile)
file_list <- list("access_alttop.r", "access_custom.r", "all_descendants.r", "alttop_search.r", "boot_excl.r", "descendants.r", "excl_spec.r", "fas_parse.r", 
			      "file_input.r", "geno_parse.r", "get_trees.r", "inp_manip.r", "input_mod.r", "interact.r", "make_bin.r", "nex_parse.r", "njf.r", "node_comp.r", 
			      "outlier_sink.r", "parse_trees.r", "plot_manip.r", "rowcol_names.r", "seq_dist.r", "sisternodes.r", "store_plot.r", "tax_comp.r", "write_nodes.r", 
			      "write_par.r")
for (f in 1:length(file_list)) {
	source(paste(script_directory, "/", file_list[[f]], sep = ""))
}
interact_list <- interact()
for (i in 1:length(interact_list)) {
	assign(names(interact_list)[i], interact_list[[i]])
}
if (!to_access) {
	input_mod_list <- input_mod(root, outgroup, taxdef, taxnames, addex, addgroups, seqfile, missdat, binary, ogfile, jackfile, addjacks, num_bootstraps, rooting, midpoint, 
		                        cons_char, taxdef_char, addex_char, missdat_char, binary_char, threads, directory, gtype)
	for (j in 1:length(input_mod_list)) {
		assign(names(input_mod_list)[j], input_mod_list[[j]])
	}
	get_trees_list <- get_trees(length_taxnames, root, mid, iseq, taxdef, addex, length_groupdiff, addgroups, taxnames, taxvect, gtype, outgroup, cons, binary, directory, 
		                        threads, num_bootstraps)
	for (k in 1:length(get_trees_list)) {
		assign(names(get_trees_list)[k], get_trees_list[[k]])
	}
	parse_trees_list <- parse_trees(alltrees_list, length_taxnames, root, mid, allclade_list, ingroup, num_bootstraps, directory, horizontal, w_range)
	for (l in 1:length(parse_trees_list)) {
		assign(names(parse_trees_list)[l], parse_trees_list[[l]])
	}
	HExT_list <- list(allboot_list = allboot_list, descend_list = descend_list, num_bootstraps = num_bootstraps, length_taxnames = length_taxnames, 
					  ingroup_strings = ingroup_strings, directory = directory, alttop_frame = alttop_frame)
	save(HExT_list, file = "HExT.RData")
	save(HExT_list, file = paste(directory, "/HExT.RData", sep = ""))
	cat("\nTree and bootstrap data was written to 'HExT.RData' in your current directory.\nYou can access and explore this data in future sessions.\n")
} else {
	load("HExT.RData") 
	for (m in 1:length(HExT_list)) {
		if (m != 6) {
			assign(names(HExT_list)[m], HExT_list[[m]])
		} else if (file.exists(HExT_list[[6]])) {
			assign(names(HExT_list)[m], HExT_list[[m]])
		} else {
			directory <- dirname("HExT.RData")
		}
	}
}
do.call("access_custom", list(allboot_list, alttop_frame, descend_list, num_bootstraps, length_taxnames, 
                              horizontal, w_range, ingroup_strings, directory, to_access))