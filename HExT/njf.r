# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# the function njf calls phylogenetic tree construction and rooting functions
njf <- function (charmat, col_names, resample, outgroup, cons, binary, gtype, root, numrow_charmat = NULL, nloci = NULL, mid) {
    tree <- nj(seq_dist(charmat, col_names, resample, binary, gtype, numrow_charmat, nloci))
    if (root) {
		if (mid) {
			tree <- midpoint(tree)
		} else {
			if (cons) {
				tree <- root(tree, outgroup, resolve.root = TRUE)
			} else {
				root_taxon <- outgroup[1]
				tree <- root(tree, root_taxon, resolve.root = TRUE)
			}
		}
    }
	if (!mid) {
		tree <- ladderize(tree)
	}
    return(tree)                                                     
}