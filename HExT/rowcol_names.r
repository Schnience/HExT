# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# function to assign row and column names to the input matrix
rowcol_names <- function (input_matrix, rows_only = FALSE) {
    row_names <- c()
    col_names <- c()
    for (p in 1:nrow(input_matrix)) {
        if (p == 1) {
            row_names[p] <- paste("full_tree_0")
        } else {
            row_names[p] <- paste("jack_tree_", p - 1, sep = "")
        }
    }
    if (!rows_only) {
        for (r in 1:ncol(input_matrix)) {
            col_names[r] <- paste("node_#", r, sep = "")
        }
    }
    rownames(input_matrix) <- row_names
    colnames(input_matrix) <- col_names
    return(input_matrix) 
}