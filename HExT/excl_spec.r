# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# writing jackknife sets to a file
excl_spec <- function (ingroup_strings, directory) {
    ex_sp_frame <- data.frame()
    for (e in 1:length(ingroup_strings)) {
        ex_sp_frame[e, 1] <- paste("Jackknife_Tree_", e, sep = "")
        ex_sp_frame[e, 2] <- ingroup_strings[e] 
    }
    colnames(ex_sp_frame) <- c("Tree", "Jackknife_Set") 
    write.table(ex_sp_frame, file = paste(directory, "/", "jackknife_specification.txt", sep = ""), 
                quote = FALSE, sep = " - ", row.names = FALSE)
}