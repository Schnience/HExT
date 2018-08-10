# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 17.07.2015

# splitting the input string (from jackknife or outgroup specification) to obtain 
# a vector of strings (taxon names) 
inp_manip <- function (inp_names, specify = FALSE) {
    if (specify == FALSE) {
        inp_names <- strsplit(inp_names, split = " ", fixed = TRUE) 
        inp_names <- unlist(inp_names)                                                              
    } else {  
        inp_names <- strsplit(inp_names, split = ",", fixed = TRUE)
        inp_names <- unlist(inp_names)
        names_list <- list()
        for (l in 1:length(inp_names)) {
            names_list[[l]] <- inp_names[l]    
        }
        inp_names <- lapply(names_list, function(x) unlist(strsplit(x, split = " ", fixed = TRUE)))
    }
    return(inp_names)
}