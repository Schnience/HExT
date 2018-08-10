# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 06.07.2015

# function for the manipulation of boxplots created in the access_custom function
plot_manip <- function () {
    while (1) {
            cat("SAVE -\nDo you want to save the boxplots and underlying data of the\nspecified nodes to a file?\n") 
            store_char <- readline("(y/n): ")
            if (store_char == "y") {
                store <- TRUE
                break
            } else if (store_char == "n") {
                store <- FALSE
                break
            } else {
                cat("Invalid input. Please try again!\n") 
            }
    }
    sctl_list <- list()
    sctl_list[[1]] <- store
    if (store) {
        while (1) {
            cat("CHANGE TITLE & LABELS -\nDo you want to change the plot title and tick labels?\n")
            change_char <- readline("(y/n): ")
            if (change_char == "y") {
                change <- TRUE
                break
            } else if (change_char == "n") {
                change <- FALSE
                break
            } else {
                cat("Invalid input. Please try again!\n") 
            }
        }
        sctl_list[[2]] <- change
        if (change) {
            while (1) {
                cat("TITLE & LABEL SPECIFICATION -\nSpecify the plot title first, followed by the tick labels.\nPlot title and tick labels should be separated by a comma,\nwhile the tick labels should be separated by empty spaces.\n(e.g. main title,label1 label2 label3)\n") 
                title_labels <- readline(": ")
                if (!is.na(title_labels)) {
                    break
                } else {
                    cat("Missing input. Please try again!\n") 
                }
            }
            sctl_list[[3]] <- inp_manip(title_labels, specify = TRUE)
        }
    }
    return(sctl_list)   
}