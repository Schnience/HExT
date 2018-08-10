# written by Kevin Schneider, December 2014
# feedback to: kevin.schneider@edu.uni-graz.at
# or, alternatively, to: kristina.sefc@uni-graz.at or stephan.koblmueller@uni-graz.at
# last modified: 04.07.2015

# a function to collect taxa for jackknifing in a vector
tax_comp <- function (currtax, taxvect) {                          
    excl_tax <- c()
    for (j in 1:length(taxvect)) {     
         if (taxvect[j] == currtax) {                             
            excl_tax <- append(excl_tax, j)                       
         }
    }  
    return(excl_tax)   
}