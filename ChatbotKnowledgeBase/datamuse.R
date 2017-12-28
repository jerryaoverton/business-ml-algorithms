#This function takes in a comma-separated list of words and adds an expanded list of related terms.
#It is based on the Datamuse API

add_related_terms <- function(terms.original){
  
  #break text into individual terms
  terms <- as.list(strsplit(terms.original, ",")[[1]])
  
  #get rid of leading and trailing spaces in terms
  terms.trim <- trimws(terms, which = c("both", "left", "right"))
  
  #turn a term into a query parameter
  parameters <-  gsub('([[:punct:]])|\\s+','+',terms.trim)
  
  #create api query
  dataMuseURL <- "https://api.datamuse.com/words?ml="
  queries <- paste(dataMuseURL,parameters, sep = "")
  
  related.terms.final <- terms.original
  
  try({
    for (query in queries){
      
      library(jsonlite)
      related.terms <- fromJSON(query)
      
      # sort related terms by score
      related.terms <- related.terms[order(-related.terms$score),]
      
      #collapse the top N related terms into a comma seperated list
      num_related_terms = 3
      
      related.terms.collapse <- related.terms[1,]$word
      for(i in 2:num_related_terms){
        related.terms.collapse <- paste(related.terms.collapse, related.terms[i,]$word, sep = ",")
      }
      
      #add the latest set of terms to a final list
      related.terms.final <- paste(related.terms.final, related.terms.collapse, sep = ",")
      
    }
  })
  
  return(related.terms.final)
  
}
