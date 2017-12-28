#Install MS LUIS

if ("mscstexta4r" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("mscstexta4r")
}

library("mscstexta4r")
textaInit()

structureResults <- function(results){
  results.structured <- c()
  i=1
  for (item in results){
    results.structured <- paste(results.structured,item, sep=" ")
    i=i+1
  }
  return(results.structured)
}

collapseResults <- function(list){
  str <- paste(list, collapse=', ' )
  
  return(str)

}

keyPhrases <- function(text){
  docsLanguage <- rep("en", length(text))
  LUIS <- textaKeyPhrases(documents = text, languages = docsLanguage)
  results <- structureResults(LUIS$results$keyPhrases)
  
  results.simplified <- collapseResults(results)
  
  #sleep for 2 seconds to not exceed the Microsoft LUIS API request limit
  Sys.sleep(2)
  
  return(results.simplified)
}

#Test code. Remove
docsText <- "It was a great restaurant. If steak is what you want, this is the place. The atmosphere is pretty bad but the food is quite good."

docsLanguage <- rep("en", length(docsText))
test.keyphrases <- textaKeyPhrases(documents = docsText, languages = docsLanguage)
test.results <- test.keyphrases$results$keyPhrases
test.out <- structureResults(test.results)
test.out.clean <- collapseResults(test.out)