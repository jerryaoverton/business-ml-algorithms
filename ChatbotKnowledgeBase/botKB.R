mergeRulesandDataset <- function(rules.qa, dataset){
  #join the doc id with the original document text into a new knolwdge base documnent
  kb.raw <- merge(rules.qa,dataset,by="doc_id")
  
  return(kb.raw)
}

cleanKB <- function(kb){
  #clean and format the columns of the knowledge base
  kb$doc_id <- NULL
  kb$rules <- NULL
  kb$support <- NULL
  kb$confidence <- NULL
  kb$lift <- NULL
  kb$count <- NULL
  kb$summary <- NULL
  kb$file <- NULL
  names(kb)[names(kb) == 'txt'] <- 'answer'
  
  #clear the blank spaces from the question
  kb$question <- gsub("[[:blank:]]", "", kb$question)
  
  #clean the knowledge base
  kb$question <- as.character(kb$question)
  kb$answer <- as.character(kb$answer)
  
  return(kb)
  
}

createKB <- function(rules.qa, dataset){
  kb.raw <- mergeRulesandDataset(rules.qa, dataset)
  kb.clean <- cleanKB(kb.raw)
  return(kb.clean)
}

writeKB <- function(kb, filename){
  
  #install.packages("xlsx")
  library("xlsx")
  write.xlsx(kb, file=filename, sheetName = "Sheet1", 
             col.names = TRUE, row.names = FALSE, append = FALSE) 
}
