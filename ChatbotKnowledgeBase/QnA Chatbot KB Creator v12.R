source("docData.R")
dataset <- createDataSet("KB/")

source("LUIS.R")
question <- sapply(dataset$txt, keyPhrases)

source("datamuse.R")
question.expanded <- sapply(question, add_related_terms)

answer <- dataset$txt

kb <- data.frame(question.expanded, answer)
 
#write the knowledge base out to file for import into the chat bot
source("botKB.R")
writeKB(kb, "../KB.xlsx")
