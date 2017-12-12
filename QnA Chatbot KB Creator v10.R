#Read a corpus of training documents
setwd("KB/")

#create a blank data set with a dummy row
txt <- NA
file <- NA
dataset <- data.frame(txt, file)

#Get a list of files in the directory
file_list <- list.files()

#Merge the content from all files into a single data frame
for (file in file_list){
  
  #read paragraphs and file name from the current file into a dataframe
  txt <- readLines(file)
  txt.df <- data.frame(txt, file)
  
  #remove the blankspaces
  txt.df[txt.df==""]<-NA
  txt.df <- txt.df[complete.cases(txt.df),]
  
  #append contents from the current file into the return results
  dataset<-rbind(dataset, txt.df)
  
}

#Remove the first dummy row from the data set
txt <- dataset[-1,]
dataset <- as.data.frame(txt)

#Format the text in the dataset
dataset$txt <- as.character(dataset$txt)



#add a summary to the dataset
dataset$summary <- dataset$txt

#create a DTM from the data set

#build a corpus (a collection of text documents)
#install.packages("tm")
library(tm)
corpus <- Corpus(VectorSource(dataset$summary))

#clean the corpus:
corpus <- tm_map(corpus, tolower) #make everything lower case
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus, removeWords, stopwords('english')) #remove stop words

#take a look at the first 2 documents in the corpus
inspect(corpus[1:2])

#create a document term matrix from the corpus
dtm <- DocumentTermMatrix(corpus)

#create variables for determining minimum and maximum word span
span_mid_point = 0.030
half_span_range = 0.015

#remove overly common terms
max_word_span = span_mid_point + half_span_range
removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}
dtm <- removeCommonTerms(dtm, max_word_span)

#remove sparse terms
min_word_span = span_mid_point - half_span_range
max_sparsity = 1- min_word_span
dtm <- removeSparseTerms(dtm, max_sparsity)

#convert the document term matrix into a data frame
dtm.m <- as.matrix(dtm)
dtm.df <- as.data.frame(dtm.m)

#factorize the document term matrix so that it can be used to create association rules
dtm.df <- as.data.frame(dtm.df > 0)

#add a document id to the data set and document term matrix
dataset$doc_id <- as.integer(1:nrow(dataset))
dtm.df$doc_id <- as.factor(dataset$doc_id)

#Create association rules that best identify the documents


#create a list of RHS conditions to look for
rhs.conditions <- paste0("doc_id=", 1:nrow(dataset))

# find association rules with default settings
#install.packages("arules")
library(arules)

#rules with rhs containing "doc_id" only
rules <- apriori(dtm.df, control = list(verbose=F),
                 parameter = list(minlen=3, maxlen=3, supp=0.003, conf=0.8), 
                 appearance = list(rhs=rhs.conditions, 
                                   default="lhs"))

#inspect the rules
rules.df <- as (rules, "data.frame")

# remove redundant rules
rules.pruned <- rules[!is.redundant(rules)]

#inspect the pruned rules
rules.pruned.df <- as (rules.pruned, "data.frame")

#clean and reformat the rules data
rules.pruned.df$rules <- gsub('\\{', '', rules.pruned.df$rules)
rules.pruned.df$rules <- gsub('\\}', '', rules.pruned.df$rules)

#split the rules into 2 distinct columns
#install.packages("stringr")
library(stringr)
cols <- str_split_fixed(rules.pruned.df$rules, "=>", 2)
rules.pruned.df$question <- cols[,1]
rules.pruned.df$doc_id <- cols[,2]

#clean and format the document id data
rules.pruned.df$doc_id <- gsub('doc_id=', '', rules.pruned.df$doc_id)
rules.pruned.df$doc_id <-  as.integer(rules.pruned.df$doc_id)

#join the doc id with the original document text into a new knolwdge base documnent
kb <- merge(rules.pruned.df,dataset,by="doc_id")


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

#install.packages("xlsx")
library("xlsx")
write.xlsx(kb, file="../KB.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
