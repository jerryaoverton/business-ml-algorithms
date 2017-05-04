#this script is an experiment in using machine learning to perform automatic triage. triage is
#applicable in use cases such as automatically prioritizing customer support requests or prioritizing
#the order of patient treatment

#in this experiment we use machine learning to triage customer complaints to a financial institution

#read in the customer complaints
data <- read.csv("consumer_complaints_sample.csv")

#filter the data down to the columns we need for analysis
cols <- c("Complaint.ID", "Product", "Consumer.complaint.narrative")
data <- data[,cols]

#build a corpus (a collection of text documents) from the consumer complaint narrative
#install.packages("tm")
library(tm)
corpus <- Corpus(VectorSource(data$Consumer.complaint.narrative))

#clean the corpus:
corpus <- tm_map(corpus, tolower) #make everything lower case
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus, removeWords, stopwords('english')) #remove stop words

#install.packages("SnowballC")
library(SnowballC)
corpus <- tm_map(corpus, stemDocument, language = "english") #stem the words

#take a look at the first 2 documents in the corpus
inspect(corpus[1:2])

#create a document term matrix from the corpus
dtm <- DocumentTermMatrix(corpus)

#remove sparse terms
sparse = .75
dtm <- removeSparseTerms(dtm, sparse)

#convert the document term matrix into a data frame
dtm.m <- as.matrix(dtm)
dtm.df <- as.data.frame(dtm.m)

#create clusters from k-means
num_clusters = 4
kmeans.result <- kmeans(dtm.df, num_clusters)

#add the cluster assignment to the original data
data$cluster <- kmeans.result$cluster

#define a function that allows us to plot the clusters using a histogram
int.hist = function(x) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n");axis(1)
}
int.hist(data$cluster)

#add the cluster assignment to the document term matrix
dtm.df$cluster <- kmeans.result$cluster

#use a decision tree to analyze the content difference between the clusters

# train a decision tree
library(party)
formula <- cluster ~ .
dtree <- ctree(formula, data = dtm.df, controls = ctree_control(maxdepth = 3))

#plot the tree
plot(dtree)

#it turns out that the amount of redacted information is a key indicator. most people had a very
#specific complaint tha they described using personal information. but there were smaller groups
#with more general, and often more emotional, complaints. customers describing those situations
#tended to use fewer personal details.