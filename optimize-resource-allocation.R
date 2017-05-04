#this script is an experiment in optimizing resource allocation. optimizing resource
#allocation is applicable in use cases such as optimizing labor staffing and distribution 
#to reduce operational costs in the back office, or optimizing the placement of business
#branches to best serve customer demand

#in this experiment, we will optimize the allocation of repair staff needed to service
#vehicle warranty claims

#read in raw vehicle warranty claims
resources <- read.csv('warranty_claims_resources.csv', stringsAsFactors = FALSE)

#simulate claims processed at a particular site by taking a random sample
resources <- resources[sample(1:nrow(resources), 1000, replace=FALSE),]

#use clustering to discover patterns of resource useage

#place claims resource records into common clusters
num_clusters = 5
resources.kmeans <- kmeans(resources[2:5], num_clusters)

#assign clusters to each claim
resources$cluster <- resources.kmeans$cluster
resources$cluster <- as.factor(resources$cluster)

#the cluster assignments represent patterns of common resource usage

#plot the distribution of clusters
plot(resources$cluster)

#display the characteristics of the clusters. display the results using integers
#to make the data easier to understand
cols <- c("Exhaust.System.Specialists", "Intake.System.Specialists", 
          "Fuel.Emissions.Specialists", "Drivetrain.Specialists",
          "cluster")
cluster.analysis <- aggregate(. ~ cluster,resources[,cols], mean)
cluster.analysis[,2:5] <- sapply(cluster.analysis[,2:5], round)

#use regression to predict the optimal allocation of resource

#aggregate, by date, the number of claims from each cluster
resources$claims <- 1
resources.trend <- aggregate(claims ~ Repair.Date + cluster, resources, FUN = sum)

#predict optimal resource allocation
predictresourcealloc <- function(cluster_label, days_forecasted, trend){
  #collect the trend data for claims of type 1
  trend.typeX <- trend[trend$cluster == cluster_label,]
  trend.typeX$cluster <- NULL
  
  #rename the trend columns so that we can fit a time series model
  names(trend.typeX)[names(trend.typeX) == 'Repair.Date'] <- 'ds'
  names(trend.typeX)[names(trend.typeX) == 'claims'] <- 'y'
  
  #predict the future demand for claims
  
  #install.packages("prophet")
  library(prophet)
  
  #Fit a time-series model
  model <- prophet(trend.typeX)
  
  #Make a prediction for claims
  future <- make_future_dataframe(model, periods = days_forecasted)
  forecast <- predict(model, future)
  plot(model, forecast)
}

#predict the optimal number of intake exhaust teams to keep on staff for the next 7 days
predictresourcealloc('5', 7, resources.trend)
