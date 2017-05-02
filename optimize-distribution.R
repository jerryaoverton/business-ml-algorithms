#this script is an experiment in optimization. optimization is applicable in use cases such 
#as deciding on the best prices for products given constraints like fluctuating demand or
#limited supplies.

#in this experiment, we will find the optimal distribution of products to different retail
#locations given a particular demand forecast. when deciding on where to ship our products
#we have to keep in mind that:

#1. products are in fluctuating demand depending on location
#2. for each location there is a different cost to ship products
#3. each location has to satisfy a minumum of their demand, but supply shouldn't exceed demand



#part 1: simulate relail demand data

#determine the number of retail locations to simulate
locations = 500

#simulate logistics records
location <- paste(rep("location",locations),rep(1:locations), sep = "_")
date <- rep(Sys.Date(),locations)
demand.forecast <- sample(0:2000, locations)
distribution.cost.per.unit <- runif(locations, 50, 250)
price.per.unit <- runif(locations, 100, 500)

sim <- data.frame(location, date, demand.forecast, distribution.cost.per.unit, price.per.unit)
sim$profit.per.unit <- sim$price.per.unit - sim$distribution.cost


#part 2: optimize the logisitics performance based on simulated retail demand data
data <- sim

#install.packages("lpSolveAPI")
library(lpSolveAPI)

#create an empty linear model
model <- make.lp(0, locations)

#create an objective function
set.objfn(model, data$profit.per.unit)
lp.control(model,sense='max')

#inventory minimums
inventory.total = locations * 250
inventory.location.quota = .2

#set up constraints:

#the total product distributed must not exceed inventory
add.constraint(model, rep(1,locations), "<=", inventory.total)

#each location must be able to serve a minimum of the demand but must not
#exceed demand
mat <- matrix(0, locations, locations)
diag(mat) <- 1

for (i in 1:locations){
  add.constraint(model, mat[i,], ">=", data[i,]$demand.forecast * inventory.location.quota)
  add.constraint(model, mat[i,], "<=", data[i,]$demand.forecast)
}

#solve the model
solve(model)
data$optimal.inventory <- as.integer(get.variables(model))
data$expected.profit <- data$profit.per.unit * data$optimal.inventory

