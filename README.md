devtools::install_github("christophe-stevens/EnsembleBaseClass")
require("EnsembleBaseClass") 


# Data preparation
require(mlbench)  # install.packages("mlbench")  
data(PimaIndiansDiabetes)
dataset <- PimaIndiansDiabetes
# The response variable should be a vector with 0 and 1, and as a factor.  
dataset$diabetes <- as.factor(ifelse(dataset$diabetes=="pos",1,0))
myformula <- diabetes ~ insulin + age + triceps
perc.train <- 0.7
index.train <- sample(1:nrow(dataset), size = round(perc.train*nrow(dataset)))
data.train <- dataset[index.train,]
data.predict <- dataset[-index.train,]

myconfigs <- make.configs("knn", type="classification")
ret <- Classification.Batch.Fit(myconfigs, myformula, data.train, ncores=2)
newpred <- predict(ret, data.predict)
plot(ret)
print(ret)


parts <- generate.partitions(1, nrow(data.train))
myconfigs <- make.configs("knn", type="classification")
instances <- make.instances(myconfigs, parts)
ret <- Regression.CV.Batch.Fit(instances, myformula, data.train)
newpred <- predict(ret, data.predict)
