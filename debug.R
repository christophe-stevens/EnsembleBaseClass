rm(list=ls())


##############################
#
#
# LOAD
#
#
##############################
options(java.parameters = "-Xmx5g") # adpat for available ram (check using task manager in winows/df in linux?)
pkg <- c("doParallel","parallel","foreach","e1071", "gbm",
         "randomForest","nnet","kknn","glmnet","bartMachine","xgboost")
sapply(pkg, library, character.only =T)

source("./R/thread_util.R")

##############################
#
#
# CLASSIFICATION
#
#
##############################

# Data preparation
require(mlbench)  # install.packages("mlbench")  
data(PimaIndiansDiabetes)
dataset <- PimaIndiansDiabetes
# The response variable should be a vector with 0 and 1, and as a factor.  
dataset$diabetes <- as.numeric(ifelse(dataset$diabetes=="pos",1,0))
myformula <- diabetes ~ insulin + age + triceps
perc.train <- 0.7
index.train <- sample(1:nrow(dataset), size = round(perc.train*nrow(dataset)))
data.train <- dataset[index.train,]
data.predict <- dataset[-index.train,]

#learners 
all.not.bart <- c("svm","nnet","knn","rf","penreg","xgboost")
only.bart <- c("bart")


myconfigs <- make.configs(all.not.bart ,type="classification")  # c("xgboost","svm","nnet","knn","rf","penreg") bart is slower
ret <- Classification.Batch.Fit(myconfigs, myformula, data.train, ncores=8)
plot(ret)
validate(ret, newdata= data.predict, formula = myformula)
newpred <- predict(ret, data.predict)
table(round(newpred[1:230,3]),data.predict$diabetes)

parts <- generate.partitions(1, nrow(data.train))
myconfigs <- make.configs(all.not.bart, type="classification")
instances <- make.instances(myconfigs, parts)
ret <- Classification.CV.Batch.Fit(instances, myformula, data.train)
newpred <- predict(ret, data.predict)
plot(ret)

mypartition <- generate.partition(nrow(data.train),nfold=3)
ret <- Classification.CV.Fit(myconfigs[[1]], myformula, data.train, mypartition)
newpred <- predict(ret, data.predict)
table(round(newpred), data.predict$diabetes)




rm(list=ls())
##############################
#
#
# LOAD
#
#
##############################
options(java.parameters = "-Xmx2400M")
pkg <- c("doParallel","parallel","foreach","e1071", "gbm",
         "randomForest","nnet","kknn","glmnet","bartMachine","xgboost")
sapply(pkg, library, character.only =T)

source("./R/thread_util.R")

##############################
#
#
# REGRESSION
#
#
##############################


data(servo)
myformula <- class~motor+screw+pgain+vgain
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]

#learners 
all.not.bart <- c("svm","nnet","knn","rf","penreg","xgboost")
only.bart <- c("bart")


myconfigs <- make.configs(all.not.bart)
ret <- Regression.Batch.Fit(myconfigs, myformula, data.train, ncores=8)
newpred <- predict(ret, data.predict)
plot(ret)

parts <- generate.partitions(1, nrow(data.train))
myconfigs <- make.configs(all.not.bart)
instances <- make.instances(myconfigs, parts)
ret <- Regression.CV.Batch.Fit(instances, myformula, data.train)
newpred <- predict(ret, data.predict)
plot(ret)

mypartition <- generate.partition(nrow(data.train),nfold=3)
ret <- Regression.CV.Fit(myconfigs[[1]], myformula, data.train, mypartition)
newpred <- predict(ret, data.predict)


