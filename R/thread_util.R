# chris: seems class are not set in environment


sources <-c("./R/utils.R","./R/baselearners.R")
sources <- c(sources, paste("./R/classification_", c("nnet","rf","svm","gbm","knn", "penreg"),".R", sep=""))
sources <- c(sources, paste("./R/regression_", c("nnet","rf","svm","gbm","knn", "penreg"),".R", sep=""))
for (src in sources){
  source(src)
}
