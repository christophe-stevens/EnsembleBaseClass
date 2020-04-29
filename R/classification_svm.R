SVM.Classification.Config <- setClass("SVM.Classification.Config", slots = c(cost="numeric", epsilon="numeric", kernel="character")
  , validity = function(object) {
   if (object@cost>=0 && object@epsilon>0 && object@kernel %in% c("linear","polynomial","radial","sigmoid")) TRUE
   else "invalid parameters"
  }
  , contains = "Classification.Config"
)

SVM.Classification.FitObj <- setClass("SVM.Classification.FitObj", contains = "Classification.FitObj")


make.configs.svm.classification <- function(df=expand.grid(cost=c(0.1,0.5,1.0,5.0,10,50,75,100), epsilon=c(0.1,0.25), kernel="radial")) {
  ret <- lapply(1:nrow(df), function(i) {
    SVM.Classification.Config(cost=df$cost[i], epsilon=df$epsilon[i], kernel=as.character(df$kernel[i]))
  })
}

setMethod("BaseLearner.Fit", "SVM.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=10) {
    respVar <- all.vars(formula)[1]
    est <- e1071::svm(formula, data, kernel=object@kernel, cost=object@cost, epsilon=object@epsilon,type="C-classification", probability=TRUE)
    pred <- attr(predict(est, newdata=data, probability=TRUE),"probabilities")[,"1"]
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- SVM.Classification.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred)
    return (ret)
  }
)

predict.SVM.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  newpred <- attr(predict(object@est, newdata=newdata, na.action=na.pass, probability=TRUE),"probabilities")[,"1"]
  #rm(object); gc()
  return (newpred)
}



