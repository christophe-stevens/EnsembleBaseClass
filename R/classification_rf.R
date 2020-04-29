RF.Classification.Config <- setClass("RF.Classification.Config", slots = c(ntree="numeric", nodesize="numeric", mtry.mult="numeric")
  , validity = function(object) {
    if (object@ntree==round(object@ntree) && object@ntree>1 && 
          object@nodesize==round(object@nodesize) && object@nodesize>0 &&
          object@mtry.mult>0) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

RF.Classification.FitObj <- setClass("RF.Classification.FitObj", contains = "Classification.FitObj")

make.configs.rf.classification <- function(df=expand.grid(ntree=c(100,500),mtry.mult=c(1,2),nodesize=c(2,5,25,100))) {
  ret <- lapply(1:nrow(df), function(i) {
    RF.Classification.Config(ntree=df$ntree[i], nodesize=df$nodesize[i], mtry.mult=df$mtry.mult[i])
  })
}

setMethod("BaseLearner.Fit", "RF.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    varnames <- labels(terms(formula))
    respVar <- all.vars(formula)[1]
    data[,respVar] <- as.factor(data[,respVar]) # required by rf
    est <- randomForest::randomForest(formula, data, ntree=object@ntree
      , nodesize=object@nodesize
      , mtry=max(floor(object@mtry.mult*length(varnames)/3), 1), do.trace=print.level>=1, keep.forest=T)
    pred <- predict(est,newdata=data, type="prob")[,"1"]
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- RF.Classification.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred)
    return (ret)
  }
)

predict.RF.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  newpred <- predict(object@est, newdata=newdata, type="prob")[,"1"]
  #rm(object); gc()
  return (newpred)
}






