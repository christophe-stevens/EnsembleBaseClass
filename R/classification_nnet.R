NNET.Classification.Config <- setClass("NNET.Classification.Config", slots = c(decay="numeric", size="numeric", maxit="numeric")
  , validity = function(object) {
    if (object@decay>=0 && round(object@size)==object@size && object@size>0 &&
      object@maxit>0 && round(object@maxit)==object@maxit) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

NNET.Classification.FitObj <- setClass("NNET.Classification.FitObj"
  , contains = "Classification.FitObj")

make.configs.nnet.classification <- function(df=expand.grid(decay=c(1e-4,1e-2,1,100), size=c(5,10,20,40), maxit=2000)) {
  ret <- lapply(1:nrow(df), function(i) {
    NNET.Classification.Config(decay=df$decay[i], size=df$size[i], maxit=df$maxit[i])
  })
}

setMethod("BaseLearner.Fit", "NNET.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    respVar <- all.vars(formula)[1]
    data[,respVar] <- as.factor( data[,respVar] )
    est <- nnet::nnet(formula, data, size=object@size, decay=object@decay, maxit=object@maxit, trace=print.level>=1)
    pred <- as.vector(predict(est))
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- NNET.Classification.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred)
    return (ret)
  }
)

predict.NNET.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  newpred <- as.vector(predict(object@est, newdata=newdata))
  #rm(object); gc()
  return (newpred)
}






