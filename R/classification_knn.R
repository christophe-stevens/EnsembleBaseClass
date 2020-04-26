KNN.Classification.Config <- setClass("KNN.Classification.Config", slots = c(kernel="character", k="numeric")
  , validity = function(object) {
    if (object@kernel %in% c("rectangular","epanechnikov","triweight","gaussian","tiangular","biweight","cos","inv","rank","optimal") && 
          object@k>0 && object@k==round(object@k)) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

KNN.Classification.FitObj <- setClass("KNN.Classification.FitObj"
  , slots = c(formula="formula", data="data.frame"), contains = "Classification.FitObj")

make.configs.knn.classification <- function(df=expand.grid(kernel=c("rectangular","epanechnikov","triweight","gaussian"),k=c(5,10,20,40))) {
  ret <- lapply(1:nrow(df), function(i) {
    KNN.Classification.Config(kernel=as.character(df$kernel[i]), k=df$k[i])
  })
}

setMethod("BaseLearner.Fit", "KNN.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    respVar <- all.vars(formula)[1]
    data[,respVar] <- as.factor(data[,respVar]) # required by rf
    est <- kknn::kknn(formula, data, data, k=object@k, kernel=object@kernel)
    pred <- as.character(est$fitted.values)
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- KNN.Classification.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred, formula=formula, data=data)
    return (ret)
  }
)

predict.KNN.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (as.character(object@pred))
  if (is.character(object@est)) object@est <- load.object(object@est)
  newreg <- kknn::kknn(object@formula, object@data, newdata, k=object@config@k, kernel=object@config@kernel)
  #rm(object); gc()
  return (as.character(newreg$fitted.values))
}


