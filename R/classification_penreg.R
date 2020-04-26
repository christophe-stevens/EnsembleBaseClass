PENREG.Classification.Config <- setClass("PENREG.Classification.Config"
  , slots = c(alpha = "numeric", lambda = "numeric")
  , validity = function(object) {
    if (object@alpha >= 0.0 && object@alpha <= 1.0 && object@lambda >= 0.0) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

# since glmnet doesn't have a formula method, we must carry extra object needed for consistent model matrix expansion during prediction
PENREG.Classification.FitObj <- setClass("PENREG.Classification.FitObj", slots = c(respVarName = "character"), contains = "Classification.FitObj")

make.configs.penreg.classification <- function(df = expand.grid(alpha = 0.0, lambda = 10^(-8:+7))) {
  ret <- lapply(1:nrow(df), function(i) {
    PENREG.Classification.Config(alpha = df$alpha[i], lambda = df$lambda[i])
  })
}

setMethod("BaseLearner.Fit", "PENREG.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    respVar <- all.vars(formula)[1]
    data[,respVar] <- as.factor(data[,respVar])
    idxResponse <- which(colnames(data)==respVar)
    X <- as.matrix(data[,-idxResponse])
    est <- glmnet::glmnet(x=X, y=data[,respVar], family = "binomial", lambda = object@lambda, alpha = object@alpha)
    pred <- as.character(as.vector(predict(est, newx=X, type= "class")))
    
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- PENREG.Classification.FitObj(config = object
      , est = if (is.null(tmpfile)) est else tmpfile
      , pred = pred, respVarName = respVar)
    return (ret)
  }
)

predict.PENREG.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)

  idxResp = which(colnames(newdata)==object@respVarName)
  newdata <- newdata[,-idxResp]
  X <- as.matrix(newdata)

  newpred <- as.character(as.vector(predict(object@est, newx=X, type="class")))
  #rm(object); gc()
  return (newpred)
}




