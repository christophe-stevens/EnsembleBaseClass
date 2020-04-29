PENREG.Classification.Config <- setClass("PENREG.Classification.Config"
  , slots = c(alpha = "numeric", lambda = "numeric")
  , validity = function(object) {
    if (object@alpha >= 0.0 && object@alpha <= 1.0 && object@lambda >= 0.0) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

# since glmnet doesn't have a formula method, we must carry extra object needed for consistent model matrix expansion during prediction
PENREG.Classification.FitObj <- setClass("PENREG.Classification.FitObj",
                                         slots = c(mm = "list"),  
                                         contains = "Classification.FitObj")

make.configs.penreg.classification <- function(df = expand.grid(alpha = 0.0, lambda = 10^(-8:+7))) {
  ret <- lapply(1:nrow(df), function(i) {
    PENREG.Classification.Config(alpha = df$alpha[i], lambda = df$lambda[i])
  })
}

setMethod("BaseLearner.Fit", "PENREG.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    mf <- model.frame(formula, data, drop.unused.levels=TRUE, na.action = na.fail)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)
    y <- as.factor(model.response(mf, "numeric"))
    est <- glmnet::glmnet(x=X, y=y, family = "binomial", lambda = object@lambda, alpha = object@alpha)
    pred <- as.vector(predict(est, newx=X, type= "response"))
    
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- PENREG.Classification.FitObj(config = object
      , est = if (is.null(tmpfile)) est else tmpfile
      , pred = pred, 
      mm = list(contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf), terms = mt, colnamesX = colnames(X)))
    return (ret)
  }
)

predict.PENREG.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  
  tt <- object@mm$terms
  Terms <- delete.response(tt)
  
  newdata <- droplevels(newdata)
  mf <- model.frame(Terms, newdata, xlev = object@mm$xlevels)
  X <- model.matrix(Terms, mf, contrasts.arg = object@mm$contrasts)
  
  newpred <- as.vector(predict(object@est, newx=X, type="response"))
  #rm(object); gc()
  return (newpred)
}




