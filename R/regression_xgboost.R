XGBOOST.Regression.Config <- setClass("XGBOOST.Regression.Config", slots = c(booster="character")
  , validity = function(object) {
    #Let's start simple for now  https://xgboost.readthedocs.io/en/latest/parameter.html#additional-parameters-for-dart-booster-booster-dart
    if (object@booster %in% c("gbtree","gblinear","dart")) TRUE
    else "invalid parameters"
  }
  , contains = "Regression.Config"
)

XGBOOST.Regression.FitObj <- setClass("XGBOOST.Regression.FitObj", contains = "Regression.FitObj",
                                      slots = c(mm = "list"))

make.configs.xgboost.regression <- function(df=data.frame(booster=c("gbtree","gblinear","dart"))) {
  df$booster <- as.character(df$booster)
  ret <- lapply(1:nrow(df), function(i) {
    XGBOOST.Regression.Config(booster=df$booster[i])
  })
}

setMethod("BaseLearner.Fit", "XGBOOST.Regression.Config",
  function(object, formula, data, tmpfile=NULL, print.level=10) {
    mf <- model.frame(formula, data, drop.unused.levels=TRUE, na.action = na.fail)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)
    y <- model.response(mf, "numeric")
    est <- xgboost::xgboost(formula, data=X, label= y, 
                                  params=list(booster=object@booster, objective="reg:squarederror")
                                  , verbose=print.level>=1, nrounds = 200)
    # must retrun probabilities 
    pred <- predict(est, newdata=X)

    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- XGBOOST.Regression.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred, mm = list(contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf), terms = mt, colnamesX = colnames(X)))
    return (ret)
  }
)

predict.XGBOOST.Regression.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)

  
  tt <- object@mm$terms
  Terms <- delete.response(tt)
  
  newdata <- droplevels(newdata)
  mf <- model.frame(Terms, newdata, xlev = object@mm$xlevels)
  X <- model.matrix(Terms, mf, contrasts.arg = object@mm$contrasts)
  
  newpred <- predict(object@est, newdata=X)
  #rm(object); gc()
  return (newpred)
}

