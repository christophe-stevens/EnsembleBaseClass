BART.Classification.Config <- setClass("BART.Classification.Config"
  , slots = c(num_trees = "numeric", k = "numeric")
  , validity = function(object) {
    if (object@num_trees == round(object@num_trees) && object@num_trees > 0 && 
        object@k > 0  # seems like k can be fractional, so no validation check included for being integer 
        
        ) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

BART.Classification.FitObj <- setClass("BART.Classification.FitObj", slots = c(mm = "list"), contains = "Classification.FitObj")

make.configs.bart.classification <- function(df = cbind(expand.grid(num_trees = c(5,15,20), k = c(1,2,4)))) {
  ret <- lapply(1:nrow(df), function(i) {
    BART.Classification.Config(num_trees = df$num_trees[i], k = df$k[i])
  })
}

setMethod("BaseLearner.Fit", "BART.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    mf <- model.frame(formula, data, drop.unused.levels=TRUE, na.action = na.fail)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)
    y <- as.factor(model.response(mf, "numeric"))
    est <- bartMachine::bartMachine(X = as.data.frame(X), y = y, num_trees = object@num_trees,
                                    k = object@k, 
                       , verbose=print.level>=1, mem_cache_for_speed = FALSE,
                       serialize = TRUE)
    # Probabilities are inversed.  
    # https://github.com/kapelner/bartMachine/issues/10
    pred <- 1-as.numeric(predict(est, new_data = as.data.frame(X), type="prob"))
    gc()
    
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- BART.Classification.FitObj(config = object
      , est = if (is.null(tmpfile)) est else tmpfile
      , pred = pred
      , mm = list(contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf), terms = mt, colnamesX = colnames(X))
    )
    return (ret)
  }
)

predict.BART.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  
  tt <- object@mm$terms
  Terms <- delete.response(tt)
  
  newdata <- droplevels(newdata)
  mf <- model.frame(Terms, newdata, xlev = object@mm$xlevels)
  X <- model.matrix(Terms, mf, contrasts.arg = object@mm$contrasts)
  
  # Probabilities are inversed.  
  # see https://github.com/kapelner/bartMachine/issues/10
  newpred <- 1-as.numeric(predict(object@est, new_data = as.data.frame(X), type = "prob"))
  #rm(object); gc()
  return (newpred)
}

