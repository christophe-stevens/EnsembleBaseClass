GBM.Classification.Config <- setClass("GBM.Classification.Config", slots = c(n.trees="numeric", interaction.depth="numeric", shrinkage="numeric", bag.fraction="numeric")
  , validity = function(object) {
    if (object@n.trees==round(object@n.trees) && object@n.trees>0 && 
          object@interaction.depth==round(object@interaction.depth) && object@interaction.depth>0 && 
          object@shrinkage>0 && object@bag.fraction>=0 && object@bag.fraction<=1.0) TRUE
    else "invalid parameters"
  }
  , contains = "Classification.Config"
)

GBM.Classification.FitObj <- setClass("GBM.Classification.FitObj", contains = "Classification.FitObj")

make.configs.gbm.classification <- function(df=expand.grid(n.trees=c(1000,2000),interaction.depth=c(3,4),shrinkage=c(0.001,0.01,0.1,0.5), bag.fraction=0.5)) {
  ret <- lapply(1:nrow(df), function(i) {
    GBM.Classification.Config(n.trees=df$n.trees[i], interaction.depth=df$interaction.depth[i], shrinkage=df$shrinkage[i], bag.fraction=df$bag.fraction[i])
  })
}

setMethod("BaseLearner.Fit", "GBM.Classification.Config",
  function(object, formula, data, tmpfile=NULL, print.level=10) {
    respVar <- all.vars(formula)[1]
    data[,respVar] <- as.character(data[,respVar]) # required by bgm
    est <- gbm::gbm(formula, distribution="bernoulli", data=data, n.trees=object@n.trees, interaction.depth=object@interaction.depth
               , bag.fraction=object@bag.fraction, shrinkage=object@shrinkage, verbose=print.level>=1)
    # must retrun probabilities 
    pred <- predict(est, newdata=data, n.trees=object@n.trees, type="response")
    #pred <- factor(pred, level=list("0","1"), label="0","1")
    if (!is.null(tmpfile)) {
      save(est, file=tmpfile, compress=FALSE)
      rm(est); gc()
    }
    ret <- GBM.Classification.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred)
    return (ret)
  }
)

predict.GBM.Classification.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  newpred <- predict(object@est, newdata=newdata, n.trees=object@config@n.trees, type="response")
  #rm(object); gc()
  return (newpred)
}

