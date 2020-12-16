#' K-Fold Cross Validation
#' @description Perform K-fold cross-validation for a simple linear regression with one explanatory
#' variable
#'
#' @param y The dependent variable
#' @param x The explanatory variable
#' @param K The number of folds to use (default 5)
#'
#' @return R-squared and RMSE of the cross-validation
#' @export
#'
#' @examples k_fold_CV(y, x, K=10)
k_fold_CV = function(y, x, K=5) {
  folds = cut(seq(1,length(x)), breaks=K, labels=FALSE)

  #Perform K-fold cross validation
  R2 = numeric(K)
  RMSE = numeric(K)
  for(i in 1:K){
    testIdx = which(folds==i,arr.ind=TRUE)
    x.test= x[testIdx]
    y.test = y[testIdx]
    x.train = x[-testIdx]
    y.train = y[-testIdx]

    # Regression using the training set
    mod = stats::lm(y.train ~ x.train)
    predictions = stats::predict(mod, newdata=data.frame(x=x.test))

    mod = stats::lm(y.test ~ predictions)
    R2[i] = summary(mod)$r.squared
    RMSE[i] = sqrt(mean(summary(mod)$residuals^2))
  }

  return(cbind(mean(R2), mean(RMSE)))
}
