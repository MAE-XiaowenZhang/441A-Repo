#' Test Cointegration
#' @description Test if two series of data are cointegrated at a specific significance level
#'
#' @param vec1 The first series
#' @param vec2 The second series
#' @param percent The significance level (default 0.05)
#'
#' @return 1 if the two series are cointegrated; 0 otherwise
#' @export
#'
#' @examples test_cointegration(vec1, vec2, percent=0.01)
test_cointegration = function(vec1, vec2, percent=0.05) {
  # Check if both series are I(1)
  result1 = aTSA::adf.test(diff(vec1), output=TRUE)
  result2 = aTSA::adf.test(diff(vec2), output=TRUE)

  if (c(result1$type1[1,3] < percent) +     # Checks that all lag=1 reject the null
      c(result1$type2[1,3] < percent) +
      c(result1$type3[1,3] < percent) <= 1) {
    return(0)
  }
  else if (c(result2$type1[1,3] < percent) +
           c(result2$type2[1,3] < percent) +
           c(result2$type3[1,3] < percent) <= 1) {
    return(0)
  }

  # Check if the residual is stationary
  mod = stats::lm(vec1 ~ vec2)
  result3 = aTSA::adf.test(mod$res, output=TRUE)
  return( c(result3$type1[1,3] < percent) +
            c(result3$type2[1,3] < percent) +
            c(result3$type3[1,3] < percent) > 1 )
}
