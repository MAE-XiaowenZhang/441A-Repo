#' Lagrange-Form Interpolation Polynomial Estimation
#' @description Use the Lagrange form to estimate the interpolation polynomial for the given data
#' points. This function then returns the y-value for a user-inputted point of query
#'
#' @param m Number of data points given
#' @param x Vector that contains the x-coordinates of the data points
#' @param y Vector that contains the y-coordinates of the data points
#' @param z Point of query whose y-value is the interest
#'
#' @return The y-value of the point of query, estimated by the interpolation polynomial
#' @export
#'
#' @examples lagrange(m, x, y, z)
lagrange = function(m, x, y, z) {
  value = 0

  for (i in 1:m) {
    product = 1

    for (j in 1:m) {
      if (i != j) {
        product = product * (z-x[j]) / (x[i]-x[j])
      }
    }

    value = value + y[i]*product
  }
  return(value)
}


