#' Wrapper for regression data
#'
#' @param x Independent variables
#' @param y Dependent variable
#' @param data data.table to analyze
#' @param controls Factors to be projected out
#' @param cluster Variables to cluster on
#'
#' @return Regression object
#'
runReg <- function(x, y, data, controls, cluster, weights = NULL) {
  eq <- formula(paste0(y, " ~ ", x, "| ", controls, "| 0 |", cluster))
  if (!is.null(weights)) {
    output <- felm(eq, data = data, weights = weights)
  } else {
    output <- felm(eq, data = data)
  }
  return(output)
}
