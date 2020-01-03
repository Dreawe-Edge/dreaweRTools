#' Tukey range for outlier
#'
#' Function to give outlier ranges based on Tukey's method
#' Output: A vector of size '2' containing outlier boundaries
#'
#' @param attr A vector of numeric values
#'
#' @return
#' @export
#'
#' @examples
#' RANGE_OUTLIER()
#'
RANGE_OUTLIER <- 	function(attr){
  tmpQuart1 <- as.vector(quantile(attr,0.25))
  tmpQuart3 <- as.vector(quantile(attr,0.75))
  tmpDiff   <- tmpQuart3 - tmpQuart1
  ret       <- c((tmpQuart1 - 1.5 * tmpDiff - 1e-09),(tmpQuart3 + 1.5 * tmpDiff + 1e-09))
  return(ret)
}
