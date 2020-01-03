#' Mean deviation
#'
#' Function to calculate mean deviation
#' Output: A vector of size '1' containing "mean absolute deviation from mean"
#'
#' @param attr A vector of numeric values
#'
#' @return
#' @export
#'
#' @examples
#' MDEV()
MDEV <- function(attr) {
  tmpMean <- mean(attr)
  ret     <- mean(abs(attr - tmpMean))
  return(ret)
}







