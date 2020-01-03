#' Mean deviation distance
#'
#' Function to calculate mean deviation distances
#' Output: A vector containing mean deviation distances from each point in given vector
#'
#' @param attr A vector of numeric values
#'
#'
#' @return
#' @export
#'
#' @examples
#' MDEV_DISTANCE()
MDEV_DISTANCE <- function(attr){
  tmpMean <- mean(attr)
  tmpDev  <- MDEV(attr)
  ret     <- (attr - tmpMean)/tmpDev
  return(ret)
}

