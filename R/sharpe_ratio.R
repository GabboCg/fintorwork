#' Title
#'
#' @param return something
#' @param h something
#'
#' @return
#' @export
#'
#' @examples
sharpe_ratio <- function(return, h) {

  sr <- sqrt((12 / h)) * mean(return) / sd(return)

  return(sr)

}
