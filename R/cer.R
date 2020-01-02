#' Annualized Certainty Equivalent Return
#'
#' @importFrom stats sd
#'
#' @param return something
#' @param RRA something
#' @param h something
#'
#' @return
#' @export
#'
#' @examples
cer <- function(return, RRA, h){

  avg_utility <- (12 / h) * (mean(return) - 0.5 * RRA * (sd(return)) ^ 2)

  return(avg_utility)

}

