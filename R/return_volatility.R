#' Return volatility
#'
#' @param x1 something
#' @param x2 something
#'
#' @return
#' @export
return_volatility <- function (x1, x2) {

  RVOL_mat <- matrix(NaN, nrow = NROW(x1) - 11, ncol = 1)

  for(i in seq_along(RVOL_mat)){

    RVOL_mat[i,] <- mean(abs(x1[i:(i+11)] - x2[i:(i+11)]))

  }

  RVOL_mat <- sqrt(pi/2)*sqrt(12)*RVOL_mat

  return(RVOL_mat)

}

#' @examples
