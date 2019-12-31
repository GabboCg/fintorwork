#' Return cumulative
#'
#' @param x something
#' @param h something
#'
#' @return
#' @export
return_cumulative <- function (x, h) {

  r_h <- matrix(NaN, nrow = NROW(x), ncol = NROW(h))

  for(i in seq_along(h)){
    for(j in 1:(NROW(x) - (h[i] - 1))){

      r_h[j,i] <- mean(x[j:(j+h[i]-1)])

    }
  }

  return(r_h)

}

#' @examples
