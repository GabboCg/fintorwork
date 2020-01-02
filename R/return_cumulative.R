#' Return cumulative
#'
#' @param x something
#' @param rfree something
#' @param h something
#'
#' @return
#' @export
return_cumulative <- function(x, rfree, h) {

  r_h <- matrix(NaN, nrow = NROW(x), ncol = NROW(h))
  er_h <- matrix(NaN, nrow = NROW(x), ncol = NROW(h))
  r_f_h <- matrix(NaN, nrow = NROW(x), ncol = NROW(h))

  for(i in seq_along(h)){
    for(j in 1:(NROW(x) - (h[i] - 1))){

      r_h[j,i] <- mean(x[j:(j+h[i]-1)])

    }
  }

  for(i in seq_along(h)){
    for(j in 1:(NROW(x) - (h[i] - 1))){

      er_h[j,i] <- prod(1 + x[j:(j+h[i]-1)]) - prod(1 + rfree[j:(j+h[i]-1)])
      r_f_h[j,i] <- prod(1 + rfree[j:(j+h[i]-1)]) - 1

    }
  }

  result_list <- list(r_h = r_h, er_h = er_h, r_f_h = r_f_h)
  return(result_list)

}

#' @examples
