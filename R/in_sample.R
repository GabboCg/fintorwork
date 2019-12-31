#' In-Sample
#'
#' @import dplyr
#' @import sandwich
#' @import lmtest
#' @import magrittr
#' @importFrom stats lm na.omit coef
#'
#' @param y something
#' @param X something
#' @param h something
#'
#' @return
#' @export
in_sample <- function(y, X, h) {

  X_i_j <- as.matrix(X[1:(NROW(X) - h)])
  Y_i_j <- y[2:(NROW(y) - (h-1))]

  results_i_j_lm <- lm(Y_i_j ~ X_i_j)
  results_i_j_lm_coef <- coeftest(results_i_j_lm,
                                  vcov. = NeweyWest(results_i_j_lm,
                                                    lag = h,
                                                    adjust=FALSE,
                                                    verbose = TRUE,
                                                    prewhite = FALSE))

  beta_hat <- matrix(NaN, nrow = 1, ncol = 4)

  beta_hat[1, ] <- cbind(results_i_j_lm_coef[2,1],
                         results_i_j_lm_coef[2,3],
                         NaN,
                         summary(results_i_j_lm)$r.squared*100)

  colnames(beta_hat) <- c('Estimate', 't value' , 'Pr(>|t|)' ,'R-squared')

  return(beta_hat)

}

#' @examples
