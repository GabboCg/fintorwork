#' Out-of-Sample
#'
#' @import broom
#' @importFrom stats pnorm
#'
#' @param y something
#' @param X something
#' @param y_2 something
#' @param h something
#' @param start something
#' @param end something
#' @param space something
#' @param k something
#'
#' @return
#' @export
#'
#' @examples
out_of_sample <- function(y, X, y_2, h, start, end, space, k){

  ols <- function(y,X){

    betas <<- solve(t(X)%*%X)%*%(t(X)%*%y)

    e <<- y - X%*%betas
    variance <- t(e)%*%e
    vcv <- 1/(NROW(X)-NCOL(X))*(as.numeric(variance)*solve(t(X)%*%X))
    betastd <<- sqrt(diag(vcv))

    regr <<- cbind(betas, as.matrix(betastd))

  }

  y <- y

  T <- NROW(y)
  LhS <- y[2:(T - (h - 1)),]

  beta_full <- matrix(0, nrow = NCOL(y), ncol = 1)

  for(i in 1:NCOL(X)){

    RhS_i <- cbind(as.matrix(X[1:(T-h),i]), matrix(1, nrow = (T-h), ncol = 1))
    results_i <- ols(100*LhS, RhS_i)
    beta_full[i] <- results_i[1, 1]
    # print(i)

  }

  T <- NROW(y)
  N <- NCOL(X)
  R <- (end - start)*12 + k
  P_0 <-  ((end + space) - end)*12
  P <- T - (R + P_0)

  FC_hA <- matrix(0, nrow = (P_0 + P), ncol = 1)
  FC_ECON <- matrix(0, nrow = (P_0 + P), ncol = N)
  beta_ECON <- array(0, c((P_0 + P),N,2))
  FC_ECON_CT <- matrix(0, nrow = (P_0 + P), ncol = N)

  for(i in 1:(P_0+P)){

    FC_hA[i] <-  mean(y_2[1:(R+(i-1)),])

    X_t <- X[1:(R+(i-1)-h),]
    y_t <- y[2:(R+(i-h)),]

    for(j in 1:N){

      results_i_j <- lm(y_t ~ X_t) # X_t[,j]
      FC_ECON[i,j] <- cbind(1, X[R+(i-1),j])%*%results_i_j$coeff # betas de results_i_j

      beta_ECON[i,j,1] <- results_i_j$coeff[2]
      beta_ECON[i,j,2] <- as.numeric(broom::tidy(results_i_j)[2,3])

      ifelse(beta_full[j] > 0,
             ifelse(results_i_j$coeff[2] > 0,
                    FC_ECON_CT[i,j] <- FC_ECON[i,j],
                    FC_ECON_CT[i,j] <- FC_hA[i]),
             ifelse(beta_full[j] < 0,
                    ifelse(results_i_j$coeff[1] < 0,
                           FC_ECON_CT[i,j] <- FC_ECON[i,j],
                           FC_ECON_CT[i,j] <- FC_hA[i]), 0))

      if(FC_ECON_CT[i,j]<0){
        FC_ECON_CT[i,j] <-  0
      }
    }

  }

  beta_ECON_AUX <- beta_ECON[(P_0+1):NROW(beta_ECON),,]

  beta_ECON <- array(0,c(NROW(beta_ECON_AUX),1,NCOL(beta_ECON_AUX)))

  beta_ECON[,,1] <- beta_ECON_AUX[,1]
  beta_ECON[,,2] <- beta_ECON_AUX[,2]

  actual <- y[(R+P_0+1):(NROW(y)-(h-1)),]

  FC_hA <- FC_hA[(P_0+1):(NROW(FC_hA)-(h-1))]
  FC_ECON <- FC_ECON[(P_0+1):(NROW(FC_ECON)-(h-1))]
  FC_ECON_CT <- FC_ECON_CT[(P_0+1):(NROW(FC_ECON_CT)-(h-1))]

  e_hA <- actual - FC_hA
  e_ECON <- actual - FC_ECON
  e_ECON_CT <- actual - FC_ECON_CT

  CSFE_hA <- cumsum(e_hA^2)
  CSFE_ECON <- cumsum(e_ECON^2)
  CSFE_ECON_CT <- cumsum(e_ECON_CT^2)

  DCSFE_ECON <- CSFE_hA - CSFE_ECON
  DCSFE_ECON_CT <-  CSFE_hA - CSFE_ECON_CT

  R2OS_ECON <- matrix(0, nrow = NCOL(FC_ECON), 2)
  R2OS_ECON_CT <- matrix(0, nrow = NCOL(FC_ECON_CT), 2)

  for(i in 1:NROW(R2OS_ECON)){

    R2OS_ECON[i,1] <- 100*(1 - (sum(e_ECON^2/sum(e_hA^2, na.rm = TRUE),
                                    na.rm = TRUE)))
    f_i <- e_hA^2 - (e_ECON^2 - (FC_hA-FC_ECON)^2) # FC_ECON[,i]
    results_i <- lm(f_i ~ matrix(1, nrow = NROW(f_i), ncol = 1))
    tstat <- coef(summary(results_i))[, "t value"]
    R2OS_ECON[i,2] <- 1 - pnorm(tstat, 0, 1)

    R2OS_ECON_CT[i,1] <- 100*(1 - (sum(e_ECON_CT^2/sum(e_hA^2, na.rm = TRUE),
                                       na.rm = TRUE)))
    f_i <- e_hA^2 - (e_ECON_CT^2 - (FC_hA-FC_ECON_CT)^2) # FC_ECON_CT[,i]
    results_i <- lm(f_i ~ matrix(1, nrow = NROW(f_i), ncol = 1))
    tstat <- coef(summary(results_i))[, "t value"]
    R2OS_ECON_CT[i,2] <- 1 - pnorm(tstat, 0, 1)

  }

  R2OS_results <- cbind.data.frame(without = R2OS_ECON, with = R2OS_ECON_CT) %>%
    rename(without_R2OS = "without.1",
           without_p_value = "without.2",
           with_R2OS = "with.1",
           with_p_value = "with.2")

  return(R2OS_results)

}



