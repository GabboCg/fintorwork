#' Title
#'
#' @param w_UB something
#' @param w_LB something
#' @param h something
#' @param ER_h something
#' @param R_f_h something
#' @param RRA something
#' @param start something
#' @param end something
#' @param k something
#' @param X something
#'
#' @return
#' @export
#'
#' @examples
oos_portfolio <- function(start, end, ER_h, R_f_h, X, w_UB, w_LB, RRA, h, k) {

  N <- NROW(X)
  R <- (end - start)*12 + k # in-sample period
  P <- N - R # out-of-sample period

  FC_PM <- matrix(NaN, nrow = P, ncol = NROW(h))
  w_PM <- matrix(NaN, nrow = P, ncol = NROW(h))
  R_PM <- matrix(NaN, nrow = P, ncol = NROW(h))
  ER_PM <- matrix(NaN, nrow = P, ncol = NROW(h))

  FC_PR <- matrix(NaN, nrow = P, ncol = NROW(h))
  w_PR <- matrix(NaN, nrow = P, ncol = NROW(h))
  R_PR <- matrix(NaN, nrow = P, ncol = NROW(h))
  ER_PR <- matrix(NaN, nrow = P,ncol = NROW(h))

  R_BH <- matrix(NaN, nrow = P, ncol = NROW(h))
  ER_BH <- matrix(NaN, nrow = P, ncol = NROW(h))
  FC_vol <- matrix(NaN, nrow = P, ncol = NROW(h))

  window <- 12*10

  for(p in 1:P) {
    print(p)
    for(j in 1:NROW(h)) {

      # volatility
      if(R+p-h <= window-1){

        FC_vol[p,j] <- sd(ER_h[1:(R+p-h)])

      }else{

        FC_vol[p,j] <- sd(ER_h[((R+p-h)-(window-1)):(R+p-h)])

      }

      # Prevailing mean benchmark
      FC_PM[p,j] <- mean(ER_h[1:(R+p-h)])

      # Predictive regressions
      for(i in 1:(NCOL(X))) {

          X_i_j_p <- cbind(matrix(1, nrow = (R+(p-1)-h), ncol = 1),
                           X[1:(R+(p-1)-h)])
          results_i_j_p <- lm(ER_h[2:(R+p-h)] ~ X_i_j_p[,2])
          FC_PR[p,i]= cbind(1, X[R+(p-1)])%*%results_i_j_p$coefficients

      }
    }
  }


  for(t in 1:(P/h)){

    FC_vol_t <- FC_vol[((t-1)*h+1)]
    FC_PM_t <- FC_PM[((t-1)*h+1)]

    w_PM_t <- (1/RRA)*FC_PM_t/(FC_vol_t)^2

    ifelse(w_PM_t > w_UB,
           w_PM[((t-1)*h+1)] <- w_UB,
           ifelse(w_PM_t < w_LB,
                  w_PM[((t-1)*h+1)] <- w_LB,
                  w_PM[((t-1)*h+1)] <- w_PM_t))

    R_PM[((t-1)*h+1)] <- R_f_h[(R+(t-1)*h+1)] + w_PM[((t-1)*h+1)]*ER_h[(R+(t-1)*h+1)]
    ER_PM[((t-1)*h+1)] <- R_PM[((t-1)*h+1)] - R_f_h[(R+(t-1)*h+1)]

    for(i in 1:NCOL(FC_PR)){

      FC_PR_i_t <- FC_PR[((t-1)*h+1)]
      w_PR_i_t <- (1/RRA)*FC_PR_i_t/(FC_vol_t)^2

      ifelse(w_PR_i_t > w_UB,
             w_PR[((t-1)*h+1),i] <- w_UB,
             ifelse(w_PR_i_t < w_LB,
                    w_PR[((t-1)*h+1),i] <- w_LB,
                    w_PR[((t-1)*h+1),i] <- w_PR_i_t))

      R_PR[((t-1)*h+1),i] <- R_f_h[(R+(t-1)*h+1)] + w_PR[((t-1)*h+1),i]*ER_h[(R+(t-1)*h+1)]
      ER_PR[((t-1)*h+1),i] <- R_PR[((t-1)*h+1)] - R_f_h[(R+(t-1)*h+1)]

    }

    R_BH[((t-1)*h+1)] <- R_f_h[(R+(t-1)*h+1)] + ER_h[(R+(t-1)*h+1)]
    ER_BH[((t-1)*h+1)] <- ER_h[(R+(t-1)*h+1)]

  }

  bench_cer <- fintorwork::cer(na.omit(R_PM), RRA, h)
  predict_cer <- fintorwork::cer(na.omit(R_PR), RRA, h)
  buy_and_hold_cer <- fintorwork::cer(na.omit(R_BH), RRA, h)

  cer_metrics <- cbind(bench_cer, predict_cer, predict_cer - bench_cer, buy_and_hold_cer - bench_cer)
  colnames(cer_metrics) <- c('Benchmark', 'Forecast', 'CER gains', 'Buy and Hold')

  bench_sr <- fintorwork::sharpe_ratio(na.omit(ER_PM), h)
  predict_sr <- fintorwork::sharpe_ratio(na.omit(ER_PR), h)
  buy_and_hold_sr <- fintorwork::sharpe_ratio(na.omit(ER_BH), h)

  sr_metrics <- cbind(bench_sr, predict_sr, buy_and_hold_sr)
  colnames(sr_metrics) <- c('Mean', 'Forecast', 'Buy and Hold')

  result_list <- list('portfolios' = cbind(R_PM, ER_PM, R_PR, ER_PR, R_BH, ER_BH),
                      'CER' = cer_metrics,
                      'Sharpe Ratio' = sr_metrics)

  return(result_list)

}
