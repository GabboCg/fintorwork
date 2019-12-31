#' In-sample and out-of-sample Goyal and Welch (2008)
#'
#' @import dplyr
#' @import magrittr
#' @importFrom stats lm na.omit coef pchisq
#'
#' @param df tibble or dataframe with date, dependent variable and independent variable (variable)
#' @param firstyear Size of the windows
#' @param cumsum Cumulative sum of  sum squared error (sse)
#'
#' @return
#' @export
is_oos_gw <- function(df, firstyear, cumsum = c('1', '0')) {

  cols_name <- c('yyyy', 'y', 'x')

  ds <- set_names(df, cols_name)

  is_xyresid <- lm(y ~ x, data = ds)
  is_meanresid <- ds$y - mean(ds$y)

  oos_xyresid <- rep(NA, nrow(ds))
  oos_meanresid <- rep(NA, nrow(ds))

  for (i in firstyear:nrow(ds)) {

    oos_meanpred <- mean(ds$y[1:i])
    oos_meanresid[i+1] <- ds$y[i+1] - oos_meanpred

    oos_lmcoef <- coef(lm(y ~ x, data = ds[1:i,]))
    oos_pred <- oos_lmcoef[1] + oos_lmcoef[2] * ds$x[i + 1]
    oos_xyresid[i+1] <- ds$y[i + 1] - oos_pred

  }

  plotwork <- tibble(date = c(ds$yyyy),
                     is_meanresid = is_meanresid,
                     is_xyresid = stats::residuals(is_xyresid),
                     oos_meanresid = oos_meanresid[1:nrow(ds)],
                     oos_xyresid = oos_xyresid[1:nrow(ds)])

  if (cumsum == '1') {

    plotwork[firstyear,] <- c(plotwork$date[firstyear - 1] + 1, 0, 0, 0, 0)

    plotwork %<>%
      na.omit() %>%
      mutate(is_improvement = cumsum(is_meanresid ^ 2) - cumsum(is_xyresid ^ 2),
             oos_improvement = cumsum(oos_meanresid ^ 2) - cumsum(oos_xyresid ^ 2))

    return(plotwork)

  } else {

    return(plotwork)

  }

}

#' @examples

