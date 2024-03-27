#' Format for continuous values as mean and sd
#'
#' Internal function to paste mean and sd of a continuous variable.
#' @param x The name of variables
#' @return Character values that combined mean and sd.
#' @keywords internal
.cont_mean_sd = function(x, digits) {
  return(sprintf("%s \u00b1 %s",
                 format(round(mean(x, na.rm=T), digits), nsmall=digits),
                 format(round(sd(x, na.rm=T), digits), nsmall=digits)
  ))
}

#' Format for continuous values as median and IQR
#'
#' Internal function to paste median and iqr of a continuous variable.
#' @param x The name of variables
#' @return Character values that combined median and IQR
#' @keywords internal
.cont_med_iqr = function(x, digits) {
  return(sprintf("%s [%s\u2014%s]",
                 format(round(median(x, na.rm=T), digits), nsmall=digits),
                 format(round(quantile(x, probs=.25, na.rm=T), digits), nsmall=digits),
                 format(round(quantile(x, probs=.75, na.rm=T), digits), nsmall=digits)
  ))
}
