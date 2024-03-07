#' Fit a multivariable logistic regression or Cox regression
#'
#' Perform a multivariable logistic regression or Cox regression
#' @param data A data for analysis.
#' @param outcome Name of a dependent variable or an outcome variable.
#' @param vars Names of independent variables or response variables.
#' @param time Observation duration variable. If not NULL, cox regression. Default is NULL.
#' @return A glm or coxph object
#' @importFrom stats as.formula glm binomial
#' @importFrom survival coxph
#' @export
multReg = function(data, outcome, vars, time=NULL){
  if(is.null(time)){
    form = sprintf("%s ~ %s", outcome, paste0(vars, collapse=" + "))
    fit = glm(as.formula(form), family=binomial(), data = data)
  } else {
    form = sprintf("survival::Surv(%s, %s==1)~%s", time, outcome, paste0(vars, collapse = " + "))
    fit = survival::coxph(as.formula(form), data = data)
  }
  return(fit)
}
