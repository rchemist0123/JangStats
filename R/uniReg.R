#' Fit a univariable logistic regression or Cox regression
#'
#' Perform a univariable logistic regression or Cox regression
#' @param data A data for analysis.
#' @param outcome Name of a dependent variable or an outcome variable.
#' @param vars Names of independent variables or response variables.
#' @param time Observation duration variable. If not NULL, cox regression. Default is NULL
#' @return A list of glm or coxph object.
#' @importFrom stats as.formula glm binomial
#' @importFrom survival coxph
#' @export
uniReg = function(data, outcome, vars, time = NULL) {
  result = lapply(vars, \(x){
    if(is.null(time)){
      form = sprintf("%s ~ %s", outcome, x) |> as.formula()
      fit = glm(form, family = binomial(), data = data)
    } else {
      form = sprintf("Surv(%s, %s == 1) ~ %s", time, outcome, x) |> as.formula()
      fit = coxph(form, data = data)
    }
  }
  )
  return(result)
}
