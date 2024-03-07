#' Multivariable logistic or Cox regression table
#'
#' Create a multivariable regression table
#' @param data A data for analysis
#' @param outcome Name of a dependent variable or an outcome variable
#' @param vars Names of independent variables or response variables
#' @param time Observation duration variable. If not NULL, cox regression. Default is NULL.
#' @param digits Digits of result values. Default as 2.
#' @param p.digits Digits of p-value. Default as 4.
#' @return A table of multivariable regression about independent variables included in this analysis
#' @importFrom stats as.formula binomial confint.default glm coef
#' @importFrom gt gt tab_style cell_fill cells_body cols_label cols_label_with md contains
#' @examples
#' #example code
#' multTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#' @export
multTable = function(data, outcome, vars, time=NULL, digits=2, p.digits=3){
  fit = multReg(data, outcome, vars, time)
  if(inherits(fit, "coxph")) {
    est = exp(coef(fit))
    ci = exp(confint.default(fit))
    p = coef(summary(fit))[,"Pr(>|z|)"]
    est_name = "HR (95% CI)"
    variable = names(coef(fit))
  }
  else if(inherits(fit,"glm")) {
    est = exp(coef(fit))[-1]
    ci = exp(confint.default(fit)[2,])
    p = coef(summary(fit))[-1,4]
    variable = names(coef(fit))[-1]
    est_name = "OR (95% CI)"
  }

  result = data.frame(
    variable = variable,
    est_ci = paste0(format(round(est,digits), nsmall=digits), ' (',
                   format(round(ci[1],digits), nsmall=digits),'-',
                   format(round(ci[2],2), nsmall=digits),")"),
    p = format(round(p, p.digits),nsmall=p.digits)
  )

  table = result |>
    gt() |>
    tab_style(
      style = list(
        cell_fill(color="lightyellow")
      ),
      locations = cells_body(
        rows = result$p < 0.05,
      )
    ) |> cols_label(
      variable = "**Variable**",
      est_ci = est_name,
      p = "**P value**",
      .fn= md
    ) |> cols_label_with(
      columns = contains("CI"),
      fn = \(x) {
        gsub("^", "**", gsub("$", "**", x)) |> md()
        }
    )
  return(table)

}
