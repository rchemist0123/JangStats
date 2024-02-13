#' Multivariable Logistic regression table
#'
#' Perform a multivariable logistic regression for numerous variables and create a table
#' @param data A data for analysis
#' @param y Name of a dependent variable or an outcome variable
#' @param vars Names of independent variables or response variables
#' @param digits Digits of result values. Default as 2.
#' @param p.digits Digits of p-value. Default as 4.
#' @return A table of multivariable regression about independent variables included in this analysis
#' @importFrom stats as.formula binomial confint.default glm coef
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md
#' @examples
#' # example code
#' lrMultTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#' @export
lrMultTable = function(data, y, vars, digits=2, p.digits=4) {
  if (y %in% vars) {
    cat("The outcome variable",y, "included in independent variables.\n",
        y,"was excluded.\n")
    vars = setdiff(vars,y)
  }
  form = paste0(y, "~", paste0(vars, collapse="+")) |> as.formula()
  fit = glm(form, family=binomial(), data = data)
  est = exp(coef(fit))[-1]
  ci = exp(confint.default(fit)[2,])
  p = coef(summary(fit))[-1,4]

  result = data.frame(
    variable = names(coef(fit))[-1],
    OR_ci = paste0(format(round(est,digits),nsmall=digits), ' (',
                   format(round(ci[1],digits),nsmall=digits),'-',
                   format(round(ci[2],2),nsmall=digits),")"),
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
      variable = md("**Variable**"),
      OR_ci = md("**OR (95% CI)**"),
      p = md("**P value**")
    )
  return(table)
}
