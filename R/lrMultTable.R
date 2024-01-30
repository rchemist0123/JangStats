#' Multivariable Logistic regression table
#'
#' Perform a multivariable logistic regression for numerous variables and create a table
#' @param data A data for analysis
#' @param y Name of a dependent variable or an outcome variable
#' @param Xs Names of independent variables or response variables
#' @return A table of multivaraible regression about independent variables included in this analysis
#' @importFrom stats as.formula binomial confint.default glm
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md
#' @examples
#' # example code
#' lrMultTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#' @export
lrMultTable = function(data, y, Xs) {
  if (y %in% Xs) {
    cat("The outcome variable",y, "included in independent variables.\n",
        y,"was excluded.\n")
    Xs = setdiff(Xs,y)
  }
  form = paste0(y, "~", paste0(Xs, collapse="+")) |> as.formula()
  fit = glm(form, family=binomial(), data = data)
  coef = exp(coef(fit))[-1]
  confint = exp(confint.default(fit)[2,])
  p = coef(summary(fit))[-1,4]

  result = data.frame(
    variable = Xs,
    OR_ci = paste0(format(round(coef,2),nsmall=2), ' (',
                   format(round(confint[1],2),nsmall=2),'-',
                   format(round(confint[2],2),nsmall=2),")"),
    p = format(round(p, 4),nsmall=4)
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
