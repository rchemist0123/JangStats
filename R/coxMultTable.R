#' Mutlivariable Cox regression table
#'
#' Perform mutlivariable Cox regressions for numerous variables and create a table
#' @param data A data for analysis
#' @param y Name of a dependent variable or an outcome variable. Should be comprised of 0 and 1.
#' @param time A time variable, that is observation duration for the outcome in a survival model.
#' @param vars Names of independent variables or response variables.
#' @param digits Digits of result values. Default as 2.
#' @param p.digits Digits of p-value. Default as 4.
#' @return A table of mutlivariable Cox regression about independent variables included in this analysis.
#' @importFrom stats as.formula confint.default
#' @importFrom survival coxph Surv
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md
#' @export
coxMultTable = function(data, y, time,  vars, digits, p.digits) {
  if (y %in% vars) {
    cat("The outcome variable",y, "included in independent variables.\n",
        y,"was excluded.\n")
    vars = setdiff(vars,y)
  }
  form = paste0("Surv(",time, ",", y,'==1)~' , vars) |> as.formula()
  fit = coxph(form, data = data)
  coef = exp(coef(fit))[-1]
  confint = exp(confint.default(fit)[2,])
  p = coef(summary(fit))[-1,4]

  result = data.frame(
    variable = vars,
    HR_ci = paste0(format(round(coef,digits),nsmall=digits), ' (',
                   format(round(confint[1],digits),nsmall=digits),'-',
                   format(round(confint[2],digits),nsmall=digits),")"),
    p = format(round(p,p.digits),nsmall=p.digits)
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
      HR_ci = md("**HR (95% CI)**"),
      p = md("**P value**")
    )
  return(table)
}
