#' Univariable Cox regression table
#'
#' Perform univariable Cox regressions for numerous variables and create a table
#' @param data A data for analysis
#' @param outcome Name of a dependent variable or an outcome variable. Should be comprised of 0 and 1.
#' @param time A time variable, that is observation duration for the outcome in a survival model.
#' @param vars Names of independent variables or response variables.
#' @param digits Digits of result values. Default as 2.
#' @param p.digits Digits of p-value. Default as 4.
#' @return A table of univariable Cox regression about independent variables included in this analysis.
#' @importFrom stats as.formula confint.default coef
#' @importFrom survival coxph Surv
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md
#' @export
coxUniTable = function(data, outcome, time,  vars, digits=2, p.digits=4) {
  result = lapply(vars,
                  \(x){
                    form = sprintf("Surv(%s, %s == 1) ~ %s", time, outcome, x) |> as.formula()
                    fit = coxph(form, data = data)
                    coef = exp(coef(fit))
                    confint = exp(confint.default(fit))
                    p = coef(summary(fit))[5]
                    data.frame(
                      variable = fit |> coef() |> names(),
                      HR_ci = paste0(format(round(coef,digits),nsmall=digits), ' (',
                                     format(round(confint[1],digits),nsmall=digits),'-',
                                     format(round(confint[2],2),nsmall=digits),")"),
                      p = format(round(p,p.digits),nsmall=p.digits)
                    )
                  }
  )
  result = result |> do.call(what="rbind")
  table = result |>
    gt() |>
    tab_style(
      style = list(
        cell_fill(color="lightyellow")
      ),
      locations = cells_body(
        rows = result$p < 0.2
      )
    ) |> cols_label(
      variable = md("**Variable**"),
      HR_ci = md("**HR (95% CI)**"),
      p = md("**P value**")
    )
  return(table)
}
