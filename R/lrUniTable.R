#' Univariable Logistic regression table
#'
#' Perform univariable logistic regressions for numerous variables and create a table
#' @param data A data for analysis
#' @param outcome Name of a dependent variable or an outcome variable.
#' @param vars Names of independent variables or response variables.
#' @param digits Digits of result values. Default as 2.
#' @param p.digits Digits of p-value. Default as 4.
#' @return A table of univariable regression about independent variables included in this analysis
#' @importFrom stats as.formula binomial coef confint.default glm
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md
#' @examples
#' # example code
#' lrUniTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#' @export
lrUniTable = function(data, outcome, vars, digits=2, p.digits=4) {
  result = lapply(vars,
         \(x){
           form = springf("%s ~ %s", outcome, x) |> as.formula()
           fit = glm(form, family=binomial(), data = data)
           coef = exp(coef(fit))[-1]
           confint = exp(confint.default(fit)[2,])
           p = coef(summary(fit))[-1,4]
           data.frame(
             variable = names(coef(fit))[-1],
             OR_ci = paste0(format(round(coef, digits),nsmall=digits), ' (',
                            format(round(confint[1],digits),nsmall=digits),'-',
                            format(round(confint[2],digits),nsmall=digits),")"),
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
        OR_ci = md("**OR (95% CI)**"),
        p = md("**P value**")
      )
  return(table)
}
