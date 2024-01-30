#' Univariable Logistic regression
#'
#' Perform univariable logistic regressions for numerous variables and create a table
#' @param data A data for analysis
#' @param y Name of a dependent variable or an outcome variable
#' @param Xs Names of independent variables or response variables
#' @return A table of univariable regression about independent variables included in this analysis
#' @importFrom stats as.formula binomial confint.default glm
#' @importFrom gt gt tab_style cell_fill cells_body
#' @importFrom DescTools StrAlign
#' @examples
#' # example code
#' lrUniTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#' @export
lrUniTable = function(data, y, Xs) {
  result = lapply(Xs,
         \(x){
           form = paste0(y, "~" ,x) |> as.formula()
           fit = glm(form, family=binomial(), data = data)
           coef = exp(coef(fit))[-1]
           confint = exp(confint.default(fit)[2,])
           p = coef(summary(fit))[-1,4]
           data.frame(
             variable = x,
             OR_ci = paste0(format(round(coef,2),nsmall=2), ' (',
                            format(round(confint[1],2),nsmall=2),'-',
                            format(round(confint[2],2),nsmall=2),")"),
             p = format(round(p,4),nsmall=4)
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
