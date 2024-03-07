#' Univariable logistic or Cox regression table
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
#' @importFrom gt gt tab_style cell_fill cells_body cols_label cols_label_with md
#' @examples
#' #example code
#' uniTable(mtcars, 'am', c('mpg','cyl','disp','hp','wt'))
#'
#' @export
uniTable = function(data, outcome, vars, time=NULL, digits=2, p.digits=3){
  fits = uniReg(data, outcome, vars, time)
  if(is.null(time)) est_name = "OR (95% CI)" else "HR (95% CI)"
  result = lapply(fits, \(x){
    if(is.null(time)){
      coef = exp(coef(x))[-1]
      confint = exp(confint.default(x)[2,])
      p = coef(summary(x))[-1,4]
      data.frame(
        variable = names(coef(x))[-1],
        est_ci = paste0(format(round(coef, digits),nsmall=digits), ' (',
                       format(round(confint[1],digits),nsmall=digits),'-',
                       format(round(confint[2],digits),nsmall=digits),")"),
        p = format(round(p,p.digits),nsmall=p.digits)
      )
    } else {
      coef = exp(coef(x))
      confint = exp(confint.default(x))
      p = coef(summary(x))[5]
      est_name <<- "HR (95% CI)"
      data.frame(
        variable = x |> coef() |> names(),
        est_ci = paste0(format(round(coef,digits),nsmall=digits), ' (',
                       format(round(confint[1],digits),nsmall=digits),'-',
                       format(round(confint[2],2),nsmall=digits),")"),
        p = format(round(p,p.digits),nsmall=p.digits)
      )
    }
  })
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
      variable = "**Variable**",
      est_ci = est_name,
      p = "**P value**",
      .fn = md
    ) |> cols_label_with(
      columns = contains("CI"),
      fn = \(x) {
        gsub("^", "**", gsub("$", "**", x)) |> md()
      }
    )
  return(table)
}
