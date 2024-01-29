#' Univariable Logistic regression
#'
#' Perform a univariable & multivariable logistic regression
#' @param data data for analysis
#' @param y Name of a Outcome variable
#' @param covars Covariates including in analysis
#' @return A Table of Univariable regression
#' @examples
#' # example code
#' lrUniReg(mtcars, am, c('mpg','cyl','disp','hp','drat','wt'))
#' @export
lrUniReg = function(data, y, covars) {
  result = lapply(covars,
         \(x){
           form = paste0(y, "~" ,x) |> as.formula()
           fit = glm(form, family=binomial(), data = data)
           coef = exp(coef(fit))[-1]
           confint = exp(confint.default(fit)[2,])
           p = coef(summary(fit))[-1,4]
           aster = ifelse(p<0.001, "***", ifelse(p<0.01,'**', ifelse(p<0.05,'*',"")))
           data.frame(
             variable = x,
             OR_ci = paste0(format(round(coef,2),nsmall=2), ' (',
                            format(round(confint[1],2),nsmall=2),'--',
                            format(round(confint[2],2),nsmall=2),")"),
             p = paste(format(round(p,4),nsmall=4), aster)
           )
         }
  )

  return(result)
}
