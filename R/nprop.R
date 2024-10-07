#' Print frequency and proportion of a vector
#'
#' Create a table that checks the n, proportion of a vector. Compatible with data.table.
#' @param x A vector to check frequency and proportion
#' @param digits A digit for proportions
#' @param missing Whether considering NA values. Default is FALSE.
#' @param paste Whehter to paste freq and prop. Default is FALSE.
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mtcars)[,nProp(cyl)]
#'
nProp = function(x, digits = 1, missing=FALSE, paste = FALSE){
  if(!missing) n = table(x)
  else n = table(x, useNA = "ifany")
  p = round(proportions(n)*100, digits) |> as.vector();
  name = deparse(substitute(x))
  l = list()
  l[[name]] = names(n);
  if(paste){
    l[['N(%)']] = sprintf("%s (%s%%)", as.vector(n), format(p, nsmall=digits))
  } else {
    l[['N']] = as.vector(n);
    l[['P']] = p
  }
  return(l)
}
