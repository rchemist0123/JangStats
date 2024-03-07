#' Print frequency and proportion of a vector
#'
#' Create a table that checks the n, proportion of a vector. Compatible with data.table.
#' @param x A vector to check frequency and proportion
#' @param digits A digit for proportions
#' @param missing Whether considering NA values. Default is FALSE.
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mtcars)[,nprop(cyl)]
#'
nprop = function(x, digits = 1, missing=FALSE){
  if(!missing) n = table(x)
  else n = table(x, useNA = "ifany")
  p = round(proportions(n)*100, digits) |> as.vector();
  nm = deparse(substitute(x))
  l = list()
  l[[nm]] = names(n);
  l[["N(%)"]] = sprintf("%s (%s)", as.vector(n), p)
  return(l)
}
