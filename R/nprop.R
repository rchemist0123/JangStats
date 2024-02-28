#' Print frequency and proportion of a vector
#'
#' Create a table that checks the n, proportion of a vector. Compatible with data.table.
#' @param x A vector that check frequency and proportion
#' @param digits A digit for proportions
#' @export
nprop = function(x, digits = 2, na.rm=FALSE){
  if(na.rm) n = table(x)
  else n = table(x, useNA = "ifany")
  p = round(proportions(n)*100, digits) |> as.vector();
  nm = deparse(substitute(x))
  l = list()
  l[[nm]] = names(n);
  l[["n"]] = as.vector(n);
  l[["prop"]] = p;
  return(l)
}
