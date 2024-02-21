#' Print frequency and proportion of a vector
#'
#' Create a table that checks the n, proportion of a vector. Compatible with data.table.
#' @param x A vector that check frequency and proportion
#' @export
nprop = function(x, na.rm=FALSE){
  if(isTRUE(na.rm)) n = table(x)
  else n = table(x, useNA = "ifany")
  p = round(proportions(n)*100,2) |> as.vector();
  nm = deparse(substitute(x))
  l = list()
  l[[nm]] = names(n);
  l[["N"]] = as.vector(n);
  l[["Prop"]] = p;
  return(l)
}
