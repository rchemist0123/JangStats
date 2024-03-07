#' Print number of distinct values of variable(s).
#'
#' Create a table of unique values of selected variables. Compatible with data.table.
#' @param x Vectors to check number of distinct values.
#' @export
#' @examples
#' as.data.table(mtcars)[,ndist(cyl)]
#' as.data.table(mtcars)[,ndist(.SD)]
#' as.data.table(mtcars)[,lapply(.SD, ndist)]
ndist = function(x){
  l = list()
  if( inherits(x, "data.frame") ){
    k = sapply(x, \(i){
      length(table(i))
    })
    l[["variable"]] = names(x)
    l[["ndist"]] = k
    return(l)
  } else {
    n = unique(x) |> length()
    return(n)
  }
}
