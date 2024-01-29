#' Missing count, proportion variables table
#'
#' Create a table that checks the n, proportion of missing values of variables
#' @param data A data for the table
#' @param include Variables to check the number of missing values
#' @importFrom data.table .SD melt dcast setorderv
#' @importFrom gt gt tab_header
#' @export
varMissTable = function(data, include){
  x_n = data[,lapply(.SD, \(x) sum(is.na(x))),.SDcols=include]
  x_rate = data[,lapply(.SD, \(x) sum(is.na(x))/nrow(data)*100),.SDcols=include]
  data = rbind(x_n, x_rate)
  data[['name']] = rep(c('missing_n','missing_rate'), length(include))
  x_melt = melt(data, id.vars='name')
  x_dcast = dcast(x_melt, variable ~ name, value.var = 'value')
  setorderv(x_dcast, -"missing_n")
  tbl = x_dcast  |>
    gt()  |>
    tab_header(
      title='Missing values of Matching variables')

  return(tbl)
}
