#' Missing count, proportion variables table
#'
#' Create a table that checks the n, proportion of missing values of variables
#' @param data A data for the table
#' @param include Variables to check the number of missing values. if NA, all variables in data are included.
#' @importFrom data.table as.data.table .SD melt dcast setorderv
#' @importFrom gt gt tab_header
#' @export
varMissTable = function(data, include=NULL){
  if(length(include) > 0){
    x_n = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x))),.SDcols = include]
    x_rate = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x))/nrow(data)*100),.SDcols=include]
  } else {
    x_n = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x)))]
    x_rate = as.data.table(data)[,lapply(.SD, \(x) round(sum(is.na(x))/nrow(data)*100,2))]
  }
  df = rbind(x_n, x_rate)
  df[['name']] = rep(c('N','rate(%)'), length(include))
  x_melt = melt(df, id.vars='name')
  x_dcast = dcast(x_melt, variable ~ name, value.var = 'value')
  setorderv(x_dcast, "N", -1)
  tbl = x_dcast  |>
    gt()  |>
    tab_header(
      title='Missing values of variables')

  return(tbl)
}
