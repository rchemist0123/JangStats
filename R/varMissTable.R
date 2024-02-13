#' Missing count, proportion variables table
#'
#' Create a table that checks the n, proportion of missing values of variables
#' @param data A data for the table
#' @param include Variables to check the number of missing values. if NA, all variables in data are included.
#' @importFrom data.table as.data.table .SD melt dcast setorderv
#' @importFrom gt gt tab_header
#' @examples
#' # example code
#' varMissTable(airquality)
#' @export
varMissTable = function(data, include=NULL) {
  stopifnot("The data must be one of data.frame, tbl_df, or data.table." =
              (class(data)[1] %in% c("data.frame","tbl_df","data.table")))
  if(length(include) > 0){
    x_n = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x))),.SDcols = include]
    x_rate = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x))/nrow(data)*100),.SDcols=include]
  } else {
    x_n = as.data.table(data)[,lapply(.SD, \(x) sum(is.na(x))), .SDcols = names(data)]
    x_rate = as.data.table(data)[,lapply(.SD, \(x) round(sum(is.na(x))/nrow(data)*100,2)), .SDcols = names(data)]
  }
  df = rbind(x_n, x_rate)
  df[['name']] = c('N','rate(%)')
  x_melt = melt(df, id.vars='name')
  x_dcast = dcast(x_melt, variable ~ name, value.var = 'value')
  setorderv(x_dcast, "N", -1)
  tbl = x_dcast  |>
    gt()  |>
    tab_header(title='Missing values of variables')

  return(tbl)
}
