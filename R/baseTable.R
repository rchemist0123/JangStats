#' Baseline characteristics table
#'
#' Create a baseline characteristic table by group variable
#' @param data A data for analysis.
#' @param by A categorical variable for group.
#' @param time_vars time related variables, which are shown as a median(IQR)
#' @param include Variables including in the table. All columns in the data as default.
#' @return A baseline characteristics table with p-value
#' @importFrom gtsummary tbl_summary add_p add_overall all_continuous all_categorical modify_header style_pvalue
#' @importFrom gt everything
#' @importFrom dplyr all_of
#' @examples
#' # example code
#' baseTable(mtcars,
#'     include = c("mpg", "am","hp","drat","wt","gear","qsec", "disp","cyl"),
#'     by="cyl")
#' @export
#'
baseTable = function(data, by = NULL, time_vars=NULL, include=everything()) {
  tbl = tbl_summary(
    data = data,
    by = by,
    include = include,
    statistic = list(
      all_continuous() ~ "{mean} \u00b1 {sd}",
      all_categorical() ~ "{n} ({p})",
      all_of(time_vars) ~ "{median} ({p25}-{p75})"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0,1)
    ),
    missing = "no"
  ) |>
    modify_header(label = "**Variable**")

  if(!is.null(by)){
    tbl = tbl |>
      add_overall() |>
      add_p(pvalue_fun = ~style_pvalue(., digits=3)) |>
      modify_header(p.value = "**P**")
  }
  return(tbl)
}
