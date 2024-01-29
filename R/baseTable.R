#' Baseline characteristics table
#'
#' Create a baseline characteristic table by group variable
#' @param data A data for analysis.
#' @param include Variables including in the table.
#' @param by A categorical variable for group
#' @param time_vars time related variables, which are shown as a median(IQR)
#' @return A baseline chararcteristics table with p-value
#' @importFrom gtsummary tbl_summary add_p add_overall all_continuous all_categorical modify_header style_pvalue
#' @importFrom dplyr all_of
#' @examples
#' # example code
#' baseTable(mtcars,
#'     include = c("mpg", "am","hp","drat","wt","gear","qsec", "disp","cyl"),
#'     by="cyl")
#' @export
#'
baseTable = function(data, include, by = NULL, time_vars=NULL) {
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
    add_overall() |>
    modify_header(label = "**Variable**", p.value = "**P**")

  if(!is.null(by)){
    tbl = tbl |>
      add_p(pvalue_fun = ~style_pvalue(., digits=3)
            )
  }
  return(tbl)
}
