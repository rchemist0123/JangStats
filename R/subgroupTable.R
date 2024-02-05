#' Subgroup analysis table
#'
#' Perform a subgroup analysis and create a table
#' @param data A data for analysis
#' @param fit A logistic regression or Cox regression model
#' @param treatment A variable that
#' @param subgroups Categorical Variable(s) for the subgroup analysis
#' @param digits Digits of result values. Default as 2.
#' @return A table of Subgroup analysis with subgroups, level, case, control,  estimates, ci, and p interaction
#' @importFrom Publish subgroupAnalysis
#' @importFrom dplyr group_by
#' @importFrom gt gt tab_style cols_label cols_label md fmt_number cols_align cells_row_groups cell_text
#' @export
subAnalTable = function(data, fit, treatment, subgroups, digits=2){
  stopifnot("No subgroups are provided." = length(subgroups) == 0)
  tbl = subgroupAnalysis(fit,
                   data = data,
                   treatment = treatment,
                   subgroups = subgroups)
  tbl[['hr_ci']] = paste0(format(round(tbl[['HazardRatio']],digits), nsmall=digits),
                          ' (', format(round(tbl[['Lower']],digits), nsmall=digits), '-',
                          format(round(tbl[['Upper']],digits), nsmall=digits),
                          ')')
  tbl[['control']] = paste0(tbl[['event_0']],'/', tbl[['sample_0']])
  tbl[['case']] = paste0(tbl[['event_1']],'/',tbl[['sample_1']])

  tbl[,c('subgroups', 'level', 'case', 'control', 'hr_ci', 'pinteraction')] |>
    group_by(subgroups) |>
    gt()  |>
    cols_label(
      level = md("**Subgroup**"),
      case = md("**Case**"),
      control = md("**Control**"),
      hr_ci = md("**HR (95% CI)**"),
      pinteraction = md("***P* for interaction**")
    ) |>
    fmt_number(
      columns = ,
      decimals = 4
    ) |>
    tab_style(
      style = list(cell_text(weight="bold")),
      locations = cells_row_groups()
    )


}
