#' Subgroup analysis table
#'
#' Perform a subgroup analysis and create a table
#' @param data A data for analysis
#' @param fit A logistic regression or Cox regression model
#' @param treatment A variable that could divide case & control group.
#' @param subgroups Categorical Variable(s) for the subgroup analysis.
#' @param digits Digits of result values. Default as 2.
#' @return A table of Subgroup analysis with subgroups, level, case, control,  estimates, ci, and p interaction
#' @importFrom Publish subgroupAnalysis
#' @importFrom gt gt tab_style cols_label cols_label md fmt_number cols_align cells_row_groups cell_text
#' @export
subgroupTable = function(data, fit, treatment, subgroups, digits=2){
  if(inherits(fit, "coxph")) {
    est = "hr_ci"
    tbl_col = "HazardRatio"
    header = "**HR (95% CI)**"
  }
  else if(inherits(fit,"glm")) {
    est = "or_ci" ;
    tbl_col = "OddsRatio"
    header = "**OR (95% CI)**"
  }
  classes = sapply(data[,c(treatment, subgroups), with=F], class)
  if(!classes[treatment] == 'factor') data[[treatment]] = as.factor(data[[treatment]])
  for(s in subgroups){
     if(!classes[s] == 'factor') data[[s]] = as.factor(data[[s]])
  }
  stopifnot("No subgroups are provided." = length(subgroups) >0)
  tbl = subgroupAnalysis(fit,
                   data = data,
                   treatment = treatment,
                   subgroups = subgroups)
  tbl[[est]] = paste0(format(round(tbl[['HazardRatio']],digits), nsmall=digits),
                          ' (', format(round(tbl[['Lower']],digits), nsmall=digits), '-',
                          format(round(tbl[['Upper']],digits), nsmall=digits),
                          ')')
  tbl[['control']] = paste0(tbl[['event_0']],'/', tbl[['sample_0']])
  tbl[['case']] = paste0(tbl[['event_1']],'/',tbl[['sample_1']])

  tbl[,c('subgroups', 'level', 'case', 'control', est, 'pinteraction')] |>
    gt(groupname_col = "subgroups")  |>
    cols_label(
      level = md("**Subgroup**"),
      case = md("**Case**"),
      control = md("**Control**"),
      starts_with(est) ~ md(header),
      pinteraction = md("***P* for interaction**")
    ) |>
    fmt_number(
      columns = "pinteraction",
      decimals = 4
    ) |>
    tab_style(
      style = list(cell_text(weight="bold")),
      locations = cells_row_groups()
    )


}
