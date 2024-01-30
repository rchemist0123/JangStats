#' Combine univariable & multivariable Logistic regression table
#'
#' Perform univariable logistic regressions for numerous variables and create a table
#' @param uni_tbl A data of univariable logistic regression table
#' @param mult_tbl A data of multivariable logistic regression table
#' @return A table of merged table of logistic regressions
#' @importFrom gt gt tab_style cell_fill cells_body cols_label md ends_with tab_spanner
#' @export

lrTableCombine = function(uni_tbl, mult_tbl){
  df = merge(uni_tbl, mult_tbl, by="variable")
  tbl = df |>
    gt() |>
    cols_label(
      variable = md("**Variable**"),
      OR_ci.x = md("**OR (95% CI)**"),
      p.x = md("**P value**"),
      OR_ci.y = md("**OR (95% CI)**"),
      p.y = md("**P value**")
    ) |>
    tab_style(
      style = list(
        cell_fill(color="lightyellow")
      ),
      locations = cells_body(
        rows = df$p.x < 0.05,
      )
    ) |>
    tab_style(
      style = list(
        cell_fill(color="lightyellow")
      ),
      locations = cells_body(
        rows = df$p.y < 0.05,
      )
    ) |>
    tab_spanner(label=md("**Univariable**"),
                columns = ends_with(".x")) |>
    tab_spanner(label=md("**Multivariable**"),
                columns = ends_with(".y")
    )
  return(tbl)
}
