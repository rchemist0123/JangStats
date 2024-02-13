#' Combine univariable & multivariable Cox regression table
#'
#' Perform univariable Cox regressions for numerous variables and create a table
#' @param uni_tbl A univariable Cox regression table
#' @param mult_tbl A multivariable Cox regression table
#' @return A table of merged table of Cox regressions
#' @importFrom gt gt tab_style cell_fill cells_body cols_align cols_label md ends_with tab_spanner data_color
#' @importFrom data.table merge.data.table setDT
#' @export

coxTableCombine = function(uni_tbl, mult_tbl){
  df = merge.data.table(uni_tbl[["_data"]], mult_tbl[["_data"]], by="variable", all.x=T, sort=F) |> setDT()
  df[is.na(df[["HR_ci.y"]]), 4:5 := "—"]
  tbl = df |>
    gt() |>
    cols_align(
      align = 'center',
      columns = !"variable"
    ) |>
    cols_label(
      variable = md("**Variable**"),
      HR_ci.x = md("**HR (95% CI)**"),
      p.x = md("**P value**"),
      HR_ci.y = md("**HR (95% CI)**"),
      p.y = md("**P value**")
    ) |>
    data_color(
      columns = ends_with(".x"),
      rows = df$p.x < 0.2,
      palette = c("lightyellow")
    ) |>
    data_color(
      columns = ends_with(".y"),
      rows = df$p.y != "—" & df$p.y < 0.05,
      palette = c("lightyellow")
    ) |>
    tab_spanner(label=md("**Univariable**"),
                columns = ends_with(".x")) |>
    tab_spanner(label=md("**Multivariable**"),
                columns = ends_with(".y")
    ) |>
    tab_header(title=md('**Tables of Cox regressions**'))
  return(tbl)
}
