#' Baseline characteristics table
#'
#' Create a baseline characteristic table by group variable
#' @param data A data for analysis.
#' @param by A categorical variable for calculating by group. If NULL, all of the data would be aggregated.
#' @param include Variables including in the table. If NULL, all variables in the data would be included.
#' @param time.vars time related variables, which are shown as a median(IQR)
#' @param row.groups A group for clinical category. Must be a list with group names of categories and row numbers.
#' @param by.order The order of character variable using in `by`. Must be a vector.
#' @param digits Digits of result values. Default as 1.
#' @return A baseline characteristics table.
#' @importFrom data.table setDT copy setnames rbindlist set setcolorder transpose .N := data.table is.data.table
#' @importFrom stats fisher.test t.test chisq.test anova lm median quantile sd
#' @importFrom gt gt cols_align cols_label tab_style cell_text cells_column_labels tab_row_group row_group_order
#' @examples
#' # example code
#' baseTable(mtcars,
#'  by="cyl",
#'  include = c("mpg", "am","hp","drat","wt","gear","qsec", "disp")
#'  )
#' @export
baseTable = function(data, by = NULL, include = NULL, time.vars = NULL,
                row.groups = NULL, by.order = NULL,  digits = 1L){
  if(!is.data.table(data)) data = setDT(copy(data))
  else data = copy(data)
  if(is.null(include)) include = names(data)
  if(!is.null(by) && by %in% include) {
    include = setdiff(include, by)
    warning(gettextf("Group variable %s in include variables, excluded.", sQuote(by)))
  }

  if(!is.null(by) && !by %in% names(data)) stop(gettextf("%s not found in data.", sQuote(by)))
  if(!is.null(by) & is.null(by.order)) by.order = levels(factor(data[[by]]))
  # stopifnot("row.groups must be a list with key and values." = !is.is.list(row.groups))
  stopifnot("digits must be numeric class" = is.numeric(digits))
  n_tbl = data[, .N, by=by][['N']] # Number of category

  tbls = lapply(include, \(x){
    .x_level = length(unique(data[[x]]))

    # Categorical variables
    if(class(data[[x]]) %in% c("character","factor") |
       .x_level < 5 ) {
      # Total
      tab1 = data[,.N, keyby = x][, "n_prop" := paste0(.N," (", round(.N/sum(.N)*100,1),")")][,c(x,"n_prop"),with=F]
      colname_row = data.table(x,"") |> setnames(c('x','V2'),c(x,'n_prop'))
      cat_tbl = rbindlist(list(colname_row, tab1))

      cat_tbl_total = cat_tbl |>
        setnames(c(x,"n_prop"), c("variable", paste0("total",' (n=',nrow(data),')')))

      if(!is.null(by)){
        .by_level = length(unique(data[[by]]))
        data[[by]] = factor(data[[by]], levels = by.order)
        tab1 = data[,.N, keyby=c(by, x)][,"n_prop" := paste0(.N," (", round(.N/sum(.N)*100,1),")"), by=by][,c(by,x,"n_prop"),with=F]
        cat_tbl = dcast(tab1,
                     formula = paste0(c(by,x), collapse = "~"),
                     value.var="n_prop",
                     fill = "0 (0.0)") |>
          set(j = x, value="") |>
          setcolorder(x, after = 1) |>
          transpose(make.names = 1, keep.names = "variable")
        # set(j = "variable", value = x)setcolorder("variable", before=1)
        setnames(cat_tbl,
                 old = names(cat_tbl)[(ncol(cat_tbl)-.by_level+1):ncol(cat_tbl)],
                 new = paste0(names(cat_tbl)[(ncol(cat_tbl)-.by_level+1):ncol(cat_tbl)],
                              paste0(" (n=", n_tbl,")")))
        .cat_mat = data[,table(mget(c(x,by)))]
        if (length(.cat_mat[.cat_mat<5])>2) pval = data[,fisher.test(get(by), get(x))][['p.value']]
        else pval = .cat_mat |> chisq.test() |> _[['p.value']]
        pval_tbl = data.table(x = x,  "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval,3),nsmall=3))) |>
                                setnames('x',"variable")
        cat_tbl = merge(cat_tbl, pval_tbl, by="variable", all.x = T, sort=F)
        cat_tbl[is.na(cat_tbl)] = ""
        cat_tbl_final = merge(cat_tbl_total, cat_tbl, by="variable", sort=F)
      } else {
        cat_tbl_total
      }
    } else {
      # Continuous variables
      if(x %in% time.vars){
        cont_tbl_total = data[,lapply(.SD, \(x) paste0(
                            format(round(median(x, na.rm=T), digits), nsmall=digits), " [",
                            format(round(quantile(x, probs=.25, na.rm=T), digits), nsmall=digits), '-',
                            format(round(quantile(x, probs=.75, na.rm=T), digits), nsmall=digits), ']')), .SDcols=x]
      } else {
        cont_tbl_total = data[,lapply(.SD, \(x) paste0(
          format(round(mean(x, na.rm=T), digits), nsmall=digits), " \u00b1 ",
          format(round(sd(x, na.rm=T), digits), nsmall=digits))), .SDcols=x]
      }

      cont_tbl_total = cont_tbl_total |>
        set(j = "variable", value = x) |>
        setcolorder("variable",before = 1) |>
        setnames(old = x, new = paste0("total",' (n=',nrow(data),')'))

      if(!is.null(by)){
        .by_level = length(unique(data[[by]]))
        data[[by]] = factor(data[[by]], levels = by.order)
        if(x %in% time.vars){
          cont_tbl = data[,lapply(.SD, \(x) paste0(
            format(round(median(x, na.rm=T), digits), nsmall=digits), " [",
            format(round(quantile(x, probs=.25, na.rm=T), digits), nsmall=digits),'-',
            format(round(quantile(x, probs=.75, na.rm=T), digits), nsmall=digits ),']')), .SDcols=x, keyby = by]
        } else {
          cont_tbl = data[,lapply(.SD, \(x) paste0(
            format(round(mean(x, na.rm=T), digits), nsmall=digits), " \u00b1 ",
            format(round(sd(x, na.rm=T), digits), nsmall=digits))), .SDcols=x, keyby = by]
        }
        cont_tbl = cont_tbl |> transpose(make.names = 1, keep.names = "variable")
        setnames(cont_tbl,
                 old = names(cont_tbl)[(ncol(cont_tbl)-.by_level+1):ncol(cont_tbl)],
                 new = paste0(names(cont_tbl)[(ncol(cont_tbl)-.by_level+1):ncol(cont_tbl)],
                              paste0(" (n=", n_tbl,")"))
                 )

        .pval_form = paste0(x,"~",by) |> as.formula()
        if(.by_level >= 3) pval = anova(lm(.pval_form, data))[["Pr(>F)"]] |> _[1]
        else pval = t.test(.pval_form, data)[['p.value']]
        cont_tbl = cbind(cont_tbl, "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval,3), nsmall=3)))
        cont_tbl_final = merge(cont_tbl_total, cont_tbl, by="variable")
      } else {
        cont_tbl_total
      }
    }
  })
  result = tbls |>
    rbindlist(use.names = T) |>
    gt(rowname_col = T, rownames_to_stub = T) |>
    cols_align(align = "center", columns = !"variable") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

  if(!is.null(row.groups)){
    for(i in names(row.groups)){
      result = result |> tab_row_group(label = i, rows = row.groups[[i]])
    }
    result = result |>
      row_group_order(groups = c(names(row.groups)))
  }

  if(is.null(by)){
    result = result |>
      cols_align(align =  "center",
                 columns = !"variable") |>
      cols_label("variable" = "Variable")
  }
  return(result)
}

