#' Baseline characteristics table
#'
#' Create a baseline characteristic table by group variable
#' @param data A data for analysis.
#' @param by A categorical variable for calculating by group. If NULL, all of the data would be aggregated.
#' @param include Variables including in the table. If NULL, all variables in the data would be included.
#' @param median.vars Variables aggregated to median and IQR.
#' @param binary.vars Variables having two categories. These variables will show N and proportions of the last category.
#' @param row.groups Experimental. A group for clinical category. Must be a list with group names of categories and row numbers.
#' @param by.order The order of character variable using in `by`. Must be a vector.
#' @param total Logical. Whether includes aggregation values of the all data. The default is TRUE.
#' @param missing Logical. Whether includes missing variable in categorical varibles. The default is TRUE.
#' @param digits Digits of result values. The default is 1.
#' @param p.digits Digits of p values. The default is 3.
#' @return A baseline characteristics table.
#' @importFrom data.table setDT copy setnames rbindlist set setcolorder transpose .N := data.table is.data.table
#' @importFrom stats kruskal.test fisher.test t.test chisq.test anova lm median quantile sd wilcox.test
#' @importFrom gt gt cols_align cols_label tab_style cell_text cells_column_labels tab_row_group row_group_order fmt_markdown
#' @examples
#' # example code
#' baseTable(
#'  data = mtcars,
#'  by = "cyl",
#'  include = c("mpg", "am","hp","drat","wt","gear","qsec", "disp"),
#'  binary.vars = c("vs","am"),
#'  median.vars = c("drat","wt")
#'  )
#' @export
baseTable = function(data, by = NULL, include = NULL, median.vars = NULL,
                     binary.vars = NULL, by.order = NULL, total=TRUE,
                     missing = TRUE,
                     digits = 1L, p.digits= 3L, row.groups=NULL){

  stopifnot("digits must be integer class" = is.numeric(digits))
  stopifnot("p.digits must be integer class" = is.numeric(p.digits))
  if(!is.data.table(data)) data = setDT(copy(data))
  else data = copy(data)
  if(is.null(include)) include = names(data)
  if(!is.null(by)){
    if(is.null(by.order)) by.order = levels(factor(data[[by]]))
    n_value = table(factor(data[[by]], levels=by.order)) # Number of category
    if(by %in% include) {
      include = setdiff(include, by)
      warning(gettextf("Group variable %s in include variables, excluded.", sQuote(by)))
    }
    if(!by %in% names(data)) stop(gettextf("%s not found in data.", sQuote(by)))
    if(any(is.na(data[[by]]))) {
      warning(gettextf("Missing values exist on group variable %s, NA are excluded.",sQuote(by)))
      data = data[!is.na(by)]
    }
  } else {
    n_value = nrow(data)
  }
  # TODO: missing variables
  tbls = lapply(include, \(x){
    if(!missing) data = data[!is.na(x)]
    else data = data
    .x_level = length(unique(data[[x]]))
    ############### Categorical variables ##################
    if(class(data[[x]])[1] %in% c("character","factor","ordered") | .x_level < 5 ) {
      tab1 = data[,.N, keyby = x] |> _[, "n_prop" := sprintf("%s (%s)", get("N"), round(get("N")/sum(get("N"))*100, digits))][,c(x,"n_prop"),with=F]
      colname_row = data.table(x,"") |> setnames(c('x','V2'),c(x,'n_prop'))
      cat_tbl = rbindlist(list(colname_row, tab1))

      if(x %in% binary.vars){
        tab1_1 = tab1[nrow(tab1),]
        cat_tbl = data.table(
          name = sprintf("%s: %s", colname_row[[x]], tab1_1[[x]]) ,
          n_prop = tab1_1[["n_prop"]]
        ) |> setnames(c("name"),x)
      }
      cat_tbl_total = cat_tbl |>
        setnames(c(x,"n_prop"), c("Variable", sprintf("total (n=%s)",format(nrow(data),big.mark=","))))
      cat_tbl_total[-1, x:= sprintf("\U00A0\U00A0\U00A0\U00A0%s",x), env = list(x="Variable")]
      if(!is.null(by)){
        .by_level = length(unique(data[[by]]))
        data[[by]] = factor(data[[by]], levels = by.order)
        tab1 = data[,.N, keyby=c(by, x)] |> _[,"n_prop" := sprintf("%s (%s)", get("N"), round(get("N")/sum(get("N"))*100, digits)), by=by][,c(by, x,"n_prop"), with=F]
        cat_tbl = dcast(tab1,
                        formula = sprintf("%s ~ %s", by, x),
                        value.var="n_prop",
                        fill = "0 (0.0)")

        setnames(cat_tbl,
                 names(cat_tbl)[-1],
                 sprintf("\U00A0\U00A0\U00A0\U00A0%s",names(cat_tbl)[-1]))
        if(x %in% binary.vars){
          setnames(cat_tbl,
                   names(cat_tbl)[length(cat_tbl)],
                   sprintf("%s: %s",x, trimws(names(cat_tbl))[length(cat_tbl)]))
          cat_tbl = cat_tbl[,c(1, length(cat_tbl)), with=F] |>
            transpose(make.names = 1, keep.names = "Variable")
        } else {
          cat_tbl = cat_tbl |>
            set(j = x, value="") |>
            setcolorder(x, after = 1) |>
            transpose(make.names = 1, keep.names = "Variable")
        }
        setnames(cat_tbl,
                 names(cat_tbl)[(ncol(cat_tbl)-.by_level+1):ncol(cat_tbl)],
                 paste0(names(cat_tbl)[(ncol(cat_tbl) - .by_level+1):ncol(cat_tbl)],
                              paste0(" (n=", format(n_value, big.mark=","),")")))

        ## p-value
        .cat_mat = data[,table(mget(c(x,by)))]
        if (length(.cat_mat[.cat_mat <5])>2) pval = data[,fisher.test(get(by), get(x) ,simulate.p.value=TRUE )][['p.value']]
        else pval = .cat_mat |> chisq.test() |> _[['p.value']]
        if(x %in% binary.vars) {
          pval_tbl = data.table(name = paste0(colname_row[[x]],": ",tab1_1[[x]]),
                                "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval, p.digits),nsmall=p.digits))) |>
                                  setnames('name',"Variable")
        } else {
          pval_tbl = data.table(x, "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval, p.digits),nsmall=p.digits))) |>
            setnames('x',"Variable")
        }

        cat_tbl = merge(cat_tbl, pval_tbl, by="Variable", all.x = T, sort=F)
        cat_tbl[is.na(cat_tbl)] = ""
        if(!total){
          cat_tbl_final = cat_tbl
        } else {
          cat_tbl_final = merge(cat_tbl_total, cat_tbl, by="Variable", sort=F)
        }
      } else {
        cat_tbl_total
      }
    } else {
      #############  Continuous Variables  ####################
      if(x %in% median.vars){
        cont_tbl_total = data[,lapply(.SD, \(x) .cont_med_iqr(x, digits)), .SDcols=x]
      } else {
        cont_tbl_total = data[,lapply(.SD, \(x) .cont_mean_sd(x, digits)), .SDcols=x]
      }

      cont_tbl_total = cont_tbl_total |>
        set(j = "Variable", value = x) |>
        setcolorder("Variable",before = 1) |>
        setnames(x, sprintf("total (n=%s)", format(nrow(data), big.mark=",")))

      if(!is.null(by)){
        .by_level = length(unique(data[[by]]))
        data[[by]] = factor(data[[by]], levels = by.order)
        if(x %in% median.vars){
          cont_tbl = data[,lapply(.SD, \(x) .cont_med_iqr(x, digits)), .SDcols=x, keyby = by]
        } else {
          cont_tbl = data[,lapply(.SD, \(x) .cont_mean_sd(x, digits)), .SDcols=x, keyby = by]
        }
        cont_tbl = cont_tbl |> transpose(make.names = 1, keep.names = "Variable")
        setnames(cont_tbl,
                 names(cont_tbl)[(ncol(cont_tbl)-.by_level+1):ncol(cont_tbl)],
                 paste0(names(cont_tbl)[(ncol(cont_tbl)-.by_level+1):ncol(cont_tbl)],
                              paste0(" (n=", format(n_value, big.mark=","),")")))

        .pval_form = sprintf("%s ~ %s", x, by) |> as.formula()

        if(.by_level >= 3) {
        # ANOVA
          if(x %in% median.vars) {
            pval = kruskal.test(.pval_form, data)[['p.value']]
          } else {
            pval = anova(lm(.pval_form, data))[["Pr(>F)"]] |> _[1]
          }
        } else {
        # T-test
          if(x %in% median.vars){
            pval = wilcox.test(.pval_form, exact=F, data)[['p.value']]
          } else {
            tryCatch(
              assign("pval", t.test(.pval_form, data)[['p.value']]),
              error = function(e){
                warning(gettextf("Not enough observation in %s. Perform Wilcoxon Rank Sum test instead.", sQuote(x)))
              },
              finally = assign("pval", wilcox.test(.pval_form, exact=F, data=data)[['p.value']])
            )
          }
        }
        cont_tbl = cbind(cont_tbl, "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval, p.digits), nsmall=p.digits)))
        if(!total){
          cont_tbl_final = cont_tbl
        } else {
          cont_tbl_final = merge(cont_tbl_total, cont_tbl, by="Variable")
        }
      } else {
        cont_tbl_total
      }
    }
  })
  result = tbls |>
    rbindlist(use.names = T)
  # TODO Split rows by `row.groups`
  # if(!is.null(row.groups)){
  #   tbls_split = lapply(names(row.groups), \(x) {
  #     if(nrow(result) < row.groups[[x]][1]) {} # only if by in includes
  #     else {
  #       temp = result[row.groups[[x]],]; col_len = length(names(temp))
  #       colnum = ncol(temp)
  #       blank_row = matrix(rep("", col_len), ncol=col_len) |> as.data.table()
  #       setnames(blank_row, names(blank_row), names(temp))
  #       temp2 = rbind(blank_row, temp)
  #       temp2[['category']] = c(x, rep("", length(row.groups[[x]])))
  #       setcolorder(temp2, "category", before=1)
  #     }
  #   })
  #   result = rbindlist(tbls_split)
  # } else {
  #   result
  # }
  result = result |>
    gt(rowname_col = T) |>
    fmt_markdown(
      columns = "Variable") |>
    cols_align(align = "center",
               columns = names(result)[2:ncol(result)]) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

  if(is.null(by)){
    result = result |>
      cols_align(align =  "center",
                 columns = !"Variable")
  }
  return(result)
}

