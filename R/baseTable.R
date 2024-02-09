#' Baseline characteristics table
#'
#' Create a baseline characteristic table by group variable
#' @param data A data for analysis.
#' @param by A categorical variable for calculating by group. If NULL, all of the data would be aggregated.
#' @param include Variables including in the table. If NULL, all variables in the data would be included.
#' @param time.vars time related variables, which are shown as a median(IQR)
#' @param binary.vars Variables having two categories. These variables will show N(%) of the last category.
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
                     binary.vars = NULL,
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
  if(any(is.na(data[[by]]))) {
    warning(gettextf("Missing values exist on group variable %s, NA are excluded.",sQuote(by)))
    data = data[!is.na(by)]
    }
  stopifnot("digits must be numeric class" = is.numeric(digits))
  n_value = data[, .N, by=by][['N']] # Number of category

  # TODO by count n수 format 필요
  tbls = lapply(include, \(x){
    .x_level = length(unique(data[[x]]))

    # Categorical variables
    if(class(data[[x]])[1] %in% c("character","factor","ordered") | .x_level < 5 ) {
      tab1 = data[,.N, keyby = x] |> _[, "n_prop" := paste0(get("N")," (", round(get("N")/sum(get("N"))*100,1),")")][,c(x,"n_prop"),with=F]
      colname_row = data.table(x,"") |> setnames(c('x','V2'),c(x,'n_prop'))
      cat_tbl = rbindlist(list(colname_row, tab1))

      if(x %in% binary.vars){
        tab1_1 = tab1[nrow(tab1),]
        cat_tbl = data.table(
          name = paste0(colname_row[[x]],": ",tab1_1[[x]]),
          n_prop = tab1_1[["n_prop"]]
        ) |> setnames(c("name"),x)
      }
      cat_tbl_total = cat_tbl |>
        setnames(c(x,"n_prop"), c("variable", paste0("total",' (n=',format(nrow(data),big.mark=","),')')))

      if(!is.null(by)){
        .by_level = length(unique(data[[by]]))
        data[[by]] = factor(data[[by]], levels = by.order)
        tab1 = data[,.N, keyby=c(by, x)] |> _[,"n_prop" := paste0(get("N")," (", round(get("N")/sum(get("N"))*100,1),")"), by=by][,c(by,x,"n_prop"),with=F]
        cat_tbl = dcast(tab1,
                     formula = paste0(c(by,x), collapse = "~"),
                     value.var="n_prop",
                     fill = "0 (0.0)")
        if(x %in% binary.vars){
          setnames(cat_tbl, names(cat_tbl)[length(cat_tbl)], paste0(x,": ", names(cat_tbl)[length(cat_tbl)]))
          cat_tbl = cat_tbl[,c(1, length(cat_tbl)),with=F] |>
            transpose(make.names = 1, keep.names = "variable")
        } else {
          cat_tbl = cat_tbl |>
            set(j = x, value="") |>
            setcolorder(x, after = 1) |>
            transpose(make.names = 1, keep.names = "variable")
        }
        setnames(cat_tbl,
                 old = names(cat_tbl)[(ncol(cat_tbl)-.by_level+1):ncol(cat_tbl)],
                 new = paste0(names(cat_tbl)[(ncol(cat_tbl)-.by_level+1):ncol(cat_tbl)],
                              paste0(" (n=", format(n_value, big.mark=","),")")))
        .cat_mat = data[,table(mget(c(x,by)))]
        ## p-value
        if (length(.cat_mat[.cat_mat<5])>2) pval = data[,fisher.test(get(by), get(x) ,simulate.p.value=TRUE )][['p.value']]
        else pval = .cat_mat |> chisq.test() |> _[['p.value']]
        pval_tbl = data.table(name = paste0(colname_row[[x]],": ",tab1_1[[x]]),
                              "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval,3),nsmall=3))) |>
                                setnames('name',"variable")

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
        setnames(old = x, new = paste0("total",' (n=',sprintf("%s", format(nrow(data), big.mark=",")),')'))

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
                              paste0(" (n=", format(n_value, big.mark=","),")")))

        .pval_form = paste0(x,"~",by) |> as.formula()
        if(.by_level >= 3) pval = anova(lm(.pval_form, data))[["Pr(>F)"]] |> _[1]
        else {
          tryCatch(
            pval = t.test(.pval_form, data)[['p.value']],
            error = function(e){
              warning(gettextf("Not enough observation in %s. Perform Wilcoxon Rank Sum test instead.", sQuote(x)))
            },
            finally =
              wilcox.test(.pval_form, exact=F, data=data)[['p.value']]
          )
          }
        cont_tbl = cbind(cont_tbl, "P-value" = ifelse(pval<0.001,'<0.001', format(round(pval,3), nsmall=3)))
        cont_tbl_final = merge(cont_tbl_total, cont_tbl, by="variable")
      } else {
        cont_tbl_total
      }
    }
  })
  result = tbls |>
    rbindlist(use.names = T)
  if(!is.null(row.groups)){
    tbls_split = lapply(names(row.groups), \(x) {
      if(nrow(result) < row.groups[[x]][1]) {} # only if by in includes
      else {
        temp = result[row.groups[[x]],]; col_len = length(names(temp))
        colnum = ncol(temp)
        blank_row = matrix(rep("", col_len), ncol=col_len) |> as.data.table()
        setnames(blank_row, names(blank_row), names(temp))
        temp2 = rbind(blank_row, temp)
        temp2[['category']] = c(x, rep("", length(row.groups[[x]])))
        setcolorder(temp2, "category", before=1)
      }
    })
    result = rbindlist(tbls_split)
  } else {
    result
  }
  result = result |>
    gt(rowname_col = T) |>
    cols_align(align = "center",
               columns = names(result)[2:ncol(result)]) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

  if(is.null(by)){
    result = result |>
      cols_align(align =  "center",
                 columns = !"variable") |>
      cols_label("variable" = "Variable")
  }
  return(result)
}

