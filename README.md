# JangTable

Functions for efficient clinical data analyses and tables. Various table functions are included for time-saving tables production.

## Lists of tables available

### Tables
- `baseTable()`: A table for baseline characteristics.
  - `data`: data for calculation
  - `include`: Variables that would be displayed in the baseline characteristics table.
  - `by`: A category variable for aggregating by each categories.
  - `median.vars`: Continuous variables to calculate median and IQR.
  - `binary.var`: Binary / Dichotomous variables to show only one category.
  - `by.order`: The order of categories shown in the baseline characteristics table.
  - `total`: Whether include total characteristics of variables. The default is `TRUE`.
  - `digits`: Digits of aggregated values. The default is 1.
  - `p.digits`: Digits of p-values. The default is 3.
  
- `varMissTable()`: A table for Missing value count & proportions.
  - `data`: Data for calculation
  - `include`: Variables to check frequencies and proportions of `NA`.
- `subgroupTable()`: A table for subgroup analyses.
  - `data`
  - `fit`: A logistic or Cox regression model.
  - `treatment`: A variable that could divide case & control group. Must be factor variables.
  - `subgroup`: Categorical Variable(s) for the subgroup analysis. Must be factor variables.
  - `digits`: Digits of printed values.
- `uniTable()`: Integrated univariable tables
- `multTable()`: Integrated multivariable tables
- `lrUniTable()`: Univariable Logistic regression 
- `lrMultTable()`: Multivariable Logistic regression 
- `lrTableCombine()`: Merged(Univariable & Multivariable) Logistic regression
- `coxUniTable()`: Univariable Cox regression 
- `coxMultTable()`: Multivariable Cox regression 
- `coxTableCombine()`: Merged(Univariable & Multivariable) Cox regression

### Integrated regression
- `uniReg()` : Univariable logistic or Cox regression
- `multReg()`: Multivariable logistic or Cox regression

## Installation
```{r}
# Install a stable development version from GitHub (requires compilation)
remotes::install_github("rchemist0123/JangTable")
```

