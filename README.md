# JangTable

Functions for efficient clinical data analyses and tables. Various table functions are included for time-saving tables production.

## Lists of tables available

### Descriptive tables
- `baseTable()`: Baseline characteristics 
  - `by`: a category variable for aggregating by each categories.
  - `include`: variables that would be displayed in the baseline characteristics table.
  - `median.vars`: continuous variables to calculate median and IQR.
  - `by.order`: The order of categories shown in the baseline characteristics table.
  
- `varMissTable()`: Missing value count & proportions

### Logistic regression tables
- `lrUniTable()`: Univariable Logistic regression 
- `lrMultTable()`: Multivariable Logistic regression 
- `lrTableCombine()`: Merged(Univariable & Multivariable) Logistic regression

### Cox regression tables
- `coxUniTable()`: Univariable Cox regression 
- `coxMultTable()`: Multivariable Cox regression 
- `coxTableCombine()`: Merged(Univariable & Multivariable) Cox regression

## Installation

```{r}

# Install a stable development version from GitHub (requires compilation)

remotes::install_github("rchemist0123/JangTable")

```

