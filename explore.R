#------------------------------------------------------------
# Exploratory data analysis
#------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------


# Read data

library(foreign)
data.sav = read.spss("data/cholest.sav", to.data.frame = T)
str(data.sav)

# Basic descriptive statistics

# - In general, provided the data are properly prepared,
summary(data.sav)
# - which are too simple, not really useful
# - better explore the data by variable types and our objective.

## Numerical
library(psych)  #to use describe
describe(data.sav[, c("chol","age", "exercise")])
# - bcs describe() does not report IQR (it reports median absolute deviation [MAD])
# - you may want to obtain IQR
sapply(data.sav[, c("chol","age", "exercise")], IQR)
# - IQR() function can only accept one var at a time
# - using sapply() allows analysis for > 1 var
# - Obtain 95% CI
library(epiDisplay)
sapply(data.sav[, c("chol","age", "exercise")], ci)

## Categorical
tab_sex = table(data.sav$sex); tab_categ = table(data.sav$categ)
tab_sex; tab_categ
prop.table(tab_sex)*100; prop.table(tab_categ)*100
cbind(n = tab_sex, "%" = prop.table(tab_sex)*100)
cbind(n = tab_categ, "%" = prop.table(tab_categ)*100)
# - now the shortcut
tab_all = sapply(data.sav[, c("sex", "categ")], table); tab_all
prop_all = sapply(tab_all, function(x) prop.table(x)*100); prop_all

# - after all, here is the shortcut...
library(epiDisplay)
codebook(data.sav)

# By groups

## Numerical
by(data.sav[, c("chol","age", "exercise")], data.sav$sex, describe)
by(data.sav[, c("chol","age", "exercise")], data.sav$categ, describe)
# - shortcut
describeBy(data.sav[, c("chol","age", "exercise")], data.sav$sex)
describeBy(data.sav[, c("chol","age", "exercise")], data.sav$categ)

## Categorical
# - Cross-tabulation
tab = table(Category = data.sav$categ, Gender = data.sav$sex); tab  # count
per = prop.table(table(Category = data.sav$categ, Gender = data.sav$sex))*100
per  # % 
cbind(tab, per)
addmargins(tab)  # marginal counts
# nicer view, % next to the count
cell = paste0(tab, " (", per, "%)")
str(tab)
tab1 = tab  # to give tab1 same dimension to tab
tab1[] = cell[]  # then copy all contents from cell to tab1
tab1
ftable(tab1)  # nicer 'flat' view

# - using by and aggregate
aggregate(categ ~ sex, summary, data = data.sav)
by(data.sav$categ, data.sav$sex, summary)
