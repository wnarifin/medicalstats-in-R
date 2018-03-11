#--------------------------------------------------------
# Data Analysis Using R (2017) IKU
#--------------------------------------------------------
# Wan Nor Arifin
#--------------------------------------------------------
# Comparing proportions
#--------------------------------------------------------

# Independent samples

## Chi-squared test (for association)
lung_table = read.table(header = F, text = "
20  12
55 113
")
lung_table = as.table( as.matrix(lung_table) )
dimnames(lung_table) = list(smoking = c("smoking", "no smoking"), cancer = c("lung cancer", "no lung cancer"))
str(lung_table); lung_table
addmargins(lung_table)
prop.test(lung_table)  # 2 x k
chi_lung = chisq.test(lung_table)  # k x k
chi_lung
str(chi_lung)
chi_lung$expected  # < 5, < 20%
library(MASS)
smoke = data.frame(sex = survey$Sex, smoke = survey$Smoke); str(smoke)
smoke = na.omit(smoke); str(smoke)
smoke_table = table(smoke); smoke_table
chi_smoke = chisq.test(smoke_table)
chi_smoke
chi_smoke$expected

## Fisher's exact test
fisher.test(lung_table)
fisher.test(smoke_table)

## Chi-squared test for trend
# - when grouping is ordinal
levels(smoke$smoke)
smoke$smoke1 = factor(smoke$smoke, levels = c("Never", "Occas", "Regul", "Heavy"))
levels(smoke$smoke1); str(smoke)
table(smoke$smoke, smoke$smoke1)
table(smoke$smoke == smoke$smoke1)
smoke_table1 = table(smoke = smoke$smoke1, sex = smoke$sex); smoke_table1
library(coin)
chisq_test(smoke_table1)  # common X2 test
chisq_test(smoke_table1, scores = list("smoke" = 0:3))  # smoke ordinal


# Dependent samples

## McNemar test
# - outcome: 2
# - repetition: 2
# --- PM rating (Agresti, pg409), n=1600
"Data:
            second
first        approve disapprove
  approve        794        150
  disapprove      86        570
"
pm_table = read.table(header = FALSE, text = "
794 150
86  570
")
pm_table = as.table( as.matrix(pm_table) )
dimnames(pm_table) = list(first = c("approve", "disapprove"), second = c("approve", "disapprove"))
str(pm_table); pm_table
addmargins(pm_table)  # view marginal counts
mcnemar.test(pm_table)

## Cochran's Q test
# - outcome: 2
# - repetition: > 2
lect = read.csv("lect.csv")  # student's understanding by lecturer
str(lect); lect
lect = as.data.frame( lapply(lect, factor) )  # have to factor
str(lect); lect
library(coin)
mh_test(understanding ~ lecturer | student, data = lect)

## Stuart-Maxwell test
# - outcome: 2, > 2
# - repetition: 2
# My stats lecture understanding level, n=200
"Data:
              after.lecture
before.lecture confused so-so understand
    confused         12     8         80
    so-so            10    10         20
    understand        5     8         47
"
stats_table = read.table(header = FALSE, text = "
12  8 80
10 10 20
 5  8 47
")
stats_table = as.table( as.matrix(stats_table) )
dimnames(stats_table) = list(
  before.lecture = c("confused", "so-so", "understand"),
  after.lecture = c("confused", "so-so", "understand")
  )
str(stats_table); stats_table
addmargins(stats_table)  # view marginal counts
mh_test(stats_table)  # as nominal
mh_test(stats_table, scores = list(response = 1:3))  # as ordinal
