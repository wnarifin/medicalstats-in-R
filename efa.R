#------------------------------------------------------------
# Exploratory factor analysis and Cronbach's alpha
#------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------

# Libraries
library(foreign)
library(psych)
library(MVN)

# Load data set
data = read.spss("Attitude_Statistics v3.sav", use.value.labels = F, to.data.frame = T)
head(data)
data1 = data[-1]  # no ID
dim(data1)
head(data1)

# Exploratory factor analysis
#----------------------------

# Preliminary steps
## Descriptive statistics:
### Check minimum-maximum values per item.
describe(data1)
### n(%) of response to options per item.
response.frequencies(data1)
## Normality of data
### Univariate normality
# Histograms
par(mfrow = c(3,4))  # set view to 3 rows & 4 columns
apply(data1, 2, hist)
par(mfrow = c(1,1))  # set to default full view
# multi.hist(data1)  # at times, error
# Shapiro Wilk's test
apply(data1, 2, shapiro.test)
### Multivariate normality
mardiaTest(data1, qqplot = TRUE)

# Step 1
## 1. Check suitability of data for analysis
### a) Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy
KMO(data1)  # middling
### b) Bartlet's test of sphericity
cortest.bartlett(data1)  # <0.05
## 2. Determine the number of factors by
### a) Eigenvalues
### b) Scree plot
scree = scree(data1)  # we are only concerned with FA scree & eigenvalues
print(scree)
### c) Parallel analysis
parallel = fa.parallel(data1, fm = "pa", fa = "fa")
print(parallel)

# Step 2
## 1. Run EFA by fixing number of factors as decided from previous step.
## 2. Decide on rotation method. Choose an oblique rotation, Promax.
fa = fa(data1, nfactors = 2, rotate = "promax", fm = "pa")
print(fa)
print(fa, cut = .3, digits = 3)
# h2 = communalities
# u2 = error variance

## Assess the results:
## 1. To judge quality of items. Remove poor performing items.
# Communality? Q1 < Q12 < Q2 < .25
# Pattern coeff/factor loading FL? Q1 < .3, Q12 < .4, Q2 & Q3 < .5 
## 2. To check for overlap between factors.
# PA1-PA2 = .107 < .85 OK

# Step 3
## 1. Re-run the analysis similar to Step 2 every time an item is removed. Make judgment based on the results.
## 2. The analysis is finished when:
### Satisfying number of factors.
### Satisfactory quality of items.
# remove Q1? Low com & FL
fa1 = fa(data1[-1], nfactors = 2, rotate = "promax", fm = "pa")
print(fa1, cut = .3, digits = 3)
# remove Q12? Low com & FL
fa2 = fa(data1[-c(1,12)], nfactors = 2, rotate = "promax", fm = "pa")
print(fa2, cut = .3, digits = 3)
# remove Q2? Low com & FL
fa3 = fa(data1[-c(1,2,12)], nfactors = 2, rotate = "promax", fm = "pa")
print(fa3, cut = .3, digits = 3)
# remove Q3? Low com & FL
fa4 = fa(data1[-c(1,2,3,12)], nfactors = 2, rotate = "promax", fm = "pa")
print(fa4, cut = .3, digits = 3)
# DONE!!!
# Summary:
# PA1 =~ Q4, Q5, Q6, Q7, Q11
# PA2 =~ Q8, Q9, Q10
# Name the factor?

# Cronbach's alpha
#-----------------

# Determine the reliability for each factor separately by including the selected items only.
names(data1)
PA1 = c("Q4","Q5","Q6","Q7","Q11")
PA2 = c("Q8","Q9","Q10")

alpha.pa1 = alpha(data1[PA1])
print(alpha.pa1)
# r.drop = Corrected item-total correlation
# look at raw_alpha for Cronbach's alpha if item deleted
smc(data1[c(4,5,6,7,11)])  # Squared Multiple Correlation (smc)

alpha.pa2 = alpha(data1[PA2])
print(alpha.pa2)
smc(data1[c(8,9,10)])  # Squared Multiple Correlation (smc)
