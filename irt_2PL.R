#------------------------------------------------------------
# Item Response Theory Analysis: 2-Parameter Logistic
#------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------

# Introduction to IRT using R
# Our focus is on 2PL model only
# 1PL (Rasch), 3PL & 4PL have to learn on your own

# Textbooks & Articles
# "The Basics of Item Response Theory" by Frank B. Baker
# http://echo.edres.org:8080/irt/baker/final.pdf
# "Applying item response theory (IRT) modeling to questionnaire development, evaluation, and refinement"
# https://www.researchgate.net/profile/Maria_Edelen/publication/6432794_Applying_item_response_theory_(IRT)_modeling_to_questionnaire_development_evaluation_and_refinement/links/09e415092a186a0ccb000000.pdf

# Install required libraries (packages)
install.packages(c("psych", "ltm", "irtoys", "mirt", "latticeExtra"))

# Load required packages
library("psych")
library("ltm")
library("irtoys")
library("mirt")
library("latticeExtra")

# Read data set "mtf.csv" into "data.mtf" data frame
data.mtf = read.csv("mtf.csv", header = TRUE)  # Includes headers
head(data.mtf)  # View first 6 students in the data set
names(data.mtf)  # List down variables in the data set
dim(data.mtf)  # Data set consists of 10 variables and 160 students

# Percentages of correct answers (1) by items
response.frequencies(data.mtf)

# ltm package
#------------
# Basic statistics using ltm, focus on percentages of correct answers (1) by questions
descript(data.mtf)
# Perform the analysis with ltm(), and save the results in "irt.mtf"
irt.mtf = ltm(data.mtf ~ z1, IRT.param = TRUE)
coef(irt.mtf)  # Obtain difficulty and discrimination parameter estimates
summary(irt.mtf)  # Obtain LL, SE & z.vals
plot(irt.mtf, type = "ICC", legend = TRUE)  # Item Characteristic Curves
plot(irt.mtf, type = "ICC", legend = TRUE, items=3) # Q1c
plot(irt.mtf, type = "IIC", legend = TRUE)  # Item Information Curves
# or Item Information Function
plot(irt.mtf, type = "IIC", legend = TRUE, items=3) # Q1c
plot(irt.mtf, items = 0, type = "IIC")  # Test Information Function
information(irt.mtf, c(-3,3))  # Test information between -3 to +3 ability range
# "irtoys" package
plot(trf(est(data.mtf, model = "2PL", engine = "ltm")))  #Test Characteristic Curve
# or # Test Response Function
# Item fit
item.fit(irt.mtf)  # df = 10-2 = 8
# Fit for margins
margins(irt.mtf)
table(data.mtf[,5], data.mtf[,6])
# Personfit
person.fit(irt.mtf)
# Unidimensional test
unidimTest(irt.mtf)  # takes long time to run
irt.mtf2 = ltm(data.mtf ~ z1 + z2)
anova(irt.mtf, irt.mtf2)

# "mirt" package
#---------------
# specify model
# model.uni = mirt.model("F1 = 1-10")
# fit the model
# mirt.mtf = mirt(data.mtf, model.uni, itemtype = "2PL")
# simple way to fit the model
mirt.mtf = mirt(data.mtf, 1, itemtype = "2PL")
coef(mirt.mtf, IRTpars = T, simplify = T)
# test info
areainfo(mirt.mtf, c(-3,3))
# plots
plot(mirt.mtf, type = "trace")
plot(mirt.mtf, type = "infotrace")
plot(mirt.mtf, type = "info")
plot(mirt.mtf, type = "infoSE")
plot(mirt.mtf)
# model fit
M2(mirt.mtf)  # M2 nsig.
itemfit(mirt.mtf)
personfit(mirt.mtf)
# reliabilities: marginal & empirical
marginal_rxx(mirt.mtf)  # 0.5574205
theta_se = fscores(mirt.mtf, full.scores.SE = T)
empirical_rxx(theta_se)  # 0.5681729
# "Item Response Theory approaches to test scoring and evaluating the score accuracy" by Anna Brown
# "https://kar.kent.ac.uk/44777/1/Brown%20-%20IRT%20Test%20Scoring%20-%202nd%20revision%20-%20Accepted.pdf"

# Chronbach's alpha
alpha(data.mtf)
descript(data.mtf)$alpha

# lavaan package
#---------------
# fit unidimensional model
col(data.mtf, as.factor = T)[1,]
model = "
KNOW =~ Q1A + Q1B + Q1C + Q1D + Q1E + Q2A + Q2B + Q2C + Q2D + Q2E
"
library(lavaan)
cfa.mtf = cfa(model, data = data.mtf, estimator = "wlsmv", ordered = names(data.mtf))
summary(cfa.mtf, fit.measures = T, standardized = T)
