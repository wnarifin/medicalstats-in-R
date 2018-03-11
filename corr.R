#--------------------------------------------------------
# Data Analysis Using R (2017) IKU
#--------------------------------------------------------
# Wan Nor Arifin
#--------------------------------------------------------
# Correlations
#--------------------------------------------------------

# Pearson's correlation
# -numerical
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = T)
str(cholest)
plot(cholest$chol ~ cholest$age)
abline(lm(chol ~ age, cholest))
cor(cholest$chol, cholest$age)
cor.test(cholest$chol, cholest$age)
# you can also explore what is offered by psych package

# Spearman's correlation
# - ranked
cor(cholest$chol, cholest$age, method = "spearman")
cor.test(cholest$chol, cholest$age, method = "spearman")

# Others

## phi (binary x binary)
lung = read.csv("lung.csv")
str(lung); table(lung)
levels(lung$Smoking)
lung$Smoking = relevel(lung$Smoking, ref = "smoking")
levels(lung$Smoking); table(lung)
library(psych)
phi(table(lung))

## tetrachoric (binary x binary)
# - code as 0 1
# smoking & cancer as smoking/cancer=1 & no smoking/cancer=0
lung1 = NULL
lung1$Smoking = rep(c(1,0,1,0), c(20,55,12,113))
lung1$Cancer = rep(c(1,1,0,0), c(20,55,12,113))
lung1 = as.data.frame(lung1)
table(lung1)
tetrachoric(lung1)

## polychoric (ordinal x ordinal)
# - code as ordinal e.g. 1 2 3
# rating by two doctors, mild=1 moderate=2 severe=3
doc = read.csv("doc.csv")
str(doc); head(doc); table(doc)
polychoric(doc)

## biserial (binary x numerical)
str(cholest)
cholest$sex1 = as.numeric(cholest$sex)-1  # convert factored sex to 1/0
biserial(cholest$age, cholest$sex1)  # x = cont, y = binary
biserial(cholest$chol, cholest$sex1)
# cannot biserial chol ~ sex1 -> error, not suitable for analysis

## polyserial (ordinal x numerical)
str(cholest)
cholest$categ1 = as.numeric(cholest$categ)
polyserial(cholest$age, cholest$categ1)
# cannot perform complex polyserial with psych

## Using `polychor`
library(polycor)
polychor(lung1$Smoking, lung1$Cancer)  # tetrachoric
polychor(doc$doc1, doc$doc2)  # polychoric
polyserial(cholest$age, cholest$sex1)  # biserial
polyserial(cholest$age, cholest$categ1)  # polyserial