#--------------------------------------------------------
# Structural Equation Modeling (DrPH)
#-------------------------------------------------------

# LEARNING OUTCOMES
# 1. Understand and apply the basic knowledge of SEM.
# 2. Specify SEM models involving correlational, causal, mediating & moderating effects, and interpret the results.

# Libraries
library(foreign)
library(psych)
library(lavaan)
library(semTools)
library(semPlot)

# Load data
data = read.spss("Attitude_Statistics v3.sav", F, T)
dim(data)
names(data)
describe(data[-1])

# Correlational & causal
# Correlations between:
## Observed variables
# Q4 & Q11
model.c = "
Q4 ~~ Q11
"
corr.c = sem(model.c, data = data, meanstructure = T)  # meanstructure -> display mean
summary(corr.c, fit.measures = T, standardized = T)
semPaths(corr.c, what = "path", whatLabels = "par", edge.color = "black")
cor(data$Q4, data$Q11)
## Latent variables
model.cl = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
"
corr.cl = sem(model.cl, data = data)
summary(corr.cl, fit.measures = T, standardized = T)
semPaths(corr.cl, what = "path", whatLabels = "par", edge.color = "black", layout = "tree2")

# Causal effects:
## Observed variables
# Q4 & Q11
model.cs = "
Q4 ~ Q11
"
cause.cs = sem(model.cs, data = data)
summary(cause.cs, fit.measures = T, standardized = T)
lm(formula = Q4 ~ Q11, data = data)  # compare with SLR
semPaths(cause.cs, what = "path", whatLabels = "par", edge.color = "black", rotation = 2)
## Latent variables
model.csl = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
F2 ~ F1
"
cause.csl = sem(model.csl, data = data)
summary(cause.csl, fit.measures = T, standardized = T)
semPaths(cause.csl, what = "path", whatLabels = "par", style = "lisrel", edge.color = "black", 
         rotation = 2, sizeMan = 4, sizeLat = 6, residuals = F)
## Multiple variables
model.cs1 = "
Q4 ~ Q7 + Q11 + Q6
"
cause.cs1 = sem(model.cs1, data = data)
summary(cause.cs1, fit.measures = T, standardized = T)
semPaths(cause.cs1, what = "path", whatLabels = "par", edge.color = "black", rotation = 2)
lm(Q4 ~ Q7 + Q11 + Q6, data = data)  # compare with MLR


# Mediation
model.me = "
Q4 ~ c*Q7 + b*Q11
Q11 ~ a*Q7  # mediator
ab := a*b  # indirect effect
total := c + a*b  # total effect
"
med.me = sem(model.me, data = data)
summary(med.me, fit.measures = T, standardized = T)
semPaths(med.me, what = "path", whatLabels = "name", edge.color = "black", layout = "spring")
semPaths(med.me, what = "path", whatLabels = "par", edge.color = "black", layout = "spring")
model.me1 = "
Q4 ~ c*Q7
"
med.me1 = sem(model.me1, data = data)
summary(med.me1, fit.measures = T, standardized = T)

# Moderation/Interraction
data$Q7.Q8 = data$Q7*data$Q8  # create interraction
head(data)
model.mo = "
Q4 ~ Q7 + Q8 + Q7.Q8
"
mod.mo = sem(model.mo, data = data)
summary(mod.mo, fit.measures = T, standardized = T)  # complete moderation
semPaths(mod.mo, what = "path", whatLabels = "par", edge.color = "black", residuals = F)
lm(Q4 ~ Q7 + Q8 + Q7*Q8, data = data)


# Exercise:
# Use builtin data set
data("HolzingerSwineford1939")
data("PoliticalDemocracy")
# measurement model
"
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"
data1 = HolzingerSwineford1939
dim(data1)
names(data1)
head(data1)
str(data1)
data1$sex1 = data1$sex - 1  # recode 1,2 to 0,1, dummy variable
head(data1)
describe(data1)

# categorical var
sex.model = "
visual   =~ x1 + x2 + x3
visual ~ sex1
"
sem.sex = sem(sex.model, data = data1)
summary(sem.sex, fit.measures = T, standardized = T)
semPaths(sem.sex, what = "path", whatLabels = "par", edge.color = "black", residuals = F, rotation = 2)

# numerical var
age.model = "
speed   =~ x7 + x8 + x9
speed ~ ageyr
"
sem.age = sem(age.model, data = data1)
summary(sem.age, fit.measures = T, standardized = T)
semPaths(sem.age, what = "path", whatLabels = "par", edge.color = "black", residuals = F, rotation = 2)

data2 = PoliticalDemocracy
dim(data2)
names(data2)
cat(names(data2[1:7]), sep = " + ")
model.y = "
Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7
"
cfa.y = cfa(model.y, data2)
summary(cfa.y, fit.measures = T, standardized = T)
model.x = "
X =~ x1 + x2 + x3
"
cfa.x = cfa(model.x, data2)
summary(cfa.x, fit.measures = T, standardized = T)
model.x.y = "
Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7
X =~ x1 + x2 + x3
"
cfa.x.y = cfa(model.x.y, data2)
summary(cfa.x.y, fit.measures = T, standardized = T)

model.y_x = "
Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7
X =~ x1 + x2 + x3
X ~ Y
"
sem.y_x = cfa(model.y_x, data2)
summary(sem.y_x, fit.measures = T, standardized = T)
semPaths(sem.y_x, what = "path", whatLabels = "par", rotation = 2, 
         edge.color = "black", edge.label.cex = 1.5, residuals = F,
         sizeMan = 8, sizeLat = 8)
