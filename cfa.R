#----------------------------------------------------------
# Confirmatory Factor Analysis & Reliability (MSc Dec 2016)
#----------------------------------------------------------

# LEARNING OUTCOMES
# 1. Perform CFA analysis, extending knowledge from EFA.
# 2. Perform reliability analysis on the fitted model.

# Libraries
library(foreign)
library(psych)
library(lavaan)
library(semTools)
library(semPlot)

# Load data
data = read.spss("Attitude_Statistics v3.sav", F, T)
names(data)
data.cfa = data[c("Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11")]  # items from F1 & F2 only
names(data.cfa)

# Descriptive statistics
## Missing values, min-max
describe(data.cfa)
## Response frequencies
response.frequencies(data.cfa)

# Multivariate normality
mardia(data.cfa)
# we think that the data is MV normal, very close to 5

# step 1 - specify the measurement model
model = "
F1 =~ Q4 + Q5 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
"

# step 2 - fit the model
cfa.model = cfa(model, data = data.cfa)
# cfa.model = cfa(model, data = data.cfa, std.lv = 1)  # latent variance = 1 approach
summary(cfa.model, fit.measures = T, standardized = T)

# step 3 - model revision
mi = modificationIndices(cfa.model)
subset(mi, mi>5)  # just to view MI>5 only
# start lower than 10 to also view relatively high mi
# mi>10, Q9 ~~ Q10  # intentionally skip bcs problematic
sr = residuals(cfa.model, type="standardized")  # > 2.58, Q5
sr

# Rev 1: Q5 ~~ Q11?
# Both from F2, reasonable by content.
model1 = "
F1 =~ Q4 + Q5 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
Q5 ~~ Q11
"
cfa.model1 = cfa(model1, data = data.cfa)
summary(cfa.model1, fit.measures=T, standardized=T)
fitMeasures(cfa.model1, c("aic", "bic"))
fitMeasures(cfa.model, c("aic", "bic"))
# Very small improvement in AIC, BIC. RMSEA UL90% CI still > .08
mi1 = modificationIndices(cfa.model1)
subset(mi1, mi>5)
# mi not helpful
sr1 = residuals(cfa.model1, type="standardized")
sr1
# Still very high SR Q5-Q8

# Rev 2: RMV Q5?
# SR = 
model2 = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
"
cfa.model2 = cfa(model2, data = data.cfa)
summary(cfa.model2, fit.measures=T, standardized=T)
fitMeasures(cfa.model, c("aic", "bic"))
fitMeasures(cfa.model1, c("aic", "bic"))
fitMeasures(cfa.model2, c("aic", "bic"))
# Improves: AIC, BIC, & other fit indices. RMSEA UL90% CI still > .08
mi2 = modificationIndices(cfa.model2)
subset(mi2, mi>5)
# mi not helpful
sr2 = residuals(cfa.model2, type="standardized")
sr2
# No more SR problem

# Rev 3: RMV Q8?
# SR = 
model3 = "
F1 =~ Q4 + Q5 + Q6 + Q7 + Q11
F2 =~ Q9 + Q10
"
cfa.model3 = cfa(model3, data = data.cfa)
summary(cfa.model3, fit.measures=T, standardized=T)
# Q9 loading > 1, Heywood case! Not an acceptable solution.

# It is reasonable to just accept model2 as it is.

# Extra testing...
# Can improve model2 further?
# Rev 2a: RMV Q11?
# Low loading
# SR = 
model2a = "
F1 =~ Q4 + Q6 + Q7
F2 =~ Q8 + Q9 + Q10
"
cfa.model2a = cfa(model2a, data = data.cfa)
summary(cfa.model2a, fit.measures=T, standardized=T)
# Worsen CFI, TLI & RMSEA

# Rev 2b:
# Q9 ~~ Q10
# Low loading
# SR = 
model2b = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
Q9 ~~ Q10
"
cfa.model2b = cfa(model2b, data = data.cfa)
summary(cfa.model2b, fit.measures=T, standardized=T)
# Heywood case, again to Q8

# Reliability
reliability(cfa.model2)  # Raykov's rho is the omega
# omega  F1 = 0.8077253 F2 = 0.8357834

# path diagram
semPaths(cfa.model2, 'path', 'std', style = 'lisrel', 
         edge.color = 'black', intercepts = F)