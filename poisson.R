#-------------------
# Poisson Regression 
#-------------------

# Basic

# p(x) = p(x) = u^x exp(-u)/x!

# ex. 2.1-2.3
dpois(10, 17.2)  # P(X = x)
ppois(10, 17.2)  # cumulative P(X <= x)
1 - ppois(10, 17.2)  # P(X > x) = 1 - P(X <= x)

# plot
X = 0:50
X = as.data.frame(X)
p.X = apply(X, 2, function(x) dpois(x, 17.2))
cbind(x=X, px=p.X)
plot(p.X)

library(epiDisplay)  # replaces epicalc
library(car)

# Fit simple Poisson regression model
#------------------------------------

# Ex. 3.1, Count data

# X categorical, UKaccident.csv is modified from builtin data Seatbelts
acc = read.csv("UKaccident.csv")
head(acc)
str(acc)
dim(acc)
# driverskilled: number of death
# law: before seatbelt law = 0, after law = 1
model.acc = glm(driverskilled ~ law, data = acc, family = poisson)
summary(model.acc)  # significant p based on Wald test
# to get CI
cbind(coef(model.acc), confint(model.acc))
# By LR test:
# model without variable
model.acc0 = glm(driverskilled ~ 1, data = acc, family = poisson)
summary(model.acc0)
lrtest(model.acc0, model.acc)
anova(model.acc0, model.acc, test = "LRT")
# Model fit
poisgof(model.acc)  # fit well, based on chi-square test on the residual deviance
pchisq(model.acc$deviance, model.acc$df.residual, lower.tail = F)  # manually
# for model comparison, Aikaike Info. Criterion
AIC(model.acc0)
AIC(model.acc)
# Diagnostics
# standardized residuals
sr = rstandard(model.acc)
sr[abs(sr) > 1.96]
# view predicted count/fitted values
fitted.acc = model.acc$fitted
data.frame(acc, fitted.acc)
# to get the RR
exp(cbind(coef(model.acc), confint(model.acc)))
idr.display(model.acc)  # easier, also view LR test
86.26667/110.52336 # = 0.7805288

# X numerical
# Data http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
# num_awards: The number of awards earned by students at one high school.
# math: the score on their final exam in math."
aw = read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
head(aw)
str(aw)
dim(aw)
model.aw = glm(num_awards ~ math, data = aw, family = poisson)
summary(model.aw)  # sig. math
cbind(coef(model.aw), confint(model.aw))
poisgof(model.aw)  # fit well
sr = rstandard(model.aw)
sr[abs(sr) > 1.96]
data.frame(aw[c(4,2)], predicted = model.aw$fitted)
# 1 unit increase in math score
idr.display(model.aw)
# 10 unit increase in math score?
b1 = coef(model.aw)[[2]]*10
b1.ll = confint(model.aw)[[2]]*10
b1.ul = confint(model.aw)[[4]]*10
exp(cbind("Math RR" = b1, "95% LL" = b1.ll, "95% UL" = b1.ul))


# 3.2 rate data
# data in Fleiss et al 2003
" Table 12.1
  cigar.day person.yrs cases        rate        pred
1       0.0       1421     0 0.000000000 0.000793326
2       5.2        927     0 0.000000000 0.001170787
3      11.2        988     2 0.002024291 0.001834458
4      15.9        849     2 0.002355713 0.002607843
5      20.4       1567     9 0.005743459 0.003652195
6      27.4       1409    10 0.007097232 0.006167215
7      40.8        556     7 0.012589928 0.016813428
"
cigar.day = c(0, 5.2, 11.2, 15.9, 20.4, 27.4, 40.8)
person.yrs = c(1421, 927, 988, 849, 1567, 1409, 556)
cases = c(0, 0, 2, 2, 9, 10, 7)
cig = data.frame(cigar.day, person.yrs, cases)
cig
cig$rate = cig$cases/cig$person.yrs
cig

model.cig = glm(cases ~ cigar.day, offset = log(person.yrs), data = cig, family = "poisson")
summary(model.cig)
poisgof(model.cig)
cig$pred = model.cig$fitted/cig$person.yrs
cig
idr.display(model.cig)  # interpret?
exp(coef(model.cig)[[2]]*5)  # interpret?
exp(coef(model.cig)[[2]]*10)  # interpret?

# Fit multiple Poisson regression model
#--------------------------------------

# Data http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
# num_awards: The number of awards earned by students at one high school.
# prog: 1 = General, 2 = Academic, 3 = Vocational
# math: the score on their final exam in math."
aw = read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
str(aw)
head(aw)
dim(aw)
aw$prog1 = factor(aw$prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
str(aw)
head(aw)

# Univariable
#------------
# Math
model.aw.u1 = glm(num_awards ~ math, data = aw, family = poisson)
summary(model.aw.u1)
idr.display(model.aw.u1)
# Math sig.

# Prog
model.aw.u2 = glm(num_awards ~ prog1, data = aw, family = poisson)
summary(model.aw.u2)
idr.display(model.aw.u2)
# Vocational vs General not sig. -> Combine
aw$prog2 = aw$prog
# aw$prog2[aw$prog2 == 3] = 1  # org rajin
aw$prog2 = recode(aw$prog2, "2=2; else=1")
aw$prog2 = factor(aw$prog2, levels = 1:2, labels = c("General & Vocational", "Academic"))
as.data.frame(levels(aw$prog2))
str(aw)
codebook(aw)
head(aw)
# General & Vocational vs Academic
model.aw.u2a = glm(num_awards ~ prog2, data = aw, family = poisson)
summary(model.aw.u2a)
idr.display(model.aw.u2a)  # Sig.
table(aw$num_awards, aw$prog2)
tapply(aw$num_awards, aw$prog2, sum)

# Multivariable
#--------------
model.aw.m1 = glm(num_awards ~ math + prog2, data = aw, family = poisson)
summary(model.aw.m1)
#anova(model.aw.m1)
idr.display(model.aw.m1)  # both vars sig.
#anova(model.aw.m1, model.aw.u1, test = "LRT")
#anova(model.aw.m1, model.aw.u2a, test = "LRT")
poisgof(model.aw.m1)  # good fit
#with(model.aw.m1, pchisq(deviance, df.residual, lower.tail = F))
AIC(model.aw.m1) - AIC(model.aw.u1)  # reduction in AIC as compared to single var model1
AIC(model.aw.m1) - AIC(model.aw.u2a)  # reduction in AIC as compared to single var model2
# scaled Pearson chi-square test
aw$pred = model.aw.m1$fitted
cbind(obs = aw$num_awards, pred = round(aw$pred, 1))
aw$x2 = with(aw, (num_awards - pred)^2/pred)
x2p = sum(aw$x2)
s.x2 = x2p/(200-3)  # 200 cases, 3 estimated parameters = b0, b1, b2
s.x2  # close to 1, fit well

# Testing interactions
model.aw.i1 = glm(num_awards ~ math + prog2 + math*prog2, data = aw, family = poisson)
summary(model.aw.i1)
idr.display(model.aw.i1)  # interaction term not sig.
#anova(model.aw.i1, model.aw.m1, test = "LRT")
poisgof(model.aw.i1)  # model fit though...
AIC(model.aw.i1) - AIC(model.aw.m1)  # increase in AIC, M1 is better
# scaled chi-square
aw$pred.i = model.aw.i1$fitted
s.x2.i = sum(with(aw, (num_awards - pred.i)^2/pred.i))/(200-4)  # 4 est param
s.x2.i  # fit well though...
# but interaction not sig, AIC M1 better

# Conclusion
# Accept model.aw.m1
summary(model.aw.m1)
idr.display(model.aw.m1)
b1 = coef(model.aw.m1)[[2]]*10
b1.ll = confint(model.aw.m1)[[2]]*10
b1.ul = confint(model.aw.m1)[[5]]*10
exp(cbind("Math RR" = b1, "95% LL" = b1.ll, "95% UL" = b1.ul))
#aw$m10 = aw$math/10
#model.aw.m1.10 = glm(num_awards ~ m10 + prog2, data = aw, family = poisson)
#summary(model.aw.m1.10)
#idr.display(model.aw.m1.10)
poisgof(model.aw.m1)
s.x2
# scales Pearson chi-square also indicates overdispersion i.e. VAR(Y) > Mean(Y), if the X2:df ratio > 1
# some online sources noted that Residual Deviance/DF >  1 indicates overdispersion. But this cannot be verified.

# Extra, for overdispersed data
model.aw.m1.q = glm(num_awards ~ math + prog2, data = aw, family = quasipoisson)
summary(model.aw.m1.q)  # note s.x2 = Dispersion parameter in the results, ~ 1, not overdispersed
#anova(model.aw.m1.q)
# http://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant
library(AER)
dispersiontest(model.aw.m1, trafo = 1)  # test model M1, p-value not sig. not overdispersed
# poisgof, idr.display are not going to work here. Manually GOF
with(model.aw.m1.q, pchisq(deviance, df.residual, lower.tail = F))  #model fit well
cbind(coef(model.aw.m1.q), confint(model.aw.m1.q))
