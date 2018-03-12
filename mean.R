#------------------------------------------------------------
# Comparing means
#------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------

# Two independent samples

## Independent t-test
library(foreign)
library(psych)
cholest = read.spss("cholest.sav", to.data.frame = T)
str(cholest); head(cholest)
histBy(cholest, "chol", group = "sex")
boxplot(chol ~ sex, data = cholest)
t.test(chol ~ sex, data = cholest)
#?t.test  # other options

## Mann-Whitney U test / Wilcoxon rank-sum test
wilcox.test(chol ~ sex, data = cholest)  # not accurate for ties
#?wilcox.test
library(coin)
wilcox_test(chol ~ sex, data = cholest)
wilcox_test(chol ~ sex, data = cholest, distribution = "exact")
#?wilcox_test

# Two dependent samples

## Paired t-test
sbp = read.spss("sbp.sav", to.data.frame = T)
t.test(sbp$S1, sbp$S2, paired = T)

## Wilcoxon signed-rank test
wilcox.test(sbp$S1, sbp$S2, paired = T)
wilcoxsign_test(sbp$S1 ~ sbp$S2)
wilcoxsign_test(sbp$S1 ~ sbp$S2, distribution = "exact")

# Independent samples

## One-way ANOVA
histBy(cholest, "chol", group = "categ")
boxplot(chol ~ categ, data = cholest)
aov_chol = aov(chol ~ categ, data = cholest)
summary(aov_chol)
TukeyHSD(aov_chol)
pairwise.t.test(cholest$chol, cholest$categ, p.adj = "bonferroni")
library(multcomp)  # has to explore first
aov_mult = glht(aov_chol, linfct = mcp(categ = "Tukey"))
summary(aov_mult)
confint(aov_mult)

## Kruskal-Wallis test
kruskal.test(chol ~ categ, data = cholest)
pairwise.wilcox.test(cholest$chol, cholest$categ, p.adj = "bonferroni")

# Dependent samples

## Repeated measures ANOVA
library(car)
time = ordered(rep(1:3))
idesign = data.frame(time)
model_rm = lm(cbind(S1, S2, S3) ~ 1, sbp)
aov_rm = Anova(model_rm, idata = idesign, idesign =~ time)
summary(aov_rm, multivariate = F)  # univariate approach
summary(aov_rm)  # multivariate approach

## Friedman test
friedman.test( as.matrix(sbp[, c("S1", "S2", "S3")]) )
