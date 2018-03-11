#--------------------------------------------------------
# A Short Course on Data Analysis Using R Software (2017)
#--------------------------------------------------------
# Wan Nor Arifin
#--------------------------------------------------------
# Linear regression
#--------------------------------------------------------

# Preliminaries

## Load libraries
library(car)
library(psych)

## Load data set
salary = Salaries  # data from `car`, Salaries for Professors...
?Salaries
str(salary)
names(salary)
# View the levels of categorical variables
sapply(salary[c("rank", "discipline", "sex")], levels)

# Linear Regression

## Data exploration

### Descriptive statistics
describe(salary[c(3,4,6)])  # var 3, 4, 6 are numbers
summary(salary[c(1,2,5)])  # var 1, 2, 5 are factors
lapply(salary[c(1,2,5)], function(x) summary(x)/length(x)*100)  # in percent
# Salary by groups
describeBy(salary$salary, salary$rank)
describeBy(salary$salary, salary$discipline)
describeBy(salary$salary, salary$sex)
#lapply(salary[c(1,2,5)], function(x) describeBy(salary$salary, x))  # one line code

### Plots
multi.hist(salary[c(3,4,6)])
plot(salary)

## Univariable
str(salary)
names(salary)
# - Years since PhD,
linear.u.phd = glm(salary ~ yrs.since.phd, data = salary)
summary(linear.u.phd)
# - Years in service,
linear.u.ser = glm(salary ~ yrs.service, data = salary)
summary(linear.u.ser)
# - Rank,
linear.u.ran = glm(salary ~ rank, data = salary)
summary(linear.u.ran)
# - Discipline,
linear.u.dis = glm(salary ~ discipline, data = salary)
summary(linear.u.dis)
# - Sex,
linear.u.sex = glm(salary ~ sex, data = salary)
summary(linear.u.sex)
# - LR test
linear.u0 = glm(salary ~ 1, data = salary)
summary(linear.u0)
cat(names(salary), sep = " + ")
add1(linear.u0, scope = ~ rank + discipline + yrs.since.phd + yrs.service + sex, test = "LRT")
# - p on add that var = univar

## Multivariable
# - All
linear.m.all = glm(salary ~ rank + discipline + yrs.since.phd  + yrs.service + sex, data = salary)
summary(linear.m.all)
drop1(linear.m.all, test = "LRT")  # p on rmv that var
# - Stepwise
linear.m.step = step(linear.m.all, direction = "both")
summary(linear.m.step)
linear.m.step$anova
# - Chosen model
linear.m1 = glm(salary ~ rank + discipline + yrs.since.phd + yrs.service, data = salary)
summary(linear.m1)
# - LR test
drop1(linear.m1, test = "LRT")  # p on rmv that var

## MC
cbind(summary(linear.m1)$coefficients[,1:2])  # SE
vif(linear.m1)  # VIF

## Interaction
add1(linear.m1, scope = ~ . + rank*discipline*yrs.since.phd*yrs.service, test = "LRT")
# - two interactions: discipline:yrs.service; yrs.since.phd:yrs.service

## Revised models 
linear.m2 = glm(salary ~ rank + discipline + yrs.since.phd + yrs.service +
                  yrs.since.phd:yrs.service + discipline:yrs.service, data = salary)
summary(linear.m2)  # interractions included
vif(linear.m2)  # very large VIF
# - remove yrs.since.phd, yrs.service
linear.m1.1 = glm(salary ~ rank + discipline, data = salary)
summary(linear.m1.1)
# effect of adding them
add1(linear.m1.1, scope = ~ .  + yrs.since.phd + yrs.service, test = "LRT")
# - add yrs.since.phd
linear.m1.2 = glm(salary ~ rank + discipline + yrs.since.phd, data = salary)
summary(linear.m1.2)
# - add yrs.service
linear.m1.3 = glm(salary ~ rank + discipline + yrs.service, data = salary)
summary(linear.m1.3)
summary(linear.m1)  # too much discrepancy between model w & w/out yrs.since.phd, yrs.service
# - chosen one
linear.m3 = linear.m1.1  # salary ~ rank + discipline
summary(linear.m3)

## Residuals & Influentials
plot(linear.m3)  # all defaults 1:4
# - Normality
hist(resid(linear.m3), main = "Residuals", xlab = "Residuals", ylab = "Frequency")
plot(linear.m3, which = 2)
# - Linearity
plot(linear.m3, which = 1)  # residuals vs predicted
plot(linear.m3, which = 3)  
plot(linear.m3$residuals ~ salary$rank, ylab = "Residuals", xlab = "Rank")  # prof. variance is big
plot(linear.m3$residuals ~ salary$discipline, ylab = "Residuals", xlab = "Discipline")
# - Influentials
plot(linear.m3, which = 4)  # all D < 1
plot(linear.m3, which = 5)  # leverage < 0.5
plot(linear.m3, which = 6)
par( mfrow = c(2, 3) ); plot(linear.m3, which = 1:6)
par( mfrow = c(1, 1) )  # reset
# - May need to handle these influential cases, but beyond the context of this workshop

# - Somehow, ended up with only cat var, basically an ANOVA
summary( aov(linear.m3) )
# - But it depends on your obj. of analysis, predict / compare groups

## Final model

# - Accept linear.m3
summary(linear.m3)
library(rsq)  # R^2 for GLM
rsq(linear.m3)
# - salary ~ rank + discipline
final = cbind( salary[c("rank", "discipline", "salary")], predicted_salary = predict(linear.m3) )
final_ranked = final[order(final$rank), ]; head(final_ranked); tail(final_ranked)
# - review back levels/var
levels(salary$rank)
levels(salary$discipline)
# - if rank = "Prof", discipline = "B"
predict(linear.m3, list(rank = "Prof", discipline = "B"), se.fit = T)
head( salary[salary$rank == "Prof" & salary$discipline == "B", c("rank", "discipline", "salary")] )
mean( salary[salary$rank == "Prof" & salary$discipline == "B", "salary"] )
# - if rank = "AsstProf", discipline = "B"
predict(linear.m3, list(rank = "AsstProf", discipline = "B"), se.fit = T)
head( salary[salary$rank == "AsstProf" & salary$discipline == "B", c("rank", "discipline", "salary")] )
mean( salary[salary$rank == "AsstProf" & salary$discipline == "B", "salary"] )
