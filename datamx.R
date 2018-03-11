#--------------------------------------------------------
# A Short Course on Data Analysis Using R Software (2017)
#--------------------------------------------------------
# Wan Nor Arifin
#--------------------------------------------------------
# Data management
#--------------------------------------------------------

# Read data

library(foreign)
data.sav = read.spss("cholest.sav", to.data.frame = T)
str(data.sav)

# Subsetting

# - Selecting specific parts of data

# - Three ways
#   - $ (S3 object), @ (S4 object)  # use str() to check outputs
#   - data[ , ]
#   - subset()

## Select a variable ("column")
data.sav$age  # View "age" only
data.sav[, "age"]
data.sav[, 2]

## Select a case ("row)
data.sav[73, ]  # 73rd observation

## Select row & col
# - data[row(number/name), col(number/name)]
data.sav[1:10, 2:4]  # Row 1 to 10; col 2 to 4
data.sav[c(1,3,5,7,9), c("age", "chol")]  # Row 1,3,5,7,9; col age & chol

## Select based on logical expression
# == equal, >= more or eq, <= less or eq, > more, < less, != not equal
data.sav[data.sav["age"] == 38, ]  # Row age = 38
data.sav[data.sav["age"] == 38, c("age", "chol")]  # Row age = 38; col age & chol
data.sav[data.sav["age"] == 38, ]$chol  # $ & [] combo...
data.sav[data.sav$age == 38, c("age", "chol")]
levels(data.sav[, "sex"])  # to remind us of the levels
data.sav[data.sav$sex == "female", c("sex", "chol")]  # Row sex = female; col sex & chol
# - TASK, change == to other logical expression
# - age > 38, sex = female
data_sub = data.sav[data.sav["age"] > 38, ]
data_sub[data_sub$sex == "female", ]
data.sav[data.sav["age"] > 38 & data.sav$sex == "female", ]
# - & and, | or

# - using subset(), syntax subset(data, condition, variable)
subset(data.sav, age < 38, select = c("age", "chol"))
subset(data.sav, age < 38)$age
subset(data.sav, age < 38, select = age:sex)  # can specify column range by name
subset(data.sav, sex == "male", select = age:sex)

# - lastly, you can assign the subset to a new data object
data.sav.short = data.sav[, c(1,4)]; data.sav.short
( data.sav.short = data.sav[, c(1,4)] ) # chol & sex only
( data.sav.male = data.sav[data.sav$sex == "male", ] ) # male only
# - trick here, enclose between ( ) to view the newly assigned object


# Creating a new variable

names(data.sav); dim(data.sav)
# - age in month
data.sav$age_month = data.sav$age * 12
names(data.sav); dim(data.sav)
# - some data
data.sav$some.var = NA  # i.e. empty data
head(data.sav)
data.sav[data.sav$age > 38, ]$some.var = 2
head(data.sav); tail(data.sav)
data.sav[data.sav$age <= 38, ]$some.var = 1
head(data.sav); tail(data.sav)  # basically we're recoding the age...


# Recoding

## Numerical to Categorical
data.sav$age_cat <- cut(data.sav$age, breaks = c(-Inf,40,50,Inf),
                    labels = c("< & = 40", "41-50", "> 50"))
str(data.sav$age_cat)
levels(data.sav$age_cat)
data.sav[order(data.sav$age), c("age", "age_cat")]  # the way to sort a data frame using order()

## Numerical coding to Categorical coding
str(data.sav$some.var)
data.sav$some.var_cat = factor(data.sav$some.var, levels = c(1,2), labels = c("< or = 38", "> 38"))
str(data.sav$some.var_cat)

## Categorical to Categorical
table(data.sav$age_cat)  # view counts using table(), more on this later
# - only one observation labeled as `> 50`. We want to combine `40-49` with `> 50`.
library(car)
data.sav$age_cat1 = recode(data.sav$age_cat, "c('41-50', '> 50') = '41 & >'")
table(data.sav$age_cat1)  # combined


# Saving the modified data set
write.csv(data.sav, "datanew.csv")


# Removing a variable
str(data.sav)
data.sav[6:10] = NULL
str(data.sav)  # all newly created vars removed


# Direct data entry

# - for short data entry, usually table data

## Data frame
data_frame = read.table(header = TRUE, text = "
ID Group BMI
1 Fat 30
2 Fat 31
3 Fat 32
4 Thin 20
5 Thin 19
6 Thin 18
")
str(data_frame)
data_frame

## Table
data_table = read.table(header = FALSE, text = "
80 10
5 100
")
colnames(data_table) = c("Cancer", "No Cancer")
rownames(data_table) = c("Smoker", "Non-smoker")
str(data_table)  # still a data frame, but laid out in form of a table.
data_table


# Q&A?


# Extra

## Handle duplicates

# - let say we have this data:
data = read.table(header = T, text = "
ID age gender
8110 20 M
8110 20 M
1627 30 M
1234 23 F
4567 12 F
4567 12 F
")
str(data); data
# - Use `duplicated`, a built-in function in R
?duplicated
anyDuplicated(data)  # 2 duplicates
dupli = data[duplicated(data), "ID"]  # check for duplicated ID
dupli
data[data$ID == dupli, 1:3]  # view duplicated entry
data[data$ID != dupli, 1:3]  # view entry minus duplicated
data[!duplicated(data), "ID"]  # just the unduplicated ID
# - then you can easily save the unduplicated entry
data_clean = data[data$ID != dupli, 1:3]
write.csv(data_clean, "data_clean.csv", row.names = F)

## Handle missing (NA)
data_na = read.table(header = T, sep = ",", text = "
ID, age, gender
8110, 20, M
8110, 20, M
1627, 30, 
1234, 23, F
4567, , F
4567, 12, F
")  # we use comma separated values in this example
str(data_na); data_na
summary(data_na)  # NA in age, " " category in gender
anyNA(data_na)  # TRUE, yes there is
is.na(data_na)
data_na_clean = na.omit(data_na)
summary(data_na_clean)
data_na_clean[data_na_clean$gender != " ", ]  # exclude gender = empty
data_na_cleaner = data_na_clean[data_na_clean$gender != " ", ]
data_na_cleaner
write.csv(data_na_cleaner, "data_na_cleaner.csv", row.names = F)