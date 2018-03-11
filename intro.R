#--------------------------------------------------------
# A Short Course on Data Analysis Using R Software (2017)
#--------------------------------------------------------
# Wan Nor Arifin
#--------------------------------------------------------
# Getting started with R
#--------------------------------------------------------

# R & RStudio

## Installation
# - R @ https://cran.r-project.org/
# - RStudio @ http://www.rstudio.com/

# RStudio Interface

## The windows
# 1. Script
# 2. Console
# 3. Environment & History
# 4. Files & others

## Tasks
# - Set the working directory (Files)
# - Install packages a.k.a libraries (Packages)
#   - psych, car
install.packages("psych")  # by command
install.packages("car")
# - Open a new R script
#   - type all commands/functions here
#   - comments, start with "#"
#   - run all commands by Ctrl+Enter

# Function, Library, Object

## Function
# - function(), think of MS Excel function

## Library
library(psych)

## Object
# - name assigned on left side of "<-" / "="
# - variable, data (data frame, matrix, list)
x <- 1
y = 2
z = x + y
z  # type object name, you'll get the value

## Help
?psych
?library
??mean

# Read data

## Built in data sets
data()
AirPassengers
ChickWeight
sleep
abc = sleep  # view data
View(abc)
?sleep

## Reading data sets

# We have these files:
# - cholest.csv
# - cholest.sav
# - cholest.dta
# - cholest.xlsx
# Always make sure that you set the working directory first!
data.csv = read.csv("cholest.csv")  # most natural way to open data in R
library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("cholest.sav", to.data.frame = TRUE)  #SPSS
data.dta = read.dta("cholest.dta")  # STATA
library(readxl)  # library to read excel files, must install first
data.xls = read_excel("cholest.xlsx", sheet = 1)

# Handle data

## Basics
str(data.sav)  # Basic info
dim(data.sav)  # Dimension (row/case column/variable)
names(data.sav)  # Variable names

## View data
head(data.sav)  # View data, first 6 rows
tail(data.sav)  # View data, last 6 rows
data.sav  # View all
View(data.sav)  # View, graphical way

# Data structure

## The basic data types
str(data.sav)
# - numeric = numerical
# - factor = categorical
# - basically a variable in R is a VECTOR
data_num = c(1,2,3,4,5); str(data_num)
data_cat = factor( c("M", "F", "M", "F", "M") ); str(data_cat)
# - use ";" to write two lines of short commands into one

## The basic containers
# - Data frame
str(data.sav)
data.frame(data_num, data_cat)
data_frame = data.frame(data_num, data_cat); str(data_frame)
# - List
list(data_num, data_cat)
data_list = list(data_num, data_cat); str(data_list)
# - Matrix
matrix(data = c(data_num, data_cat), nrow = 5, ncol = 2)
data_matrix = matrix(data = c(data_num, data_cat), nrow = 5, ncol = 2)
data_matrix
str(data_matrix)  # shown as num

# Q&A?