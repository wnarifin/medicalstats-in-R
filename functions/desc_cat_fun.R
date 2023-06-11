# function: descriptive for categorical variables
# Author: Wan Nor Arifin
# Description
# - A convenient function to come out with n (%) side by side
# - Return a list of data frame(s)
# - Can select to display percent or proportion
# - only accept data frame as input

desc_cat = function(data, percent = TRUE) {
  list = vector("list", ncol(data))
  for (i in 1:ncol(data)) {
    n = as.data.frame(table(data[i]))
    prop = as.data.frame(prop.table(table(data[i])))
    var_name = c(names(data[i]), rep("-", length(table(data[i]))-1))
    df = data.frame(Variable = var_name,
                     Label = n[1],
                     n = n$Freq)
    if(!percent) {df$Proportion = prop$Freq} else {df$Percent = prop$Freq*100}
    list[[i]] = df
  }
  names(list) = names(data)
  return(list)
}

# Examples:
# library(foreign)
# data = read.spss("https://wnarifin.github.io/data/cholest.sav", T, T)
# desc_cat(data[4:5])
# desc_cat(data[4:5], percent = T)
# desc_data = desc_cat(data[4:5])
# desc_data$categ
