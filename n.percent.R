# n.percent
# A convenient function to come out with n ($) side by side
# Author: Wan Nor Arifin
library(plyr)
n.percent = function(des) {
  x = 1; for(x in 1:dim(des)[2]) { 
    print( summarise( des, Variable = c(names(des[x]),rep("-", length(table(des[x], useNA="ifany"))-1)),
                      Label = as.data.frame(table(des[x], useNA="ifany"))$Var1,
                      n = as.data.frame(table(des[x], useNA="ifany"))$Freq,
                      Proportion = prop.table(table(des[x], useNA="ifany")))
           , row.names=F, digits=3 )  # side-by-side n & %
    cat("\n")
  }
}