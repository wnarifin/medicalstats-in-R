#------------------------------------------------------------
# npercent
# 
# Description:
# A convenient function to come out with variable name, 
# labels and n (%) side by side
#------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------

library(plyr)
n.percent = function(des) {
  x = 1; for(x in 1:dim(des)[2]) { 
    print( summarise( des, Variable = c(names(des[x]),rep("-", length(table(des[x], useNA="ifany"))-1)),
                      Label = as.data.frame(table(des[x], useNA="ifany"))$Var1,
                      n = as.data.frame(table(des[x], useNA="ifany"))$Freq,
                      Percent = 100*prop.table(table(des[x], useNA="ifany")))
           , row.names=F, digits=3 )  # side-by-side n & %
    cat("\n")
  }
}

# Usage:
# n.percent(data.frame)
# 
# Examples:
# n.percent(esoph[1:3])  # will work
# n.percent(esoph[1])  # will work
# n.percent(esoph$agegp)  # will not work
# n.percent(esoph[, 1:3])  # will not work

