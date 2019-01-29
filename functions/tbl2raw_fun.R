# Aggregate/table data to raw function
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Author: Wan Nor Arifin
#
# Function:
# tbl2raw(table_data)
#
# Description:
# - Converts data in m x n table/matrix format to data frame of x y variables.
#
# Usage:
# - Loads the function from tbl2raw_fun.R
#
#   source("tbl2raw_fun.R")
#
# - Obtain the raw data in data frame format
#   
#   tbl2raw(table_data)
#
# ^^^^^^^^^^^^^^^

tbl2raw = function(tbl_xy){
  y = rep(1:dim(tbl_xy)[2], times = margin.table(tbl_xy, 2))
  x = rep(rep(1:dim(tbl_xy)[1], dim(tbl_xy)[2]), as.numeric(tbl_xy))
  xy = data.frame(x, y)
  return(xy)
}
