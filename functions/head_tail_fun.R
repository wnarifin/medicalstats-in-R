# function: head & tail
# Author: Wan Nor Arifin

head_tail = function(data, row = 6) {
  cat("First", row, "rows:\n")
  print( head(data, row) )
  cat("\n")
  cat("Last", row, "rows:\n")
  print( tail(data, row) )
}