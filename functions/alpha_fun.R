# Cronbach's alpha for scale with N/A option
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Author: Wan Nor Arifin
#
# Function:
# c_alpha(data, digits = 3)
#
# Description:
# - It is meant for scales containing items with “not applicable” (N/A) option.
# - Imputation methods is not applicable because the missingness is intentional and by design.
# - Item variances are obtained for available numerical responses (1/0) per item. N/A responses are excluded.
# - The scale score is scaled-up to the number of items in the scale. This inflates the scale score for responses containing N/A, as if all items are included.
# - The variance of the adjusted scale scores is then easily calculated.
#
# Usage:
# - Loads the function from alpha_fun.R
#
#   source("alpha_fun.R")
#
# - Obtain Cronbach's alpha
#   
#   c_alpha(data)
#
# ^^^^^^^^^^^^^^^

c_alpha = function(data, digits = 3) {
  # raw scale score
  raw_scale = rowSums(data, na.rm = T)
  # k = number of items in a scale
  k = dim(data)[2]
  # adjusted scale score, scaled-up to the number of items in a scale
  adjusted_scale = k * (raw_scale / rowSums(!is.na(data)))
  
  # variances
  item_variance = mapply(var, data, na.rm = T)
  scale_variance = var(adjusted_scale, na.rm = T)
  
  # alpha
  alpha =  k / (k - 1) * (1 - sum(item_variance, na.rm = T)/scale_variance)
  
  # additional output:
  # alpha if item removed; item-scale, item-rest, and average inter-item correlations
  alpha_r = itc = irc = aic = aic_n = rep(0, k)
  itc = cor(data, adjusted_scale, use = "pairwise.complete.obs")
  n_cor = mapply(function(x) length(na.omit(x)), data)
  for(i in 1:k) {
    raw_scale_r = rowSums(data[-i], na.rm = T)
    k_r = dim(data[-i])[2]
    adjusted_scale_r = k_r * (raw_scale_r / rowSums(!is.na(data[-i])))
    item_variance_r = mapply(var, data[-i], na.rm = T)
    scale_variance_r = var(adjusted_scale_r, na.rm = T)  
    alpha_r[i] = k_r / (k_r - 1) * (1 - sum(item_variance_r, na.rm = T)/scale_variance_r)
    irc[i] = cor(data[i], adjusted_scale_r, use = "pairwise.complete.obs")
    aic[i] = mean(cor(data[i], data[-i], use = "pairwise.complete.obs"), na.rm = T)
    aic_n[i] = rowSums(!is.na(cor(data[i], data[-i], use = "pairwise.complete.obs")))
  }
  
  # output
  cat(c("Cronbach's alpha = ", round(alpha, digits), "\n\n"), sep = "")
  matrix(round(c(alpha_r, itc, irc, n_cor, aic, aic_n), digits), nrow = length(alpha_r), ncol = 6,
         dimnames = list(names(data),
                         c("Alpha w/o item", "Item-scale cor.", "Item-rest cor.", 
                           "n cor.", "Av. item-item cor", "n item")))
}
