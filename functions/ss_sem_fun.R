# Sample size calculator for SEM
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Author: Wan Nor Arifin
# Last update: 2020-07-30
#
# Functions:
# df_model(n_item, n_factor)  # df for model
# df_baseline(n_item, n_factor)  # df for baseline model -- for CFI
# cormat_equal(n_item, n_factor, fl, factor_cor)  # generate cor matrix, equal item per factor
# cormat_unequal(vector_item, fl, factor_cor)  # generate cor matrix, unequal item per factor
# ncp_calc(alpha, power, df)  # calculate non-centrality parameter given alpha, power, df
# nrmsea_calc(rmsea = 0.05, alpha, power, df)  # sample size using prespecified RMSEA
# ncfi_calc(cfi = 0.05, alpha, power, df, dfB, cormat)  # sample size using prespecified CFI
#
# Description:
# - This script includes functions to calculate df for sample size calculation.
# - Also includes functions to generate correlation matrices for equal and 
#   unequal item per factor.
# - ncp_calc() converts method to calculate ncp described in Kim (2005) for 
#   SAS in R.
# - nrmsea_calc() and ncfi_calc() calculate sample sizes given RMSEA and CFI 
#   in Kim (2005).
#
# Usage:
# - Refer to ss_sem_examples.R
#
# References:
# 1. Brown, T. A. (2015). Confirmatory factor analysis for applied research. 
#    New York: The Guilford Press.
# 2. Kim, K. H. (2005) The Relation Among Fit Indexes, Power, and Sample Size in
#    Structural Equation Modeling. Structural Equation Modeling: A 
#    Multidisciplinary Journal, 12(3), 368-390. DOI: 10.1207/s15328007sem1203_2
#
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Calculate model df
df_model = function(n_item, n_factor) {
  n_item = n_item
  n_factor = n_factor
  n_cor = n_factor*(n_factor - 1)/2
  b = n_item*(n_item + 1)/2
  a = (n_item - n_factor) + n_item + n_factor + n_cor
  df = b - a  # model df
  return(df)
}

# Calculate baseline df
df_baseline = function(n_item, n_factor) {
  n_item = n_item
  n_factor = n_factor
  b = n_item*(n_item + 1)/2
  a_ = (n_item - n_factor) + 0 + n_factor + 0  # FL & Cor = 0
  dfb = b - a_
  return(dfb)
}

# Calculate NCP given alpha, power and model df
ncp_calc = function(alpha, power, df) {
  crit = qchisq(1 - alpha, df)
  delta = round(crit - df)
  times = 1
  direc = 1
  amount = 10
  
  while (times < 9) {
    delta = delta + direc * amount
    pow = 1 - pchisq(crit, df, delta)
    if (direc * (power - pow) < 0) {
      times = times + 1
      direc = -1 * direc
      amount = amount / 10
    }
  }
  return(delta)
}

# Obtain model-implied correlation matrix, equal no of item per factor
cormat_equal = function(n_item, n_factor, fl, factor_cor) {
  n_item = n_item
  n_factor = n_factor
  fl_ = fl
  cor_ = factor_cor
  if(n_item %% n_factor != 0){
    print("Number of items must be multiples of factor!")
  } else if(n_item %% n_factor == 0) {
    # FL per factor matrix
    mat_fl = matrix(rep(0, n_item*n_factor), ncol=n_factor)
    start_loc = 1
    n_per_factor = n_item/n_factor
    for(i in 1:n_factor) {
      end_loc = start_loc + n_per_factor - 1
      mat_fl[start_loc:end_loc, i] = rep(fl_, n_per_factor)
      start_loc = end_loc + 1
    }
    # factor correlation matrix
    mat_fc = matrix(rep(cor_, n_factor^2), ncol=n_factor)
    diag(mat_fc) = 1
    # unique variance matrix
    uvar = 1 - fl_^2
    uvar = diag(uvar, n_item, n_item)
    # correlation matrix
    mat_cor = mat_fl %*% mat_fc %*% t(mat_fl) + uvar
    return(mat_cor)
  }
}

# Obtain model-implied correlation matrix, unequal no of item per factor
cormat_unequal = function(vector_item, fl, factor_cor) {
  vec_item = vector_item
  n_item = sum(vec_item)
  n_factor = length(vec_item)
  fl_ = fl
  cor_ = factor_cor
  # FL per factor matrix
  mat_fl = matrix(rep(0, n_item*n_factor), ncol=n_factor)
  start_loc = 1
  for(i in 1:n_factor) {
    end_loc = start_loc + vec_item[i] - 1
    mat_fl[start_loc:end_loc, i] = rep(fl_, vec_item[i])
    start_loc = end_loc + 1
  }
  # factor correlation matrix
  mat_fc = matrix(rep(cor_, n_factor^2), ncol=n_factor)
  diag(mat_fc) = 1
  # unique variance matrix
  uvar = 1 - fl_^2
  uvar = diag(uvar, n_item, n_item)
  # correlation matrix
  mat_cor = mat_fl %*% mat_fc %*% t(mat_fl) + uvar
  return(mat_cor)
}

# Calculate sample size given expected RMSEA
nrmsea_calc = function(rmsea = 0.05, alpha, power, df) {
  ncp = ncp_calc(alpha, power, df)
  N_e = (ncp / (rmsea^2 * df)) + 1
  return(N_e)
}

# Calculate sample size given expected CFI
ncfi_calc = function(cfi = 0.95, alpha, power, df, dfb, cormat) {
  ncp = ncp_calc(alpha, power, df)
  F_B = -log(det(cormat))
  N_cfi = (ncp + dfb*(1 - cfi)) / (F_B*(1 - cfi)) + 1
  return(N_cfi)
}
