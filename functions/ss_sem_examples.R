# Examples for Sample Size for SEM R script

# Load the script
source("ss_sem_fun.R")

# Parameter values
alpha = 0.05
power = 0.8

n_factor = 3
n_item = 9

fl = 0.6
cor = 0.3

# model df
df = df_model(n_item, n_factor); df

# baseline df
dfb = df_baseline(n_item, n_factor); dfb

# NCP
ncp_calc(alpha, power, df)

# RMSEA
nrmsea_calc(rmsea = 0.05, alpha, power, df)

# CFI

# obtain model-implied correlation matrix, equal no of item/factor
mat_cor = cormat_equal(n_item, n_factor, fl, cor); mat_cor
ncfi_calc(0.95, alpha, power, df, dfb, mat_cor)

# obtain model-implied correlation matrix, unequal no of item/factor
# 3 factors; 4, 3 and 2 items each
mat_cor = cormat_unequal(c(4, 3, 2), fl, cor); mat_cor
ncfi_calc(0.95, alpha, power, df, dfb, mat_cor)
