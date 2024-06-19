library(pwr)

#Set parameters
f2 = 0.1 / (1 - 0.1)  # Convert R^2 to f2 effect size
power = 0.90
sig_level = 0.05
number_of_predictors = 5  # Number of predictors

#Calculate sample size
sample_size <- pwr.f2.test(u = number_of_predictors, v = NULL, f2 = f2, sig.level = sig_level, power = power)

#Print the result
print(sample_size)
