library(rstan)
library(tidyverse)


#rstan::get_posterior_mean(fit)


#lambda <- 1
#dat <- rexp(18, lambda)
#mean(dat)


dat <- read_csv("data.csv")[3:4,] %>%
  mutate(Y = 1, j = l/2)
len <- nrow(dat)

fit <- stan(
  file = "stan_test.stan",
  data = list(rtg_years=dat$rtg_years_ind,l=dat$l,w=dat$w,j = dat$j, LENGTH = len),
  warmup = 750,
  iter = 1500,
  chains = 2
)

print(fit)

mcmc_chain = as.matrix(fit)



plot(density(mcmc_chain[,'lambda']))
plot(density(mcmc_chain[,'tau']))
plot(density(mcmc_chain[,'alpha_var']))
plot(density(mcmc_chain[,'beta_var']))
