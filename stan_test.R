library(rstan)
library(tidyverse)


#rstan::get_posterior_mean(fit)


lambda <- 1
dat <- rexp(18, lambda)
mean(dat)
len <- length(dat)

fit <- stan(
  file = "stan_test.stan",
  data = list(Y=dat, LENGTH = len),
  warmup = 750,
  iter = 1500,
  chains = 10
)

print(fit)

mcmc_chain = as.matrix(fit)



plot(density(mcmc_chain[,'lambda']))
