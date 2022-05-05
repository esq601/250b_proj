library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())

dat <- read_csv("data.csv") %>%
  mutate(Y = 1, j = floor((w-1)*(l/w)))
len <- nrow(dat)

dat1 <- data.frame()


for(i in 1:nrow(dat)) {
  
  temp <- matrix(rep(c(dat[[i,4]],dat[[i,5]],dat[[i,6]],dat[[i,9]]),dat[[i,3]]),nrow = dat[[i,3]],byrow = TRUE)
  
  dat1 <- bind_rows(dat1,data.frame(temp))
  
}

colnames(dat1) <- c("rtg_years","l","w","j")
len <- nrow(dat1)
fit <- stan(
  file = "stan_test.stan",
  data = list(rtg_years=dat1$rtg_years,l=dat1$l,w=dat1$w,j = dat1$j, LENGTH = len),
  warmup = 750,
  iter = 1500,
  chains = 8
)

saveRDS(fit, "fit.rds")