library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
parallel::detectCores()
dat <- read_csv("data_new.csv") %>%
  mutate(Y = 1, j = floor((w-1)*(l/w)))
len <- nrow(dat)

dat1 <- data.frame()


for(i in 1:nrow(dat)) {
  
  temp <- matrix(rep(c(dat[[i,3]],dat[[i,4]],dat[[i,5]],dat[[i,14]]),dat[[i,2]]),nrow = dat[[i,2]],byrow = TRUE)
  
  dat1 <- bind_rows(dat1,data.frame(temp))
  
}

colnames(dat1) <- c("rtg_years","l","w","j")
len <- nrow(dat1)
fit <- stan(
  file = "stan_test.stan",
  data = list(rtg_years=dat1$rtg_years,l=dat1$l,w=dat1$w,j = dat1$j, LENGTH = len),
  warmup = 1000,
  iter = 2000,
  chains = parallel::detectCores()
)

saveRDS(fit, paste0("fit",Sys.Date(),".rds"))
