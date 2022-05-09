library(rstan)
library(tidyverse)
library(patchwork)
library(rstanarm)

fit <- readRDS("fit2022-05-07.rds")
print(fit)

mcmc_chain = as.matrix(fit)

dfsum <- as.data.frame(summary(fit))

dfsum1 <- dfsum %>%
  select(`summary.mean`,`summary.2.5.`,`summary.25.`,`summary.50.`,`summary.75.`,`summary.97.5.`,`summary.Rhat`)

write.csv(dfsum1,"fit.csv")
plot(density(mcmc_chain[,'lambda']))
plot(density(mcmc_chain[,'tau']))
plot(density(mcmc_chain[,'alpha_var']))
plot(density(mcmc_chain[,'beta_var']))

chain_df <- data.frame(mcmc_chain)
lambda_quants <- quantile(chain_df$lambda,c(.025,.1,.5,.9,.975))

plt1 <- ggplot(chain_df)+
  geom_density(aes(x = lambda), fill =ggsci::pal_jama()(2)[1],alpha = .75) +
  scale_x_continuous(limits = c(min(chain_df$lambda),max(chain_df$lambda)),breaks = lambda_quants) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(
    title = "Posterior Density for Lambda"
  )


plt2 <- ggplot(chain_df) +
  geom_segment(aes(y=0,yend=0,x=lambda_quants[[1]],xend=lambda_quants[[5]]),
               size = 1) +
  geom_segment(aes(y=0,yend=0,x=lambda_quants[[2]],xend=lambda_quants[[4]]),
               size = 2.5, color = ggsci::pal_jama()(2)[2]) +
  scale_x_continuous(limits = c(min(chain_df$lambda),max(chain_df$lambda)),breaks = round(as.vector(lambda_quants),digits = 5)) +
  geom_point(aes(y=0,x=lambda_quants[[3]]), size = 5,color=ggsci::pal_jama()(4)[4]) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(size = 16)
  ) 

plt1/plt2 +
  plot_layout(heights = c(20,1))

ggsave("plot1.png",dpi = 320, width = 12, height = 6)

tau_quants <- quantile(chain_df$tau,c(.025,.1,.5,.9,.975))

plt1 <- ggplot(chain_df)+
  geom_density(aes(x = tau), fill =ggsci::pal_jama()(2)[1],alpha = .75) +
  scale_x_continuous(limits = c(min(chain_df$tau),max(chain_df$tau)),breaks = tau_quants) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(
    title = "Posterior Density for Tau"
  )


plt2 <- ggplot(chain_df) +
  geom_segment(aes(y=0,yend=0,x=tau_quants[[1]],xend=tau_quants[[5]]),
               size = 1) +
  geom_segment(aes(y=0,yend=0,x=tau_quants[[2]],xend=tau_quants[[4]]),
               size = 2.5, color = ggsci::pal_jama()(2)[2]) +
  scale_x_continuous(limits = c(min(chain_df$tau),max(chain_df$tau)),breaks = round(as.vector(tau_quants),digits = 1)) +
  geom_point(aes(y=0,x=tau_quants[[3]]), size = 5,color=ggsci::pal_jama()(4)[4]) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(size = 16)
  ) 

plt1/plt2 +
  plot_layout(heights = c(20,1))

ggsave("plot2.png",dpi = 320, width = 12, height = 6)

alpha_var_quants <- quantile(chain_df$alpha_var,c(.025,.1,.5,.9,.975))

plt1 <- ggplot(chain_df)+
  geom_density(aes(x = alpha_var), fill =ggsci::pal_jama()(2)[1],alpha = .75) +
  scale_x_continuous(limits = c(min(chain_df$alpha_var),max(chain_df$alpha_var)),breaks = alpha_var_quants) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(
    title = "Posterior Density for Alpha"
  )


plt2 <- ggplot(chain_df) +
  geom_segment(aes(y=0,yend=0,x=alpha_var_quants[[1]],xend=alpha_var_quants[[5]]),
               size = 1) +
  geom_segment(aes(y=0,yend=0,x=alpha_var_quants[[2]],xend=alpha_var_quants[[4]]),
               size = 2.5, color = ggsci::pal_jama()(2)[2]) +
  scale_x_continuous(limits = c(min(chain_df$alpha_var),max(chain_df$alpha_var)),breaks = round(as.vector(alpha_var_quants),digits = 2)) +
  geom_point(aes(y=0,x=alpha_var_quants[[3]]), size = 5,color=ggsci::pal_jama()(4)[4]) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(size = 16)
  ) 

plt1/plt2 +
  plot_layout(heights = c(20,1))

ggsave("plot3.png",dpi = 320, width = 12, height = 6)

beta_var_quants <- quantile(chain_df$beta_var,c(.025,.1,.5,.9,.975))

plt1 <- ggplot(chain_df)+
  geom_density(aes(x = beta_var), fill =ggsci::pal_jama()(2)[1],alpha = .75) +
  scale_x_continuous(limits = c(min(chain_df$beta_var),max(chain_df$beta_var)),breaks = beta_var_quants) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(
    title = "Posterior Density for Beta"
  )


plt2 <- ggplot(chain_df) +
  geom_segment(aes(y=0,yend=0,x=beta_var_quants[[1]],xend=beta_var_quants[[5]]),
               size = 1) +
  geom_segment(aes(y=0,yend=0,x=beta_var_quants[[2]],xend=beta_var_quants[[4]]),
               size = 2.5, color = ggsci::pal_jama()(2)[2]) +
  scale_x_continuous(limits = c(min(chain_df$beta_var),max(chain_df$beta_var)),breaks = round(as.vector(beta_var_quants),digits = 2)) +
  geom_point(aes(y=0,x=beta_var_quants[[3]]), size = 5,color=ggsci::pal_jama()(4)[4]) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(size = 16)
  ) 

plt1/plt2 +
  plot_layout(heights = c(20,1))

ggsave("plot4.png",dpi = 320, width = 12, height = 6)

#ggsci::pal_jama()(1)

stan_diag(fit)
stan_ess(fit)
stan_dens(fit)
stan_mcse(fit)
stan_par(fit, par = 'lambda')
stan_ac(fit)
gg1 <- stan_plot(fit, pars = c("lambda","tau","alpha_var","beta_var"))


stan_plot(fit, pars = c("lambda"), point_est = "mean") +
  theme(
    panel.grid.major  = element_blank()
  )
exp(data.frame(mcmc_chain)$lp_)
stan_plot(fit, pars = c("tau"))
stan_rhat(fit)
stan_scat(fit, pars = c("tau","lambda"))
stan_scat(fit, pars = c("tau","alpha_var"))

rstan::get_posterior_mean(fit)




lambda <- .0008
alpha <- 1.13
beta <- 150.22
tau <- 20.26

L <- 208
W <- 2
j <- L/2

df <- data.frame() 

for(i in seq(from =1,to =50, by = .25)) {
  
  df <- bind_rows(df,
                  data.frame(t=i,
                             rc = R_c(t=i,tau=tau, alpha = alpha, beta = beta),
                             R_g = R_g(j,L,W,t=i,tau,lambda,alpha,beta),
                             f_t = pdf_R_g(j,L,W,t=i,tau,lambda,alpha,beta)))
  
}
df1 <- df %>%
  mutate(R_g = 1-R_g, r_c = 1-rc) %>%
  select(-rc) %>%
  pivot_longer(-t) %>%
  mutate(cum = case_when(
    name == "f_t" ~ "PDF",
    T ~ "CDF"
  ))

ggplot(df1, aes(x = t, y = value, color = name)) +
  geom_path(size = 2) +
  geom_vline(aes(xintercept = 17)) +
  theme_minimal() +
  facet_wrap(~cum,ncol=1,scales = "free_y") +
  ggsci::scale_color_jama() +
  labs(
    title = "Reliability Model Density Functions",
    subtitle = paste("L:",L,"W:",W,"Lambda:",lambda,"Tau:",tau,"Alpha:",alpha,"Beta:",beta),
    color = "Type",
    x = "t (years)"
  ) +
  theme(
    text = element_text(size=16),
    plot.background = element_rect(fill="white"),
    axis.title.y = element_blank()
  )

ggsave("densplot.png",dpi = 320, width = 12, height =6)



### with params ####

mcmc_chain_df <- as.data.frame(mcmc_chain)
L <- 208
W <- 2
j <- L/2
mcmc_chain_df[1,2]
for(i in 1:nrow(mcmc_chain_df)) {
  
  for(k in 1:30){
    mcmc_chain_df[i,6+k] <- R_g(j,L,W,k,mcmc_chain_df[i,2],mcmc_chain_df[i,1],mcmc_chain_df[i,3],mcmc_chain_df[i,4])
  }
  
  
  
  
}



summary(mcmc_chain_df$V7)
ggplot(mcmc_chain_df) +
  geom_histogram(aes(x=V7))


mcmc_chain_df <- mcmc_chain_df %>%
  select(-pred)

cordf <- cor(mcmc_chain_df)

corrplot::corrplot(cordf)

ggplot(mcmc_chain_df) +
  geom_point(aes(x=lp__, y = V7))

names <-paste0(seq(1:30))


reldf <- mcmc_chain_df %>%
  select(7:36) %>%
  rename_all(~names) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(mean_val = mean(value),
            med_val = median(value),
            stdev = sqrt(var(value)),
            `quant_2.5` = quantile(value,.025),
            quant_10 = quantile(value,.1),
            quant_90 = quantile(value,.9),
            `quant_97.5` = quantile(value,.975)) %>%
  mutate(year = as.numeric(name)) %>%
  ungroup() %>%
  arrange(year)

ggplot(reldf,aes(x=year)) +
  geom_path(aes(y=med_val)) +
  geom_ribbon(aes(ymin=quant_10,ymax=quant_90), color ="transparent",
              fill = "darkblue", alpha = 0.5) +
  geom_ribbon(aes(ymin=`quant_2.5`,ymax=`quant_97.5`), color ="transparent",
              fill = "darkblue", alpha = 0.25) +
  geom_vline(aes(xintercept = 17))
  scale_y_continuous(limits = c(0,1))
