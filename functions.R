library(tidyverse)



R_c <- function(t=0,tau=20,lambda=.0024,alpha=1,beta=1) {
  if(t <= tau) {
    
    exp(-lambda*t)
  } else {
    
    exp(-(lambda*tau + ((t-tau)/beta)^alpha))
  }
}

faults <- function(i,L,W) {
  f <- 0
  
    for(k in 0:floor(i/W)){
      
      f_temp <- ((-1)^k)*(choose(L*W-k*W,i-k*W))*(choose(L,k))
      #print(choose(L*W-k*W,i-k*W))
      f <- f + f_temp
    }
  f
}

faults(120,286,2)

R_g <- function(j,L,W,t,tau,lambda,alpha,beta){
  rel <- 0
  
  for(i in 0:j) {
    rel_temp <- faults(i,L,W) * R_c(t,tau,lambda,alpha,beta)^(L*W-i) *(1-R_c(t,tau,lambda,alpha,beta))^i
    rel <- rel + rel_temp
  }
  rel
}

firstpart <- function(t,tau,beta,alpha){
  ((t-tau)/beta)^(alpha-1)
}

secondpart <- function(lambda,tau,t,alpha,beta){
  exp(-lambda*tau-((t-tau)/beta)^alpha)
}

pdf_R_g <- function(j,L,W,t,tau,lambda,alpha,beta){
  
  p <- 0
  m <- L*W
  
  if(t <= tau) {
    for(i in 0:j) {
      
      p_temp <- faults(i,L,W) * -((i*lambda*((1-exp(-t*lambda))^(i-1))*exp(lambda*t*(-(m-i))-lambda*t)) - (lambda*(m-i)*((1-exp(-t*lambda))^i) * exp(lambda*t*(-(m-i)))))
      #print(p_temp)
      p <- p + p_temp
    }
    
  } else {
    for(i in 0:j) {
      
      p_temp <- faults(i,L,W) * ((alpha*(m-i) * firstpart(t,tau,beta,alpha)*(1-secondpart(lambda,tau,t,alpha,beta))^i *((secondpart(lambda,tau,t,alpha,beta)^(m-i))))/beta - 
                                   (alpha* i * firstpart(t,tau,beta,alpha)*(1-(secondpart(lambda,tau,t,alpha,beta)))^(i-1) *((secondpart(lambda,tau,t,alpha,beta)^(m-i+1))))/beta)
      p <- p + p_temp
    }
  }
  p
  
}

pdf_R_g(143,286,2,10,20,.0024,1.52,30)

#### Trials with functions ####




pdf_R_g(2,4,2,10,20,.0024,10,10)

lambda <- .0008
alpha <- 1.13
beta <- 150.22
tau <- 20.26

L <- 208
W <- 2
j <- L/2

df <- data.frame() 

for(i in seq(from =1,to =60, by = .25)) {
  
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
    title = "Density Functions",
    subtitle = paste("L:",L,"W:",W,"Lambda:",lambda,"Tau:",tau,"Alpha:",alpha,"Beta:",beta),
    color = "Type",
    x = "t (years)"
  ) +
  theme(
    text = element_text(size=16),
    plot.background = element_rect(fill="white"),
    axis.title.y = element_blank()
  )
  
ggsave("densplot.png",dpi=320,height = 6,width = 8)



dftest <- df %>%
  
  mutate(int = .1*f_t)

sum(dftest$int) # Should integrate to R_g(max(t))

alp <- 3
bet <- 20
tau <- 20
df <- data.frame()
for(i in seq(from =1,to = 50, by = .1)) {

  df <- bind_rows(df,data.frame(t=i,rc = R_c(t=i,tau=tau, alpha = alp, beta = bet),R_g = R_g(64,120,2,t=i,tau,.0024,alp,bet)))
    
}



ggplot(df) +
  geom_path(aes(x=t, y= R_g)) +
  geom_path(aes(x=t, y = rc), color = "red") +
  scale_y_continuous(limits = c(0,1))

