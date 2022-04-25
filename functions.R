library(tidyverse)


mu = c(1,-1)

sigma <- matrix(c(1,.5,.5,1),nrow =2)

samples <- as.data.frame(MASS::mvrnorm(n=10000, mu=mu,Sigma = sigma))

ggplot(samples)+
  geom_point(aes(x=V1,y=V2))


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
      
      f <- f + f_temp
    }
  f
}

faults(2,4,2)

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



#### Trials with functions ####




pdf_R_g(2,4,2,10,20,.0024,10,10)

lambda <- .0024
alpha <- 1
beta <- .25*1/lambda
tau <- 20

L <- 128
W <- 2
j <- 128

df <- data.frame() 

for(i in seq(from =1,to =40, by = .1)) {
  
  df <- bind_rows(df,
                  data.frame(t=i,
                             rc = R_c(t=i,tau=tau, alpha = alpha, beta = beta),
                             R_g = R_g(j,L,W,t=i,tau,lambda,alpha,beta),
                             f_t = pdf_R_g(j,L,W,t=i,tau,lambda,alpha,beta)))
  
}

ggplot(df, aes(x = t, y = f_t)) +
  geom_path() +
  geom_path(aes(y = 1-R_g),color = "red")



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

