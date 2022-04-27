//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.

functions{
  
  real faults_fun(int i,int L,int W,real i_r, real W_r) {
    real f;
    real flr;
    int flr_int;
    real f_temp;
    
    f = 0;
    flr_int = 0;
    flr = i_r/W_r;
    
    while(flr_int < flr){
      flr_int += 1;
    }
    
    flr_int -= 1;
    
    for(k in 0:flr_int){
      
      f_temp = ((-1)^k)*(choose(L*W-k*W,i-k*W))*(choose(L,k));
      
      f = f + f_temp;
    }
    
    return f;
  }
  
  real firstpart(real t,real tau,real beta,real alpha){
    real part1;
    part1 = ((t-tau)/beta)^(alpha-1);
    return part1;
  }
  
  real secondpart(real lambda,real tau,real t,real alpha,real beta){
    real part2;
    part2 = exp(-lambda*tau-((t-tau)/beta)^alpha);
    return part2;
  }
  
  real rtg_lpdf(real j,real L,real W,real t,real tau,
    real lambda,real alpha,real beta){
      
    real i_r;
    real W_r;
    real p;
    real m;
    real p_temp;
    real fault_num;
    int j_rd;
    int L_rd;
    int W_rd;
    
    p = 0;
    m = L*W;
    
    j_rd = 0;
    L_rd = 0;
    W_rd = 0;
    
    while(j_rd < j) {
      j_rd += 1;
    }
    
    while(L_rd < j) {
      L_rd += 1;
    }
    
    while(W_rd < j) {
      W_rd += 1;
    }
    
    if(t <= tau){
      for(i in 0:j_rd) {
        
        i_r = i * 1.0;
        W_r = W * 1.0;
        
        fault_num = faults_fun(i,L_rd,W_rd,i_r,W_r);
        
        p_temp =  fault_num * -((i*lambda*((1-exp(-t*lambda))^(i-1))*exp(lambda*t*(-(m-i))-lambda*t)) - (lambda*(m-i)*((1-exp(-t*lambda))^i) * exp(lambda*t*(-(m-i)))));
        
        p = p + p_temp;
      }
    } else {
      for(i in 0:j_rd) {
        i_r = i * 1.0;
        W_r = W * 1.0;
        
        fault_num = faults_fun(i,L_rd,W_rd,i_r,W_r);
        
        p_temp = fault_num * ((alpha*(m-i) * firstpart(t,tau,beta,alpha)*(1-secondpart(lambda,tau,t,alpha,beta))^i *((secondpart(lambda,tau,t,alpha,beta)^(m-i))))/beta - (alpha* i * firstpart(t,tau,beta,alpha)*(1-(secondpart(lambda,tau,t,alpha,beta)))^(i-1) *((secondpart(lambda,tau,t,alpha,beta)^(m-i+1))))/beta);
        p = p + p_temp;
      }
    }
    return p;
  }

  
}


data {
  int LENGTH;
  vector[LENGTH] Y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> lambda;
  real<lower=0> tau;
  real<lower=0> alpha;
  real<lower=0> beta;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real alpha_a;
  real alpha_b;
  real beta_a;
  real beta_b;
  real tau_a;
  real tau_b;
  real lambda_a;
  real lambda_b;
  
  tau_a = 20.0;
  tau_b = 1.0;
  lambda_a = .01;
  lambda_b = .001;
  alpha_a = 0.5;
  alpha_b = 2.0;
  beta_a = 50;
  beta_b = 0.125;
  
  lambda ~ uniform(lambda_a,lambda_b);
  tau ~ gamma(tau_a,tau_b);
  alpha ~ uniform(alpha_a,alpha_b);
  beta ~ gamma(beta_a,beta_b);
  
  Y ~ rtg_lpdf(lambda);
}

generated quantities{
  real pred;
  pred = exponential_rng(lambda);
}
