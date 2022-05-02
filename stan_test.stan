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
  
  real big_choose(int n, int k) {
    real big;
    //big = 1.0*tgamma(n+1)/(tgamma(k+1)*(tgamma(n-k+1)));
    big = exp(lchoose(n,k));
    //print(big);
    return big;
  }
  
  real faults_fun(int i,int L,int W,real i_r, real W_r) {
    real f;
    real flr;
    int flr_int;
    real f_temp;
    
    f = 0;
    flr_int = 0;
    flr = i_r/W_r;
    
    while(flr_int <= flr){
      flr_int += 1;
    }
    
    flr_int -= 1;
    //print(W)
    for(k in 0:flr_int){
      //print(k)
      //print(flr_int," ", i," ", k," ", W," ",L*W-k*W," ",i-k*W)
      //print("input"," ",tgamma(313.0));
      //f_temp = ((-1)^k)*(choose(L*W-k*W,i-k*W))*(choose(L,k));
      f_temp = ((-1)^k)*(big_choose(L*W-k*W,i-k*W))*(big_choose(L,k));
      
      f = f + f_temp;
      //print(i, " ",f_temp, " ", f);
    }
    //print(f);
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
  
  real rtg_lpdf(real t, real j, real L, real W, real tau, real lambda, real alpha_val, real beta_val){
      
    real i_r;
    real W_r;
    real m;
    real p;
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
    
    while(j_rd <= j) {
      j_rd += 1;
    }
    
    j_rd -= 1;
    
    while(L_rd <= L) {
      L_rd += 1;
    }
    
    L_rd -= 1;
    
    while(W_rd <= W) {
      
      W_rd += 1;
    }
    
    W_rd -= 1;
    
    if(t <= tau){
      for(i in 0:j_rd) {
        
        i_r = i * 1.0;
        W_r = W * 1.0;
        
        fault_num = faults_fun(i,L_rd,W_rd,i_r,W_r);
        //print(fault_num)
        p_temp =  fault_num * -((i*lambda*((1-exp(-t*lambda))^(i-1))*exp(lambda*t*(-(m-i))-lambda*t)) - (lambda*(m-i)*((1-exp(-t*lambda))^i) * exp(lambda*t*(-(m-i)))));
        
        if(p_temp > 0){
          p = p + log(p_temp);
        }
        
        //print(p);
        
      }
    } else {
      for(i in 0:j_rd) {
        i_r = i * 1.0;
        W_r = W * 1.0;
        
        fault_num = faults_fun(i,L_rd,W_rd,i_r,W_r);
        
        p_temp = fault_num * ((alpha_val*(m-i) * firstpart(t,tau,beta_val,alpha_val)*(1-secondpart(lambda,tau,t,alpha_val,beta_val))^i *((secondpart(lambda,tau,t,alpha_val,beta_val)^(m-i))))/beta_val - (alpha_val* i * firstpart(t,tau,beta_val,alpha_val)*(1-(secondpart(lambda,tau,t,alpha_val,beta_val)))^(i-1) *((secondpart(lambda,tau,t,alpha_val,beta_val)^(m-i+1))))/beta_val);
        if(p_temp > 0){
          p = p + log(p_temp);
        }
      }
    }
    //print(p)
    return p;
  }

  
}


data {
  int LENGTH;
  vector[LENGTH] j;
  vector[LENGTH] l;
  vector[LENGTH] w;
  vector[LENGTH] rtg_years;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper = .004> lambda;
  real<lower=20, upper = 30> tau;
  real<lower=1> alpha_var;
  real<lower=25> beta_var;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // real alpha_a;
  // real alpha_b;
  // real beta_a;
  // real beta_b;
  // real tau_a;
  // real tau_b;
  // real lambda_a;
  // real lambda_b;
  // 
  // tau_a = 15.0;
  // tau_b = 25.0;
  // lambda_a = .001;
  // lambda_b = .01;
  // alpha_a = 10;
  // alpha_b = 1.5;
  // beta_a = 50;
  // beta_b = 0.125;
  // 
  // lambda ~ uniform(lambda_a,lambda_b);
  // tau ~ uniform(tau_a,tau_b);
  // alpha_var ~ gamma(alpha_a,alpha_b);
  // beta_var ~ gamma(beta_a,beta_b);
  //print("test", " ",lambda, " ",tau, " ",alpha_var, " ",beta_var);
  for(t in 1:LENGTH){
    
    rtg_years[t] ~ rtg(j[t], l[t], w[t], tau, lambda, alpha_var, beta_var);
  }
  
}

generated quantities{
  real pred;
}
