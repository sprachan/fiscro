/*
WANT: define a model that takes each "slice" of data in time (pass as array?)
and fits a model to the slice. The model is a multi-logistic Binomial regression,
where the number of checklists that a bird appeared on is a Binomial(N, p) where
N is the number of checklists (from the data) and p is given by 
invlogit(beta1*precip+beta2*temp+beta3*lc+beta0)

*/
data {
  int <lower = 0> N; // length of the data
  int <lower = 0> K; // number of predictors
  array [N] int obs; // bird observations
  array [N] int lists; // lists
  array[365] int group_size; // number of observations for each day group;
  matrix[N, K] predictors; // predictors
}

// The parameters accepted by the model. 
parameters {
  matrix[365, K] coeffs; // coefficients
  real <lower = 0> sigma;
  vector[365] inter;
}

/* The model to be estimated. We model the output
'occ' to be a binomial(n, p) with n from the data and p drawn from a 
 categorical logistic regression.*/
 
model {
  /* ---------------------------
        priors
  ------------------------------ */
  // autoregressive process for coefficients. Initial priors from Gelman
  coeffs[1] ~ cauchy(0, 2.5); // initialize the first row of coefficients
  inter[1] ~ cauchy(0, 10); // initialize the first intercept
  
  for(t in 2:365){
    // infer the next day's coefficients based on the previous day's
    coeffs[t] ~ normal(coeffs[t-1], sigma); 
    inter[t] ~ normal(inter[t-1], sigma);
  }
  
  /* ---------------------------
        infer model
  ------------------------------ */
  // do this on groups of the data (grouped by day)
  int pos;
  pos = 1;
  vector[10] x;
  for (i in 1:365){
    segment(obs, pos, group_size[i]) ~ binomial_logit(segment(lists, pos, group_size[i]),
                                                      block(predictors, pos, 1,
                                                            group_size[i], K)
                                                      *to_vector(coeffs[i])+inter[i]);

    pos = pos + group_size[i];                                               
  }
}




