/*
This Stan program defines a model for the number of checklists that a bird
appears on in a given cell. 
This model uses land cover as a predictor.

The input data is: occurrence data, a vector 'occ' of length 'N' (the 
 number of defined observations);
categorical land cover data, given by a vector of length 'N'
*/
data {
  int<lower=0> N; // length of data
  int<lower=0> K; // number of predictos
  int occ[N]; // observed outcomes
  int checklists[N]; // observed number of lists
  
  // land cover: each is coded as a dummy binary variable
  // each column is a different lc type: 
  // from L to R, water, developed, barren, forest, shrub, grassland, planted, wetlands
  matrix[N, K] lc;
}

// The parameters accepted by the model. 
parameters {
  real inter; // intercept
  vector[K] coeffs; // coefficients
  // real b_water;
  // real b_developed;
  // real b_barren;
  // real b_forest;
  // real b_shrub;
  // real b_grassland;
  // real b_planted_cultivated;
  // real b_wetlands;
}

/* The model to be estimated. We model the output
'occ' to be a binomial(n, p) with n from the data and p drawn from a 
 categorical logistic regression.*/
model {
  // priors
  to_vector(coeffs) ~ cauchy(0, 2.5); // Gelman's weakly informative prior
  // but want to make a few of these a little more informative
  coeffs[1] ~ cauchy(2, 3); // I think water coeff is positive, but making more diffuse also
  coeffs[2] ~ cauchy(0.5, 2.5); // positive development
  coeffs[4] ~ cauchy(0.5, 3); // negative forest
  
  // run a logistic regression to generate the p that controls the occurrence data
  occ ~ binomial_logit(checklists, lc*coeffs);
  
}




