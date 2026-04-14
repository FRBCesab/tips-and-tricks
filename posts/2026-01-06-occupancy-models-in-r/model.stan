data {
  int<lower=1> M; // number of sites
  array[M] int nvisit; // number of visits per sites-years
  array[M] int<lower=0> n; // observations vector
}

parameters {
  real psi_logit; // value of psi on the logit scale
  real p_logit; // value of p on the logit scale
}

model {
  // 1. Priors
  target += normal_lpdf(psi_logit | 0, 3);
  target += normal_lpdf(p_logit | 0, 3);

  // 2. Variables
  int nvi;

  // 3. Model specification
  for (i in 1:M) { // iterate over sites
    nvi = nvisit[i]; // number of visits
    if (n[i] > 0) { // the species was seen
      // Update log-likelihood: species was detected | present
      target += log_inv_logit(psi_logit) + binomial_logit_lpmf(n[i] | nvi, p_logit);
    } else {
      // Update log-likelihood: species was non-detected | present
      // or non-detected | absent
      target += log_sum_exp(log_inv_logit(psi_logit) + binomial_logit_lpmf(0 | nvi, p_logit), log1m_inv_logit(psi_logit));
    }
  }
}

generated quantities {
  real<lower=0,upper=1> p = inv_logit(p_logit);
  real<lower=0, upper=1> psi = inv_logit(psi_logit);
}
