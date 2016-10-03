library('rstan'); library('rethinking')
d <- subset(
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 't_lin', 't_squ', 't_cub'
  )
)
d <- na.omit(d)

stanDat <- list( # data for STAN
  spell_id = as.integer(d$spell_id),
  infant_mortality = d$infant_mortality,
  t_lin = d$t_lin, t_squ = d$t_squ, t_cub = d$t_cub,
  N = nrow(d), J = nlevels(d$spell_id))
)

stanModel <- "
  data {
    int<lower = 1> N;
    real infant_mortality[N];
    real <lower = -1, upper = 1> t_lin[N];
    real <lower = -1, upper = 1> t_squ[N];
    real <lower = -1, upper = 1> t_cub[N];
    int <lower = 1> J;
    int <lower = 1, upper = J> spell_id[N];
  }
  
  parameters {
    vector[4] beta;
    vector[J] u;
    real<lower = 0> sigma_e;
    real<lower = 0> sigma_u;
  }
  
  model {
    real mu;
    // priors
    u ~ normal(0, sigma_u);
    // likelihood
    for(i in 1:N){
      mu = beta[1] + u[spell_id[i]] + 
        beta[2] * t_lin[i] + beta[3] * t_squ[i] +
        beta[4] * t_cub[i];
      infant_mortality[i] ~ lognormal(mu, sigma_e);
    }
  }
"

ranIntFit <- stan(
  model_code = stanModel, data = stanDat, warmup = 2000,
  iter = 16e3, thin = 10,
  chains = 4, cores = 3
)
precis(ranIntFit)
help(package = rethinking)
print(ranIntFit, probs = c(.025, .5, .975))
traceplot(ranIntFit, pars = 'u')