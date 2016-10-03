rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('rethinking')

# Constants
nIter <- 20e3 # run length of chain

# Variables
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 't_lin', 't_squ', 't_cub',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_wdi_pop65",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa"
  )
)
d <- na.omit(d)

# Models ===================================================
# Varying intercept baseline -------------------------------
fit_interceptOnly <- map2stan(
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a + a_spell[spell_id],
    # Priors
    a ~ dnorm(0, 10),
    a_spell[spell_id] ~ dnorm(0, sigma_regime),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
save(
  fit_interceptOnly,
  file = file.path(
    pathOut, paste0('stan_interceptOnly_', Sys.Date(), '.RData')
  )
)
# precis(fit_interceptOnly)
# precis(fit_interceptOnly, depth = 2)
# plot(fit_interceptOnly)

# Vary intercept, fixed slope models -----------------------
fit_fixedGrowthCurve1 <- map2stan(            # linear trend
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a + a_spell[spell_id] + b_lin * t_lin,
    # Priors
    a ~ dnorm(0, 10),
    a_spell[spell_id] ~ dnorm(0, sigma_regime),
    b_lin ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
# precis(fit_fixedGrowthCurve1)
# precis(fit_fixedGrowthCurve1, depth = 2)
save(
  fit_fixedGrowthCurve1,
  file = file.path(
    pathOut, paste0('stan_fixedGrowthCurve1_', Sys.Date(), '.RData')
  )
)

fit_fixedGrowthCurve2 <- map2stan(           # squared trend
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a + a_spell[spell_id] + b_lin * t_lin + b_squ * t_squ,
    # Priors
    a ~ dnorm(0, 10),
    a_spell[spell_id] ~ dnorm(0, sigma_regime),
    c(b_lin, b_squ) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
# precis(fit_fixedGrowthCurve)
# precis(fit_fixedGrowthCurve, depth = 2)
save(
  fit_fixedGrowthCurve2,
  file = file.path(
    pathOut, paste0('stan_fixedGrowthCurve2_', Sys.Date(), '.RData')
  )
)

fit_fixedGrowthCurve3 <- map2stan(             # cubic trend
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a + a_spell[spell_id] +
      b_lin * t_lin + b_squ * t_squ + b_cub * t_cub,
    # Priors
    a ~ dnorm(0, 10),
    a_spell[spell_id] ~ dnorm(0, sigma_regime),
    c(b_lin, b_squ, b_cub) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
# precis(fit_fixedGrowthCurve3)
# precis(fit_fixedGrowthCurve3, depth = 2)
save(
  fit_fixedGrowthCurve3,
  file = file.path(
    pathOut, paste0('stan_fixedGrowthCurve3_', Sys.Date(), '.RData')
  )
)
compare(
  fit_interceptOnly, fit_fixedGrowthCurve1,
  fit_fixedGrowthCurve2, fit_fixedGrowthCurve3
)
rm(fit_fixedGrowthCurve1, fit_fixedGrowthCurve3)
# Fixed squared growth curve has lowest WAIC and gets all
# the weight.

# Posterior predictive checks on squared trend
post <- extract.samples(fit_fixedGrowthCurve2, n = 5000)
# fixed effect correlations?
pairs(post[c('a', 'b_lin', 'b_squ')])

# chains converged?
# plot(fit_fixedGrowthCurve2)

# Rhat == 1?
precis(fit_fixedGrowthCurve2)
precis(fit_fixedGrowthCurve2, depth = 2, pars = c('a_spell'))

# model implied dep_var ranges ok?
invert_boxcox <- function(x, lambda){ # inverse boxcox transformation
  if(lambda == 0){
    exp(x)
  } else {
    (lambda * x + 1)^(1/lambda)
  }
}
lambda <- .5             # original transformation parameter
summary(invert_boxcox(d$infant_mortality, .5))   # reference

# average intercept
sim_mu <- invert_boxcox(x = post[['a']], lambda = lambda)
dens(sim_mu)                               # no mass below 0

# regime intercepts
sim_mu <- apply(post[['a_spell']], 2, function(x){ x + post[['a']]})
sim_mu <- apply(sim_mu, 2, invert_boxcox, lambda = lambda)
summary(as.vector(sim_mu)) # all positive, no surprisingly large

alpha <- .5
plot_mu <- data.frame(   # plot uncertainty about intercepts
  x = 1:ncol(sim_mu),
  mu = apply(sim_mu, 2, mean, numeric(1)),
  lower = apply(sim_mu, 2, quantile, probs = alpha),
  upper = apply(sim_mu, 2, quantile, probs = 1-alpha)
)
ggplot(
  data = plot_mu,
  aes(
    x = reorder(x, mu, decrease = TRUE),
    y = mu, ymin = lower, ymax = upper
  )
) +
geom_errorbar()
rm(alpha, plot_mu) # all non-negative, some extremely uncertain

# plot implied trends in dep_var
sim_trend <- link(fit_fixedGrowthCurve2, n = 5000)
pdta <- data.frame(
  spell_id = d$spell_id, t_lin = d$t_lin, t(sim_trend)
)
pdta <- gather(pdta, key = draw, value = value, 3:ncol(pdta))
pdta <- within(pdta, value <- invert_boxcox(value, lambda))
pdta <- aggregate(
  value ~ spell_id + t_lin, data = pdta, FUN = quantile, probs = c(.05, .5, .95)
)
ggplot(
  data = pdta,
  aes(
    x = t_lin, y = value[, 2], ymin = value[, 1], ymax = value[, 3],
    group = spell_id
  )
) +
  geom_pointrange()
rm(pdta, sim_trend, post) # all positive & in sensible range

# add flexible growth curve --------------------------------
# Note: originally tried fully flexible growth curve, but
#   (a) mixing was extremely slow,
#   (b) ALL rhat == 1 & ALL neff == 45000 (0 autocorrelation)
# Too good to be true, assume model was misspecified
fit_ranefGrowthCurve2 <- map2stan(
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a_spell[spell_id] +
      b_lin * t_lin + b_spell_squ[spell_id] * t_squ,
    # Priors
    c(a_spell, b_spell_squ)[spell_id] ~ dmvnorm2(
      c(a, b_squ), sigma_regime, Rho
    ),
    c(a, b_lin, b_squ) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
save(
  fit_ranefGrowthCurve2,
  file = file.path(
    pathOut, paste0('stan_ranefGrowthCurve2_', Sys.Date(), '.RData')
  )
)
load(
  file.path(
    pathOut, paste0('stan_ranefGrowthCurve2_', Sys.Date(), '.RData')
  )
)
precis(fit_ranefGrowthCurve2)
precis(fit_ranefGrowthCurve2, depth = 2)
# plot(fit_ranefGrowthCurve2) # all fine

compare(
  fit_interceptOnly, fit_fixedGrowthCurve2, fit_ranefGrowthCurve2
) # varying growth curve fits best, but SE in WAIC increases
# drastically. Still estimated WAIC more than 2 SE below
# fixed effect model

# Posterior predictive checks
post <- extract.samples(fit_ranefGrowthCurve2, n = 5000)

# correlation fixed effects
pairs(post[c('a', 'b_lin', 'b_squ')])

# valid range on intercepts?
sim_mu <- invert_boxcox(x = post[['a']], lambda = lambda)
dens(sim_mu) # no mass below 0

sim_mu <- post[['a_spell']]
sim_mu <- apply(sim_mu, 2, invert_boxcox, lambda = lambda)
summary(as.vector(sim_mu)) # no mass below 0

# implied trends
sim_trend <- link(fit_ranefGrowthCurve2, n = 5000)
pdta <- data.frame(
  spell_id = d$spell_id, t_lin = d$t_lin, t(sim_trend)
)
pdta <- gather(pdta, key = draw, value = value, 3:ncol(pdta))
pdta <- within(pdta, value <- invert_boxcox(value, lambda))
summary(pdta$value)
summary(invert_boxcox(d$infant_mortality, .5))
pdta <- aggregate(
  value ~ spell_id + t_lin, data = pdta, FUN = quantile, probs = c(.05, .5, .95)
)
ggplot(
  data = pdta,
  aes(
    x = t_lin,
    y = value[, 2], ymin = value[, 1], ymax = value[, 3]
  )
) +
  geom_errorbar() +
  geom_line(colour = 'red', size = .5) + 
  facet_wrap(~ spell_id) +
  theme(
    strip.text = element_blank(), strip.background = element_blank(),
    text = element_blank(), axis.ticks = element_blank()
  )
rm(pdta, sim_trend, post) # all positive & in sensible range