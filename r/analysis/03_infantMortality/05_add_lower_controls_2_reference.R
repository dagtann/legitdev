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

fit_ranefGrowthCurve2_AddLowerControls <- map2stan(
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a_spell[spell_id] +
      b_lin * t_lin + b_squ * t_squ +
      b_grgdp * growth_mad_gdppch + b_gdp * lag_mad_gdppch +
      b_agr * lag_wdi_agrvagdp + b_pop65 * lag_wdi_pop65 +
      b_oil * lag_ross_oil_value_2000 +
      b_gas * lag_ross_gas_value_2000,
    # Priors
    a_spell[spell_id] ~ dnorm(a, sigma_regime),
    c(a, b_lin, b_squ, b_grgdp, b_gdp, b_agr, b_pop65, b_oil, b_gas) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2)
  ),
  data = d, iter = nIter, warmup = 5000,
  chains = 3, cores = 3, verbose = TRUE
)
save(
  x = fit_ranefGrowthCurve2_AddLowerControls,
  file = file.path(pathOut, 'stan_fit_ranefGrowthCurve2_AddLowerControls.RData')
)
precis(fit_ranefGrowthCurve2_AddLowerControls)