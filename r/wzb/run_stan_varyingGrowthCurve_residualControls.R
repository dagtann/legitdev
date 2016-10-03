# Run reference level with residual controls added =========
# ==========================================================
rm(list = ls())
library('rethinking')
setwd('~/Documents/legitdev')
load('./analysis.RData')

# declarations ---------------------------------------------
# Constants
nIter <- 10000 # run length of chain
nChains <- 8
burnIn <- 3000

# Variables ------------------------------------------------
d <- subset(
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 't_lin', 't_squ', 't_cub',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    "fe_etfra"
  )
)
d <- na.omit(d)

# define model & run models --------------------------------
fit_ranefGrowthCurve2_noControls <- map2stan( # ------------
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
  data = d, iter = nIter, warmup = burnIn,
  chains = nChains, cores = nChains, verbose = FALSE
)
save(
  x = fit_ranefGrowthCurve2_noControls,
  file = './stan_fit_ranefGrowthCurve2_noControls.RData'
)
rm(fit_ranefGrowthCurve2_noControls)

fit_ranefGrowthCurve2_AddLowerControls <- map2stan( # ------
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a_spell[spell_id] +
      b_lin * t_lin + b_spell_squ[spell_id] * t_squ +
      b_grgdp * growth_mad_gdppch + b_gdp * lag_mad_gdppch +
      b_agr * lag_wdi_agrvagdp +
      b_oil * lag_ross_oil_value_2000 +
      b_gas * lag_ross_gas_value_2000,
    # Priors
    c(a_spell, b_spell_squ)[spell_id] ~ dmvnorm2(
      c(a, b_squ), sigma_regime, Rho
    ),
    c(a, b_lin, b_squ, b_grgdp, b_gdp, b_agr, b_pop65, b_oil, b_gas) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, iter = nIter, warmup = burnIn,
  chains = nChains, cores = nChains, verbose = FALSE
)
save(
  x = fit_ranefGrowthCurve2_AddLowerControls,
  file = './stan_fit_ranefGrowthCurve2_AddLowerControls.RData'
)
rm(fit_ranefGrowthCurve2_AddLowerControls)

fit_ranefGrowthCurve2_AddRegimeTypes <- map2stan( # --------
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a_spell[spell_id] +
      b_lin * t_lin + b_spell_squ[spell_id] * t_squ +
      b_grgdp * growth_mad_gdppch + b_gdp * lag_mad_gdppch +
      b_agr * lag_wdi_agrvagdp +
      b_oil * lag_ross_oil_value_2000 +
      b_gas * lag_ross_gas_value_2000 +
      b_mon * d_monarchy + b_ide * d_ideocracy + 
      b_one * d_oneparty + b_per * d_personalist +
      b_mil * d_military,
    # Priors
    c(a_spell, b_spell_squ)[spell_id] ~ dmvnorm2(
      c(a, b_squ), sigma_regime, Rho
    ),
    c(a, b_lin, b_squ, b_grgdp, b_gdp, b_agr, b_pop65, b_oil, b_gas) ~ dnorm(0, 10),
    c(b_mon, b_ide, b_one, b_per, b_mil) ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, iter = nIter, warmup = burnIn,
  chains = nChains, cores = nChains, verbose = FALSE
)
save(
  x = fit_ranefGrowthCurve2_AddRegimeTypes,
  file = './stan_fit_ranefGrowthCurve2_AddRegimeTypes.RData'
)
rm(fit_ranefGrowthCurve2_AddRegimeTypes)

fit_ranefGrowthCurve2_FullControls <- map2stan( # ----------
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a_spell[spell_id] +
      b_lin * t_lin + b_spell_squ[spell_id] * t_squ +
      b_grgdp * growth_mad_gdppch + b_gdp * lag_mad_gdppch +
      b_agr * lag_wdi_agrvagdp +
      b_oil * lag_ross_oil_value_2000 +
      b_gas * lag_ross_gas_value_2000 +
      b_mon * d_monarchy + b_ide * d_ideocracy + 
      b_one * d_oneparty + b_per * d_personalist +
      b_mil * d_military +
      b_cath * lp_catho80 + b_mus * lp_muslim80 + b_prot * lp_protmg80 +
      b_eap * d_eap + b_eca * d_eca + b_lac + d_lac + b_mena * d_mena + b_sa * d_sa +
      b_etf * fe_etfra,
    # Priors
    c(a_spell, b_spell_squ)[spell_id] ~ dmvnorm2(
      c(a, b_squ), sigma_regime, Rho
    ),
    c(a, b_lin, b_squ, b_grgdp, b_gdp, b_agr, b_pop65, b_oil, b_gas) ~ dnorm(0, 10),
    c(b_mon, b_ide, b_one, b_per, b_mil) ~ dnorm(0, 10),
    c(b_cath, b_mus, b_prot) ~ dnorm(0, 10),
    c(b_eap, b_eca, b_lac, b_mena, b_sa) ~ dnorm(0, 10),
    b_etf ~ dnorm(0, 10),
    sigma_regime ~ dcauchy(0, 2),
    sigma_e ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, iter = nIter, warmup = burnIn,
  chains = nChains, cores = nChains, verbose = FALSE
)
save(
  x = fit_ranefGrowthCurve2_FullControls,
  file = './stan_fit_ranefGrowthCurve2_FullControls.RData'
)

# housekeeping =============================================
rm(list = ls())
q('no')