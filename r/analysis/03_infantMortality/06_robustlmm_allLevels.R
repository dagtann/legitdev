# Generate robust hlm models ===============================
# Deviates on random slopes in lmer models violate normality.
# Refit models using robust methods to compare results.
# Perform simulations to check substantive effect of regime types.
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm', 'robustlmm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ', 't_cub', 't_qud',
    "dur_lin", "dur_squ", 'dur_cub',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    'fe_etfra'
  )
)
d <- na.omit(d)
rhlm_baseline <- rlmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id),
  data = d, REML = FALSE
)
rhlm_varyingControls <- update(rhlm_baseline,
  . ~ . +
  growth_mad_gdppch + lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000 # rents
)
rhlm_regimeTypes <- update(rhlm_varyingControls,
  . ~ . +
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military
)
rhlm_allControls <- update(rhlm_regimeTypes,
  . ~ . +
  lp_catho80 + lp_muslim80 + lp_protmg80 +        # religion
  fe_etfra +                      # ethnic fractionalization
  d_eap + d_eca + d_lac + d_mena + d_sa,            # region
  data = d
)

compare(hlm_regimeTypes, rhlm_regimeTypes)
summary(hlm_regimeTypes)
# Simulate model implied distributions of DV ---------------
sims <- 2000
sim_theta <- mvrnorm(
  n_sims, fixef(rhlm_regimeTypes), vcov(rhlm_regimeTypes)
)
pred_dta <- data.frame(
  intercept = 1,
  t_lin = 0,
  t_squ = 0,
  growth_mad_gdppch =       0, #mean(d$growth_mad_gdppch),
  lag_mad_gdppch =          0, #mean(d$lag_mad_gdppch),
  lag_wdi_agrvagdp =        0, #mean(d$lag_wdi_agrvagdp),
  lag_ross_oil_value_2000 = 0, #mean(d$lag_ross_oil_value_2000),
  lag_ross_gas_value_2000 = 0, #mean(d$lag_ross_gas_value_2000),
  d_monarchy    = c(1, 0, 0, 0, 0, -1),
  d_ideocracy   = c(0, 1, 0, 0, 0, -1),
  d_oneparty    = c(0, 0, 1, 0, 0, -1),
  d_personalist = c(0, 0, 0, 1, 0, -1),
  d_military    = c(0, 0, 0, 0, 1, -1)
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(sim_theta, pred_dta)
# Disregards random effects b/c intercept variance does not
# interfere with fixed effect on regimetype

# Regime type deviations from the mean ---------------------
tmp <- apply(yhat, 2, function(x){
  unscaled_x <- (.5*x + 1)^(1/.5)
  mu <- mean(unscaled_x)
  lower <- quantile(unscaled_x, .05, names = FALSE)
  upper <- quantile(unscaled_x, .95, names = FALSE)
  out <- c(mu = mu, lower = lower, upper = upper)
  return(out)
  }
)
tmp <- data.frame(t(tmp))
tmp[, 'regime_type'] <- c(
  'monarchy', 'ideocracy', 'oneparty', 'personalist',
  'military', 'electoral'
)
ggplot(
  data = tmp,
  aes(x = reorder(regime_type, mu), y = mu, ymin = lower, ymax = upper)
) +
  geom_hline(
    yintercept = (.5*mean(sim_theta[, 1]) + 1)^(1/.5),
    size = .3, linetype = 'dashed'
  ) +
  geom_pointrange()

# Difference oneparty to ideocracy -------------------------
delta <- yhat[, 3] - yhat[, 2]
delta <- (.5*delta + 1)^(1/.5)
delta_dens <- density(delta)
delta_dens <- data.frame(x = delta_dens[['x']], y = delta_dens[['y']])
delta_quan <- quantile(x = delta, probs = c(.05, .95))

ggplot() +
  geom_vline(xintercept = 0, colour = 'white') + # mask grid line
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_area(
   data = subset(
      delta_dens, x >= delta_quan[1] & x <= delta_quan[2]
    ),
    aes(x=x, y = y), stat = 'identity', fill = 'grey', alpha = .6
  ) +
  geom_line(data = delta_dens, aes(x = x, y = y))

