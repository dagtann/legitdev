# Generate & compare baseline candidate models =============
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'absolute_poverty', 'spell_id',
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
hlm_varyingControls <- lmer(
  absolute_poverty ~ t_lin + t_squ + (t_lin | spell_id) +
  growth_mad_gdppch + lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000, # rents
  data = d, REML = FALSE
)
hlm_regimeTypes <- update(
  hlm_varyingControls, . ~ . +
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military
)
anova(hlm_varyingControls, hlm_regimeTypes)
summary(hlm_regimeTypes)
hlm_allControls <- update(hlm_regimeTypes,
  . ~ . +
  lp_catho80 + lp_muslim80 + lp_protmg80 +
  d_eap + d_eca + d_lac + d_mena + d_sa +
  fe_etfra
)
anova(hlm_varyingControls, hlm_regimeTypes, hlm_allControls)
summary(hlm_allControls)

# Simulate model implied distributions of DV ---------------
n_sims <- 2000
n_spells <- length(unique(d$spell_id))
n_coef <- length(fixef(hlm_regimeTypes))
pred_dta <- cbind(
  model.matrix(hlm_regimeTypes),
  spell_id = as.numeric(d$spell_id)
)
beta_hat <- array(
  data = numeric(1),
  c(n_spells, n_sims, n_coef),
  dimnames = list(
    as.character(unique(pred_dta[, 'spell_id'])),
    paste0('sim', 1:n_sims),
    paste0('beta', 1:n_coef)
  )
)

sim_theta <- sim(hlm_regimeTypes, n_sims)
# plot predicted distributions over dep_var ----------------
dim(beta_hat)
for(i in 1:n_spells){
  beta_hat[i, , ] <- cbind(
      sim_theta@fixef[, 1:2] + sim_theta@ranef[['spell_id']][, i, ]
      ,
      sim_theta@fixef[, 3:ncol(sim_theta@fixef)]
  )
}
y_hat <- lapply(
  unique(pred_dta[, 'spell_id']),
  function(x){
    # cat(paste0(x,'\n'))
    d <- pred_dta[pred_dta[, 'spell_id'] == x, 1:(ncol(pred_dta)-1)]
    if(class(d) == "matrix"){              # some only 1 obs
      y_hat <- tcrossprod(beta_hat[as.character(x), , ], d)
    } else {
      y_hat <- beta_hat[as.character(x), , ] %*% d
    }
    y_hat <- y_hat + rnorm(n_sims, 0, sim_theta@sigma)
    return(y_hat)
  }
)
names(y_hat) <- as.character(unique(pred_dta[, 'spell_id']))
y_hat <- do.call(cbind, y_hat)

plot(
  density(d[, 'absolute_poverty']),
  type = 'n', xlim = c(-10, 10), ylim = c(0, .3),
  main = paste0("Fit from simulated data")
)
for(i in 1:n_sims){
  lines(density(y_hat[i, ]), col = scales::alpha('black', 0.2))
}
lines(density(d$absolute_poverty), col = 'red')
lines(density(predict(hlm_regimeTypes)), col = 'green')
legend(
  x = 'topright',
  legend = c('Observed', 'Fitted', 'Simulated'),
  col = c('red', 'green', 'black'), pch = 19
)

# plot fitted regime type effects
pred_dta <- data.frame(
  intercept = 1,
  t_lin = 0, t_squ = 0,
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
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
dim(yhat)
boxplot(yhat); abline(h = mean(fixef(sim_theta)[, 1]))
tmp <- apply(yhat, 2, function(x){
  unscaled_x <- boot::inv.logit(x)#(.5*x + 1)^(1/.5)
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
geom_pointrange() #+
#geom_hline(yintercept = (.5*mean(fixef(sim_theta)[, 1]) + 1)^(1/.5))
