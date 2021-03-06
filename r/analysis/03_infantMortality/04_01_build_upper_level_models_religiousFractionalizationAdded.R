# Generate & compare baseline candidate models =============
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){ library(i, character.only = TRUE) }

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ',
    "lag_mad_gdppch", "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military",
    "refra", 'fe_etfra'
  )
)
d <- na.omit(d)
d <- within(d, {
  fe_etfra_lin <- fe_etfra/10
  fe_etfra_squ <- fe_etfra_lin^2
  }
)

# Model objects --------------------------------------------
hlm_growthCurve <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id),
  data = d, REML = FALSE
)
summary(hlm_growthCurve)
confint(hlm_growthCurve, level = .9)

hlm_varyingControls <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
  lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000, # rents
  data = d, REML = FALSE
)
summary(hlm_varyingControls)
confint(hlm_varyingControls, level = .9)

hlm_regimeTypes <- update(
  hlm_varyingControls, . ~ . +
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military
)
confint(hlm_regimeTypes, level = .9)
anova(hlm_varyingControls, hlm_regimeTypes)
summary(hlm_regimeTypes)

hlm_allControls <- update(hlm_regimeTypes,
  . ~ . + refra + fe_etfra
)
hlm_allControls2 <- update(hlm_regimeTypes,
  . ~ . + refra + fe_etfra_lin + fe_etfra_squ
)
anova(hlm_varyingControls, hlm_regimeTypes, hlm_allControls)
summary(hlm_allControls); summary(hlm_allControls2)
# identical substantive implications
# accept mild misspecification for parsominy
confint(hlm_allControls, level = .9)

# Simulate model implied distributions of DV ---------------
n_sims <- 2000
n_spells <- length(unique(d$spell_id))
n_coef <- length(fixef(hlm_allControls))
pred_dta <- cbind(
  model.matrix(hlm_allControls),
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

sim_theta <- sim(hlm_allControls, n_sims)

# plot predicted distributions over dep_var ----------------
for(i in 1:n_spells){
  beta_hat[i, , ] <- cbind(
      sim_theta@fixef[, 1:3] + sim_theta@ranef[['spell_id']][, i, ],
      sim_theta@fixef[, 4:ncol(sim_theta@fixef)]
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
  density(d[, 'infant_mortality']),
  type = 'n', xlim = c(-10, 40), ylim = c(0, .12),
  main = paste0("Fit from simulated data")
)
for(i in sample(1:n_sims, 500, replace = FALSE)){
  lines(density(y_hat[i, ]), col = scales::alpha('black', 0.2))
}
lines(density(d$infant_mortality), col = 'red')
lines(density(predict(hlm_allControls)), col = 'green')
legend(
  x = 'topright',
  legend = c('Observed', 'Fitted', 'Simulated'),
  col = c('red', 'green', 'black'), pch = 19
)

# plot fitted regime type effects
pred_dta <- data.frame(
  intercept = 1,
  t_lin = 0, t_squ = 0,
  lag_mad_gdppch =          0, #mean(d$lag_mad_gdppch),
  lag_wdi_agrvagdp =        0, #mean(d$lag_wdi_agrvagdp),
  lag_ross_oil_value_2000 = 0, #mean(d$lag_ross_oil_value_2000),
  lag_ross_gas_value_2000 = 0, #mean(d$lag_ross_gas_value_2000),
  d_monarchy    = c(1, 0, 0, 0, 0, -1),
  d_ideocracy   = c(0, 1, 0, 0, 0, -1),
  d_oneparty    = c(0, 0, 1, 0, 0, -1),
  d_personalist = c(0, 0, 0, 1, 0, -1),
  d_military    = c(0, 0, 0, 0, 1, -1),
  refra = 0,
  fe_etfra = 0
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
yhat <- apply(yhat, 2, function(x){x + rnorm(n_sims, 0, sim_theta@sigma)})

boxplot(yhat); abline(h = mean(fixef(sim_theta)[, 1]))
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
geom_pointrange() +
geom_hline(yintercept = (.5*mean(fixef(sim_theta)[, 1]) + 1)^(1/.5))
