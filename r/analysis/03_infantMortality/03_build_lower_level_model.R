# Build lower level regression model =======================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
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
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    'fe_etfra'
  )
)
d <- na.omit(d)

hlm_baseline <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id),
  data = d, REML = FALSE
)

hlm_varyingControls <- update(hlm_baseline,
  . ~ . +
  growth_mad_gdppch + lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000 # rents
)

anova(hlm_baseline, hlm_varyingControls)
lapply(
  list(hlm_baseline, hlm_varyingControls),
  function(x){cbind(AIC = extractAIC(x)[[2]], BIC = BIC(x), DIC = extractDIC(x))}
)
# Time-varying controls add information

summary(hlm_baseline); summary(hlm_varyingControls)
# Controls reduce intercept variation, but not residual variation
# GDP strong negative correlation with intercepts
# All signs are as expected
# plot slope deviates --------------------------------------
# intercept
car::qqPlot(ranef(hlm_varyingControls)[['spell_id']][, 1])
# large negative deviates violate normality
# t_lin
car::qqPlot(ranef(hlm_varyingControls)[['spell_id']][, 2])
# t_squ
car::qqPlot(ranef(hlm_varyingControls)[['spell_id']][, 3])
# Both t_lin and t_squ clearly violate normality

ggplot(
  data = tmp,
  aes(x = factor(spell), y = mu, ymin = lower, ymax = upper)
) +
  geom_pointrange() +
  geom_hline(yintercept = 0, colour='red')
# numerous significant deviations from the population mean
# hlm clearly required

# Simulate model implied distributions of DV ---------------
n_sims <- 1500
n_spells <- length(unique(d$spell_id))
n_coef <- length(fixef(hlm_varyingControls))
pred_dta <- cbind(
  model.matrix(hlm_varyingControls),
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

sim_theta <- sim(hlm_varyingControls, n_sims)

# plot predicted distributions over dep_var ----------------
for(i in 1:n_spells){
  beta_hat[i, , ] <- cbind(
      sim_theta@fixef[, 1:3] + sim_theta@ranef[['spell_id']][, i, ],
      sim_theta@fixef[, 4:8]
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
for(i in 1:n_sims){
  lines(density(y_hat[i, ]), col = scales::alpha('black', 0.2))
}
lines(density(d$infant_mortality), col = 'red')
lines(density(predict(hlm_varyingControls)), col = 'blue')
legend(
  x = 'topright',
  legend = c('Observed', 'Fitted', 'Simulated'),
  col = c('red', 'blue', 'black'), pch = 19
)