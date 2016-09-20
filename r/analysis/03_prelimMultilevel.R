library('lme4'); library('lmtest')
vars <- c(
  'spell_id', 'absolute_poverty', 'growth_mad_gdppch',
  'lag_mad_gdppch', 'lag_wdi_agrvagdp', 'lag_wdi_pop65',
  'lag_ross_oil_value_2000', 'lag_ross_gas_value_2000',
  't_cub', 't_squ', 't_lin', 'fe_etfra', 'd_monarchy',
  'd_ideocracy', 'd_oneparty', 'd_personalist', 'd_military'
)
fit_data <- na.omit(analysis[, vars])
dim(fit_data)
hlm_1 <- lmer(
  absolute_poverty ~ t_lin + t_squ + t_cub + (1 | spell_id),
  data = fit_data, REML = FALSE
)
summary(hlm_1)

t_seq <- seq(min(fit_data$t_lin), max(fit_data$t_lin), .1)
niter <- 1500
draws <- MASS::mvrnorm(n = niter, mu = fixef(hlm_1), Sigma = vcov(hlm_1))
pred_dta <- cbind(1, t_lin = t_seq, t_squ = t_seq^2, t_cub = t_seq^3)
head(pred_dta)
yhat_sim <- pred_dta %*% t(draws)
yhat_mean <- pred_dta %*% fixef(hlm_1)

png(
  file = file.path(pathOut, 'spaghetti_basisModell.png'),
  width = plot_size, height = plot_size, units = 'in',
  res = 300
)
plot(
  x = t_seq*10+1990,
  y = seq(0, 1, length.out = length(t_seq)),
  type = 'n', xlab = '',
  ylab = "Bevölkerungsanteil (%) mit weniger als 1,90 $/Tag",
  main = 'Latente Wachstumskurve des Basismodells',
  las = 1
)
for(i in 1:niter){
  lines(
    x = t_seq*10+1990,
    y = 100 * (boot::inv.logit(yhat_sim[, i])-.001),
    col = scales::alpha('black', .2)
  )
}
lines(t_seq*10+1990, 100 * (boot::inv.logit(yhat_mean)-.001), col = 'red')
dev.off()


0.45904/(0.45904+0.04145)





hlm_2 <- update(
  hlm_1, . ~ . + growth_mad_gdppch + lag_mad_gdppch +
    lag_wdi_agrvagdp + lag_wdi_pop65 + lag_ross_oil_value_2000 +
    lag_ross_gas_value_2000, data = fit_data,
  REML = FALSE
)
summary(hlm_2, correlation = FALSE)

lrtest(hlm_1, hlm_2)
confint(hlm_2)


hlm_3 <- update(
  hlm_2, . ~ . + d_monarchy + d_ideocracy + d_oneparty +
    d_personalist + d_military + fe_etfra,
  REML = FALSE, data = fit_data
)
lrtest(hlm_2, hlm_3)
summary(hlm_3)


lapply(
  list(hlm_1, hlm_2, hlm_3),
  summary, correlation = FALSE
)
ran_intercepts <- ran_intercepts[[1]]
plot(density(ran_intercepts))


ran_intercepts <- ranef(hlm_3)[['spell_id']]
names(ran_intercepts) <- 'intercept_deviate'
ran_intercepts <- within(ran_intercepts, {
  spell_id <- rownames(ran_intercepts)
  spell_id <- factor(spell_id)
  }
)
tmp <- aggregate(regime_type ~ spell_id, data = analysis, FUN = unique)
ran_intercepts <- left_join(
  ran_intercepts, tmp, by = 'spell_id'
)
ols_2ndlevel <- lm(
  intercept_deviate ~ regime_type, data = ran_intercepts
)
summary(ols_2ndlevel)





str(ran_intercepts)