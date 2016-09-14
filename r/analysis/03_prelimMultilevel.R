library('lme4'); library('lmtest')
hlm_1 <- lmer(
  absolute_poverty ~ t_lin + t_squ + t_cub + (1 | spell_id),
  data = na.omit(analysis), REML = FALSE
)
hlm_2 <- update(
  hlm_1, . ~ . + growth_mad_gdppch + lag_mad_gdppch +
    lag_wdi_agrvagdp + lag_wdi_pop65 + lag_ross_oil_value_2000 +
    lag_ross_gas_value_2000,
  REML = FALSE
)
summary(hlm_1)
summary(hlm_2)
lrtest(hlm_1, hlm_2)
confint(hlm_2)


hlm_3 <- update(
  hlm_2, . ~ . + d_monarchy + d_ideocracy + d_oneparty +
    d_personalist + d_military + fe_etfra + postColdWar + 
    region,
  REML = FALSE
)
lrtest(hlm_2, hlm_3)
summary(hlm_3)

ran_intercepts <- ran_intercepts[[1]]
plot(density(ran_intercepts))


ran_intercepts <- ranef(hlm_1)[['spell_id']]
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