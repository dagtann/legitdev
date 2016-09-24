# Find a base line hlm model for infant_mortality ==========
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('lme4'); library('MASS')

# Intercept variation? -------------------------------------
hlm_interceptOnly <- lmer(
  infant_mortality ~ 1 + (1 | spell_id),
  data = analysis, REML = FALSE
)
summary(hlm_interceptOnly)
# ICC = .8434
# Does it generate non-sensical interecepts?
# -> infant mortality is strictly positive
summary(
  predict(hlm_interceptOnly)
) # all positive
rm(hlm_interceptOnly)

hlm_1 <- lmer(
  infant_mortality ~ 
    t_lin + t_squ + t_cub +
    dur_lin + dur_squ + dur_cub +
    (1 | spell_id),
  data = analysis,
  REML = FALSE
)
summary(hlm_1)
# Does it generate non-sensical interecepts?
# -> infant mortality is strictly positive
summary(predict(hlm_1))       # negative predictions already

hlm_2 <- update(hlm_1,
  . ~ . - dur_lin - dur_squ - dur_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
lapply(
  list(hlm_1, hlm_2),
  function(x){cbind(AIC = AIC(x), BIC = BIC(x))}
)
# complex model with dur and t is preferred.

hlm_2 <- update(hlm_1,
  . ~ . - t_cub - dur_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
lapply(
  list(hlm_1, hlm_2),
  function(x){cbind(AIC = AIC(x), BIC = BIC(x))}
)
# model with both cub's is preferred
hlm_2 <- update(hlm_1,
  . ~ . - t_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
# results depends on dur_cub, while t_cub could be removed

# random slopes in time required?
hlm_2 <- update(hlm_1,
  . ~ . - (1 | spell_id) + (1 + t_cub + dur_cub | spell_id),
  REML = FALSE
)
summary(hlm_2)
lmtest::lrtest(hlm_1, hlm_2)
# There is significant variation in either random term on _cub
# Now average t_cub turns stat significant
# -> time dependencies in models are strong and quite
#   heterogeneous. Complex growth curve make for more
#   complicated models, but increase chance of unbiased
#   picture of effects.


hlm_3 <- update(hlm_2,
  . ~ . 
  + lag_mad_gdppch
  + lag_wdi_agrvagdp + lag_wdi_pop65
  + lag_ross_oil_value_2000 + lag_ross_gas_value_2000
  + d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military,
  #+ d_eap + d_eca + d_lac + d_mena + d_sa,
  data = analysis
)
summary(hlm_3)