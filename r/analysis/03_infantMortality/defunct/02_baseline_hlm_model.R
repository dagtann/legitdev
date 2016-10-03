# Find a base line hlm model for infant_mortality ==========
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('lme4'); library('MASS')

# analysis data --------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 't_lin', 't_squ', 't_cub',
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
dim(d)

# Intercept variation? -------------------------------------
hlm_interceptOnly <- lmer(
  infant_mortality ~ 1 + (1 | spell_id),
  data = d, REML = FALSE
)
summary(hlm_interceptOnly)
sigma2_regime <- unlist(VarCorr(hlm_interceptOnly))
sigma2_e <- sigma(hlm_interceptOnly)^2
sigma2_regime/(sigma2_regime + sigma2_e)
# ICC = .851
# Does it generate non-sensical interecepts?
# -> infant mortality is strictly positive
rm(sigma2_regime, sigma2_e)

hlm_1 <- lmer(
  infant_mortality ~
    t_lin + t_squ + t_cub +
    dur_lin + dur_squ + dur_cub +
    (1 | spell_id),
  data = d,
  REML = FALSE
)
summary(hlm_1)

hlm_2 <- update(hlm_1,
  . ~ . - dur_lin - dur_squ - dur_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
lapply(
  list(hlm_1 = hlm_1, hlm_2 = hlm_2),
  function(x){cbind(AIC = AIC(x), BIC = BIC(x))}
)
# complex model with dur and t is preferred.

hlm_2 <- update(hlm_1,
  . ~ . - t_cub - dur_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
lapply(
  list(hlm_1 = hlm_1, hlm_2 = hlm_2),
  function(x){cbind(AIC = AIC(x), BIC = BIC(x))}
)
# model with both cub's is preferred
hlm_2 <- update(hlm_1,
  . ~ . - t_cub, REML = FALSE
)
lmtest::lrtest(hlm_1, hlm_2)
# results depends on dur_cub, while t_cub could be removed

# random slopes in time required?
hlm_3 <- update(hlm_2,
  . ~ . - (1 | spell_id) + (1 + t_squ + dur_cub | spell_id),
  REML = FALSE
)
summary(hlm_3)
lmtest::lrtest(hlm_2, hlm_3)
# There is significant variation in either random term on _cub
# Fixed effects on t_lin and dur_lin are strongly correlated

hlm_4 <- update(hlm_3,
  . ~ . 
  + growth_mad_gdppch + lag_mad_gdppch
  + lag_wdi_agrvagdp +
  + lag_ross_oil_value_2000 + lag_ross_gas_value_2000,
  data = d, REML = FALSE
)
summary(hlm_4)

hlm_5 <- update(hlm_4,
  . ~ . 
  + d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military,
  data = d, REML = FALSE
)
summary(hlm_5)
lmtest::lrtest(hlm_4, hlm_5)

hlm_6 <- update(hlm_5, . ~ . +
  lp_catho80 + lp_muslim80 + lp_protmg80 +
  d_eap + d_eca + d_lac + d_mena + d_sa +
  fe_etfra,
  data = d, REML = FALSE
)
summary(hlm_6)