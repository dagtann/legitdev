# 
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('lme4'); library('MASS')

d <- subset(
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 't_lin', 't_squ',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "lp_catho80", "lp_muslim80", "lp_protmg80", 'fe_etfra',
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa", 'region', 'regime_type'
  )
)
d <- na.omit(d)
dim(d)

hlm_interceptOnly <- lmer(
  infant_mortality ~ 1 + (1 | spell_id),
  data = d, REML = FALSE
)
hlm_fixedGrowthCurve <- update(hlm_interceptOnly,
  . ~ . + t_lin + t_squ
)
hlm_randGrowthCurve <- update(hlm_fixedGrowthCurve,
  . ~ . - (1| spell_id) + (1 + t_squ | spell_id)
)
lmtest::lrtest(
  hlm_interceptOnly, hlm_fixedGrowthCurve, hlm_randGrowthCurve
)
summary(hlm_randGrowthCurve)

hlm_ranefGrowth_residualControls <- update(
  hlm_randGrowthCurve, . ~ . +
  growth_mad_gdppch + lag_mad_gdppch +
  lag_wdi_agrvagdp + 
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000, 
  data = d, REML = FALSE
)
summary(hlm_ranefGrowth_residualControls)
lmtest::lrtest(hlm_interceptOnly, hlm_ranefGrowth_residualControls)

hlm_fullControls <- update(hlm_ranefGrowth_residualControls,
  . ~ . + 
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military +
  fe_etfra +
  lp_catho80 + lp_muslim80 + lp_protmg80 +
  d_eap + d_eca + d_lac + d_mena + d_sa,
  data = d, REML = FALSE
)
summary(hlm_fullControls)
d_monarchy              -1.926521   1.027636   -1.87
d_ideocracy             -1.739951   1.020665   -1.70
d_oneparty               1.681452   0.718957    2.34
d_personalist            1.696987   0.829837    2.04
d_military              -0.070086   0.598421   -0.12


-2.040080 +
 0.018379 +
 1.024916 +
 0.794509 +
 0.175061 




tmp <- aggregate(
  d[, c("lp_catho80", "lp_muslim80", "lp_protmg80", 'region', 'regime_type')],
  by = list(spell_id = d$spell_id), FUN = unique
)
mytable <- with(tmp, table(region, regime_type))
chisq.test(mytable, simulate.p.value = TRUE)

tmp <- gather(tmp, 'religion', 'value', lp_catho80, lp_muslim80, lp_protmg80)
ggplot(data = tmp, aes(x = value, colour = religion)) +
  stat_ecdf() + geom_rug() + facet_wrap(~ regime_type)
with(tmp, boxplot(value ~ religion + regime_type, las = 3))


hlm_2 <- update(hlm_1,
  . ~ . 
  + d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military,
  data = analysis, REML = FALSE
)
summary(hlm_2)
lmtest::lrtest(hlm_1, hlm_2)
lapply(list(hlm_1, hlm_2), function(x){cbind(AIC(x), BIC(x))})


tmp <- hlm_2@frame
head(tmp)
tmp <- within(tmp, {
  regime_type = ifelse(d_monarchy == 1, 'Monarchy', '')
  regime_type = ifelse(d_oneparty == 1, 'Oneparty', regime_type)
  regime_type = ifelse(d_ideocracy == 1, 'Ideocracy', regime_type)
  regime_type = ifelse(d_personalist == 1, 'Personalist', regime_type)
  regime_type = ifelse(d_military == 1, 'Military', regime_type)
  regime_type = ifelse(regime_type == '', 'Electoral', regime_type)
  }
)
table(tmp$regime_type)

with(tmp, boxplot(
  (.5*infant_mortality + 1)^(1/.5) ~ regime_type, horizontal = TRUE, las = 1
  )
)

ggplot(base, aes(x = regime_type)) +
  geom_bar() + facet_wrap(~ region)

with(base, unique(cowcode[is.na(region)]))
View(base[with(base, is.na(region)), ])
summary(base[is.na(base$region) & base$year >= 1960, 'year'])
unlist(unique(base[is.na(base$region), 'year']))
View(base[is.na(base$region) & base$year >= 1960, ]) # Only NA if NA on dep_vars