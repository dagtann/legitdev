# Minor eda ================================================
vars <- c(
  'SI.POV.DDAY', 'mad_gdppc', 'lag_mad_gdppch',
  'growth_mad_gdppch', 'Fiscal_Reliance', 'wdi_agrvagdp',
  'wdi_pop65', 'fe_etfra', 'ross_gas_value_2000',
  'ross_oil_value_2000'
)
summary(base[, vars])
# adjust % vars to [0, 1] interval before analysis

tmp <- aggregate(base[, vars], # how many non-missing per spell?
  by = list(spell_id = base[['spell_id']]),
  FUN = function(x){sum(!is.na(x))}
)
summary(tmp)
# Fiscal_Reliance and SI.POV.DDAY carry practically no
# information

# within-variance ------------------------------------------
tmp <- aggregate(base[, vars],
  by = list(spell_id = base[['spell_id']]),
  FUN = sd, na.rm = TRUE
)
summary(tmp)
dev.new()
par(mfrow = c(2, (ncol(tmp)-1)/2))
for(i in 2:ncol(tmp)){
  boxplot(tmp[, i], main = names(tmp)[i])
}
# If there are multiple measurements, then there will be
# within variance. Ethnic fractionalization is the only 
# constant variable other than regime type.
dev.off()

# 0-order correlations -------------------------------------
pairs(base[, vars])
round(cor(base[, vars], use = 'pairwise.complete.obs', method = 'spearman'), 2)
summary(base[, vars])
# economic predictors strongly correlated
# econ growth weakest link to poverty
# ross_* must be logged

# variable transformations ---------------------------------
boxplot(base[, vars])
car::symbox(base$ross_oil_value_2000+.01)
summary(car::powerTransform(base$ross_oil_value_2000+.01))
# substantively 0 -> log transform

car::symbox(base$ross_gas_value_2000+.01)
summary(car::powerTransform(base$ross_gas_value_2000+.01))
# substantively 0 -> log transform

car::symbox(base$lag_mad_gdppch)
summary(car::powerTransform(base$lag_mad_gdppch))
# proposes ^(-1/4), symbox looks good on ^(-1/2)

tmp <- subset(base, select = 'lag_mad_gdppch') # direct comparisons
tmp <- within(tmp, {
  lag_mad_gdppch_power_neg1over2 <- lag_mad_gdppch^(-1/2)
  lag_mad_gdppch_power_neg1over4 <- lag_mad_gdppch^(-1/4)
  }
)
ggplot(data = tmp, aes(x = lag_mad_gdppch)) +
  geom_point(aes(y = lag_mad_gdppch_power_neg1over2, colour = '^-1/2')) +
  geom_point(aes(y = lag_mad_gdppch_power_neg1over4, colour = '^-1/4')) +
  geom_point(aes(y = log(lag_mad_gdppch), colour = 'log'))
ggplot(data = tmp) +
  geom_boxplot(aes(x = 1, y = lag_mad_gdppch_power_neg1over2, colour = '^-1/2')) +
  geom_boxplot(aes(x = 2, y = lag_mad_gdppch_power_neg1over4, colour = '^-1/4')) +
  geom_boxplot(aes(x = 3, y = log(lag_mad_gdppch)/100, colour = 'log'))
# ^(-1/4) looks most symmetric and can be justified by boxcox
# BUT log more informative
# Housekeeping =============================================
  rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])