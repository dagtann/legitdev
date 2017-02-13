# Examine upper level model for rel. fractionalization =====
# A reviewer noted that if ethnic fractionalization works
# against regime legitimacy than religious fractionalization
# should do the same thing. This script tests the idea.
# Preamble =================================================
library('car')
library('lmtest')
# (A) Univariate distribution ------------------------------
tmp <- aggregate( 
  analysis[, c('refra', 'fe_etfra')],
  by = list(spell_id = analysis[, 'spell_id']),
  FUN = min
)
summary(tmp$refra)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   9.443  29.920  34.540  59.610  73.730      57 

ggplot(data = tmp, aes(x = refra)) + stat_ecdf()
ggplot(data = tmp, aes(x = refra)) + geom_histogram(binwidth = .5)
ggplot(data = tmp, aes(x = refra)) + geom_density() + geom_rug()
# Bimodal distribution, clusters near extremes

# (B) multicollinearity ------------------------------------
ggplot(data = tmp, aes(x = fe_etfra, y = refra)) +
  geom_point() + geom_smooth() + geom_smooth(method = 'lm')
# non-monotone relationship

summary(                  # partial correlation coefficients
  lm(refra ~ fe_etfra + I(fe_etfra^2), data = tmp)
)
cor(                      # multiple correlation coefficient
  tmp[, 'refra'],
  predict(
    lm(refra ~ fe_etfra + I(fe_etfra^2), data = tmp),
    newdata = tmp
  ),
  use = 'complete.obs', method = 'pearson'
)
# medium strong, non-linear relationship
# realtionship expected, b/c ethnic identity often includes religion
# inverted u-shape is surprising and may depend on the
# religion data
rm(tmp)

# (C) upper level model ------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ', 't_cub', 't_qud',
    "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    'refra', 'fe_etfra',
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military"
  )
)
d <- na.omit(d)
d <- within(d, {spell_factor <- factor(spell_id)})

# create intercept deviates for each spell
fit <- glm(       
  infant_mortality ~ 0 +                  # no pooling model
    t_lin + t_squ + t_cub +                   # growth curve
    lag_mad_gdppch + lag_wdi_agrvagdp +            # economy
    lag_ross_gas_value_2000 + lag_ross_oil_value_2000 + # rents
    spell_factor,                         # spell intercepts
  data = d, family = gaussian
)
# extract deviates & create model for upper level
intercept_deviates <- coef(fit)[8:length(coef(fit))]; rm(fit)
intercept_deviates <- data.frame(
  spell_id = names(intercept_deviates),
  ideviate = intercept_deviates
)
intercept_deviates[, 'spell_id'] <- vapply(
  strsplit(as.character(intercept_deviates[, 'spell_id']), 'r'),
  FUN = function(x){ x[[2]] },
  character(1)
)

tmp <- aggregate(
  d[, c('fe_etfra', 'refra', "d_monarchy", "d_ideocracy",
    "d_oneparty", "d_personalist", "d_military")
  ],
  by = list(spell_id = d[, 'spell_id']),
  FUN = min
)
ulevel <- left_join(tmp, intercept_deviates, by = 'spell_id')
rownames(ulevel) <- ulevel[, 'spell_id']
rm(intercept_deviates, tmp)

# examine spell intercepts
summary(ulevel[, 'ideviate'])
plot(density(ulevel[, 'ideviate']))
qqPlot(ulevel[, 'ideviate'], dist = 'norm') 
# little non-normality, negative skew

scatterplotMatrix(ulevel[, c('ideviate', 'refra', 'fe_etfra')])
# ethnic fractionalization looks non-normal

# (d) Control model development ----------------------------
fit <- lm(
  ideviate ~ refra + fe_etfra, data = ulevel
)
summary(fit)
residualPlots(fit)
marginalModelPlots(fit) # curvature in ethnic fractionalization

fit2 <- update(fit, . ~ . - fe_etfra +
  I(fe_etfra/10) + I(fe_etfra^2/100) + I(fe_etfra^3/1000)
)
summary(fit2)
residualPlots(fit2)

fit3 <- update(fit2, . ~ . - I(fe_etfra^3/1000))
anova(fit2, fit3)
residualPlots(fit3)
qqPlot(
  rstudent(fit3), dist = 't', 
  df = length(resid(fit)) - length(coef(fit)[-1]) - 1,
  id.n = 10
)
outlierTest(fit3)
influencePlot(fit3, id.n = 10)
influenceIndexPlot(fit3, id.n = 10)
crPlots(fit3)
avPlots(fit3, id.n = 10)
# Squared term enough
fit2 <- fit3

# (e) add regime type variables ----------------------------
fit3 <- update(fit2, . ~ . + d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military)
summary(fit3)
anova(fit2, fit3)
residualPlots(fit3)
marginalModelPlots(fit3)
influenceIndexPlot(fit3, id.n = 10)
influencePlot(fit3)
avPlots(fit3)
outlierTest(fit3)
crPlots(fit3)

# (f) outlier diagnostics ----------------------------------
compareCoefs(
  fit3,
  update(fit3, data = subset(ulevel, spell_id != '572:1'))
)
ggplot(
  data = ulevel,
  aes(x = d_monarchy, y = refra, shape = ifelse(spell_id == '572:1', 'Swazi', 'other'))
) +
geom_point()
# Swaziland is an usually fractionalized monarchy
# Removing Swaziland boosts coef on d_monarchy, but does
# not substantially change results otherwise

ggplot(
  data = ulevel,
  aes(x = d_oneparty, y = ideviate, colour = ifelse(spell_id == '820:1', 'Malay', 'other'))
) +
geom_point()
# Malaysia is an electoral autocracy with an unusually low mortality
compareCoefs(
  fit3,
  update(fit3, data = subset(ulevel, spell_id != '820:1'))
)
# Low leverage point that is just poorly captured

ggplot(
  data = ulevel,
  aes(x = d_monarchy, y = ideviate, colour = ifelse(spell_id == '811:4', 'Camb', 'other'))
) +
geom_point()
# Cambodia is a monarchy with low ethnic fractionalization and
# high mortality
compareCoefs(
  fit3,
  update(fit3, data = subset(ulevel, spell_id != '811:4'))
)
# High leverage on d_monarchy, beta boosted if dropped.

compareCoefs(
  fit3,
  update(
    fit3,
    data = subset(ulevel, spell_id %in% c('572:1', '811:4') == FALSE)
  )
)
coeftest(
  update(
    fit3,
    data = subset(ulevel, spell_id %in% c('572:1', '811:4') == FALSE)
  )
)
# Dropping both unusual monarchies d_monarchy more than
# doubles and turns statistically significant at p <= .1.

fit4 <- update(fit3, . ~ . - refra - I(fe_etfra/10) - I(fe_etfra^2/100))
summary(fit4)
compareCoefs(fit3, fit4, update(fit3, .~. - I(fe_etfra^2/100)))
# accepting misspecification in fe_etfra attenuates most
# regime type coefficients
# ethnic and religious fractionalization increase child mortality

# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
 