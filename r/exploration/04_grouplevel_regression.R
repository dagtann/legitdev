# Group level regression -----------------------------------
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('lme4')
library('car')

# Initiate workspace ---------------------------------------
start <- .001
base <- mutate(                # transform DV to valid range
  base, logit_poverty = boot::logit(SI.POV.DDAY/100+start)
) %>%
  mutate(StartPostCw = ifelse(start_year >= 1990, 1, 0))

base <- data.frame(  # create & attach cubic t to data.frame
  base, poly(base[['year']] - 1990, degree = 3)[, 1:3]
)
names(base)[(ncol(base)-2):ncol(base)] <- c('t_lin', 't_squ', 't_cub')

fit_hlm <- lmer(                # extract intercept deviates
  logit_poverty ~ t_lin + t_squ + t_cub + (1 | spell_id),
  data = base
)
intercept_deviates <- ranef(fit_hlm)[[1]]
intercept_deviates[, 'spell_id'] <- rownames(intercept_deviates)

regime_types <- aggregate(
  regime_type ~ spell_id, data = base, FUN = unique
)
d_StartPostColdWar <- aggregate(StartPostCw ~ spell_id, data = base, FUN = unique)
contrasts(regime_types[['regime_type']]) <- contr.Sum(
  length(levels(regime_types[['regime_type']]))
)
tmp <- left_join(
  intercept_deviates, regime_types, by = 'spell_id'
)
tmp <- left_join(
  tmp, d_StartPostColdWar, by = 'spell_id'
)
names(tmp)[1] <- 'deviate'
table(tmp$regime_type)
contrasts(tmp$regime_type)
# group level regression -----------------------------------
fit <- lm(deviate ~ regime_type, data = tmp)
lmtest::coeftest(fit)                       # coef stat sig?
-1*sum(coef(fit)[-1])       # calc coef on electoral regimes
# Intercept deviates for ideocracies are lower than the sample
# mean. This effect is statistically significant at alpha = .99.
# Intercept deviates for electoral authoritarian regimes are
# also lower than other deviates. Cannot say whether effect
# statistically significant.
anova(fit, update(fit, . ~ . - regime_type))
# Jointly authoritarian regimes border on statistical
# significance.

# Model fit ------------------------------------------------
library('car')
residualPlot(fit)                       # strongly patterned
lmtest::bptest(fit)                  # No heteroscedasticity
residualPlots(fit, terms = ~ regime_type)

influencePlot(fit)
outlierTest(fit)                         # obs 8 not outlier
tmp[8, ] # Uruguay 1973-1983
with(
  subset(base, cowcode == 165 & spell_no == 2),
  plot(year, SI.POV.DDAY)
)
ggplot(data = subset(base, regime_type == 'Military'),
  aes(
    x = year,
    y = SI.POV.DDAY,
    colour = ifelse(
      cowcode == 165 & spell_no == 2, 'Uruguay', 'other'
    )
  )
) + geom_point() + labs(colour = '')
# Uruguay = military dict. with smallest poverty rate of all
# military regimes.
compareCoefs(fit, update(fit, data = tmp[-8, ]))
# Below average deviate MIL only b/c Uruguay

tmp[33, ] # Ghana 1981-1990, personalist regime
ggplot(data = base,
  aes(
    x = year,
    y = SI.POV.DDAY,
    colour = ifelse(
      cowcode == 452 & spell_no == 6, 'Ghana', 'other'
    )
  )
) + geom_point() + labs(colour = '')
compareCoefs(fit, update(fit, data = tmp[-c(8, 33), ]))
# Excluding Ghana effect estimates consistent, but SE on
# personalist increases by 6 per cent

marginalModelPlot(fit) # regime types offer insufficient fit
# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
detach(package:lme4)
detach(package:car)
## END