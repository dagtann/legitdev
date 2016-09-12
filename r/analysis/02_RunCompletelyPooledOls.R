# Perform simple OLS on complete pooling data, use cluster robust SE
library('car'); library('lmtest'); library('multiwayvcov')

regression_formula <- formula(
  paste(
    'absolute_poverty ~', paste(
    c(
      'growth_mad_gdppch',
      names(analysis)[ names(analysis) %in% c(
        'cowcode', 'year', 'start_year', 'end_year', 'spell_id',
        'regime_type', 'fe_etfra', 'SI.POV.DDAY', 'absolute_poverty'
      ) == FALSE]
    ), 
    collapse = ' + '
    )
  )
)
regression_formula

fit <- lm(regression_formula, data = analysis)
summary(fit)

residualPlot(fit)             # indicates heteroscedasticity
bptest(fit)                      # strong heteroscedasticity
residualPlots(fit) 
# slight curvature in agriculture
# ross variables cluster at 0, perhaps dichotomize?

dwt(fit)                            # Strong autocorrelation
plot(resid(fit), type = 'b')

marginalModelPlots(fit)                   # look mostly fine

avPlots(fit, id.n = 5)
crPlots(fit)

influencePlot(fit)
influenceIndexPlot(fit)
outlierTest(fit)

lrtest(fit, update(fit, . ~ . - dur_lin - dur_squ - dur_cub))
# regime duration not required.