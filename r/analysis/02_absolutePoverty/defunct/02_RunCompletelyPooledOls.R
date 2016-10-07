# Execute simple OLS on complete pooling data ==============

# Prep workspace -------------------------------------------
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
analysis_packs <- c('car', 'lmtest', 'multiwayvcov', 'MASS')
lapply(analysis_packs, library, character.only = TRUE)

# Initial fit ----------------------------------------------
regression_formula <- formula(
  paste(
    'absolute_poverty ~', paste(
    c(                    # remove panel id's from OLS model
      'growth_mad_gdppch',
      names(analysis)[ names(analysis) %in% c(
        'cowcode', 'year', 'start_year', 'end_year', 'spell_id',
        'regime_type', 'lp_no_cpm80', 'd_ssa',
        'SI.POV.DDAY', 'absolute_poverty', 'SP.DYN.IMRT.IN'
      ) == FALSE]
    ), 
    collapse = ' + '
    )
  )
)
fit1 <- lm(regression_formula, data = analysis)
summary(fit1)

residualPlot(fit1)            # indicates heteroscedasticity
bptest(fit1)                     # strong heteroscedasticity
residualPlots(fit1) 
# slight curvature in agriculture
# ross variables cluster at 0, perhaps dichotomize?

dwt(fit1)                           # Strong autocorrelation
plot(resid(fit1), type = 'b')

marginalModelPlots(fit1)                  # look mostly fine
# Single observation on growth > 20% causes trouble

avPlots(fit1, id.n = 5)
crPlots(fit1)

influencePlot(fit1, id.n = 5)
influenceIndexPlot(fit1, id.n = 5)
outlierTest(fit1)

lrtest(fit1, update(fit1, . ~ . - dur_lin - dur_squ - dur_cub))
# regime duration not required.

fit2 <- update(fit1, . ~ . - dur_lin - dur_squ - dur_cub)
summary(fit2)
lapply(list(fit1, fit2), AIC); lapply(list(fit1, fit2), BIC)

compareCoefs(fit1, fit2)
influenceIndexPlot(fit2, id.n = 5)
influencePlot(fit2, id.n = 5)
marginalModelPlots(fit2)

crPlots(fit2)           # problematic outlier in growth > .2
which(analysis$growth_mad_gdppch > .2 & !is.na(analysis$absolute_poverty))
compareCoefs(fit2, update(fit2, data = analysis[rownames(analysis) != "1114", ]))
crPlots(update(fit2, data = analysis[rownames(analysis) != "1114", ]))
# little impact if removed

outlierTest(fit2) # 2568
View(analysis[rownames(analysis) == "2568", ]) # Swaziland 1994
# .81 on poverty
boxplot(boot::inv.logit(analysis$absolute_poverty))
# within 1.5 IQR, but barely so
compareCoefs(fit2, update(fit2, data = analysis[rownames(analysis) != "2568", ]))
coef_t_fit2 <- coef(fit2)[c('(Intercept)', 't_lin', 't_squ', 't_cub')]
coef_t_fit3 <- coef(update(fit2, data = analysis[rownames(analysis) != "2568", ]))[c('(Intercept)', 't_lin', 't_squ', 't_cub')]
plot(c(-44, 20), c(-100, 100), type = 'n')
curve(coef_t_fit2[1] + coef_t_fit2[2] * x/10 + coef_t_fit2[3] * x^2/100 + coef_t_fit2[4] * x^3/1000, col = 'blue', add = TRUE)
curve(coef_t_fit3[1] + coef_t_fit3[2] * x/10 + coef_t_fit3[3] * x^2/100 + coef_t_fit3[4] * x^3/1000, col = 'red', add = TRUE)
dev.off(); rm(coef_t_fit3, coef_t_fit3)
# dropping Swaziland 1994 flips sign on t_lin,
# but implication on t_ polynomials basically unchanged


spells_in_model <- subset(
  analysis,
  #rownames(analysis) %in% rownames(fit2$model),
  select = 'spell_id'
)
spells_in_model <- spells_in_model[['spell_id']]
sigma_robust <- cluster.vcov(fit2, cluster = spells_in_model)
coeftest(fit2, vcov = sigma_robust)

set.seed(1942)
N <- 5000
sim_betas <- mvrnorm(n = N, mu = coef(fit2), Sigma = sigma_robust)
pdta_betas <- data.frame(sim_betas)
pdta_betas <- within(pdta_betas, 
  d_electoral <- 0 - rowSums(
    pdta_betas[, 
    c(
      "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist",
      "d_military"
    )
  ]
  )
)
# Plot estimation uncertainty
pdta_betas <- gather(pdta_betas, 'regime_type', 'value', d_monarchy:d_electoral)
head(pdta_betas)
ggplot(data = pdta_betas, aes(x = value, colour = regime_type)) +
  geom_density()

sim_dta <- data.frame(
  Intercept = 1,
  growth_mad_gdppch = mean(analysis$growth_mad_gdppch, na.rm = TRUE),
  fe_etfra  = mean(analysis$fe_etfra, na.rm = TRUE),
  lag_mad_gdppch  = mean(analysis$lag_mad_gdppch, na.rm = TRUE),
  lag_wdi_agrvagdp  = mean(analysis$lag_wdi_agrvagdp, na.rm = TRUE),
  lag_wdi_pop65  = mean(analysis$lag_wdi_pop65, na.rm = TRUE),
  lag_ross_oil_value_2000 = mean(analysis$lag_ross_oil_value_2000, na.rm = TRUE),
  lag_ross_gas_value_2000 = mean(analysis$lag_ross_gas_value_2000, na.rm = TRUE),
  lp_catho80 = mean(analysis$lp_catho80, na.rm = TRUE),
  lp_muslim80 = mean(analysis$lp_muslim80, na.rm = TRUE),
  lp_protmg80 = mean(analysis$lp_protmg80, na.rm = TRUE),
  postColdWar = 0,
  t_cub = 0, t_squ = 0, t_lin = 0,
  d_monarchy = c(1, 0, 0, 0, 0, -1),
  d_ideocracy =  c(0, 1, 0, 0, 0, -1),
  d_oneparty = c(0, 0, 1, 0, 0, -1),
  d_personalist = c(0, 0, 0, 1, 0, -1),
  d_military = c(0, 0, 0, 0, 1, -1),
  d_eap = 0,
  d_eca = 0,
  d_lac = 0, 
  d_mena = 0,
  d_sa = 0
)
sim_results <- data.frame(
  sim_dta, as.matrix(sim_dta) %*% t(sim_betas)
)
sim_results <- gather(sim_results, 'sim_no', 'value', 26:ncol(sim_results))
str(sim_results)

head(sim_results)
dev.new()
par(mfrow = c(1, 6))
plot(density(boot::inv.logit(sim_results$value[sim_results$d_monarchy == 1])))
plot(density(boot::inv.logit(sim_results$value[sim_results$d_ideocracy == 1])))
plot(density(boot::inv.logit(sim_results$value[sim_results$d_oneparty == 1])))
plot(density(boot::inv.logit(sim_results$value[sim_results$d_personalist == 1])))
plot(density(boot::inv.logit(sim_results$value[sim_results$d_military == 1])))
plot(
  density(
    boot::inv.logit(sim_results$value[
        rowSums(
          sim_results[, c("d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military")]
        ) == -5
      ]
    )
  )
)

