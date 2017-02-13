# Substantive effect regime types ==========================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ', 
    "lag_mad_gdppch", "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military",
    'refra', 'fe_etfra'
  )
)
d <- na.omit(d)
hlm_varyingControls <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
  lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000, # rents
  data = d, REML = FALSE
)
hlm_regimeTypes <- update(
  hlm_varyingControls, . ~ . +
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military +
  refra + fe_etfra
)

n_sims <- 2000
sim_theta <- sim(hlm_regimeTypes, n_sims)

pred_dta <- data.frame(
  intercept = 1,
  t_lin = 0,
  t_squ = 0,
  lag_mad_gdppch =          mean(d$lag_mad_gdppch),
  lag_wdi_agrvagdp =        mean(d$lag_wdi_agrvagdp),
  lag_ross_oil_value_2000 = mean(d$lag_ross_oil_value_2000),
  lag_ross_gas_value_2000 = mean(d$lag_ross_gas_value_2000),
  d_monarchy    = c(1, 0, 0, 0, 0, -1, 0),
  d_ideocracy   = c(0, 1, 0, 0, 0, -1, 0),
  d_oneparty    = c(0, 0, 1, 0, 0, -1, 0),
  d_personalist = c(0, 0, 0, 1, 0, -1, 0),
  d_military    = c(0, 0, 0, 0, 1, -1, 0),
  refra = mean(d$refra),
  fe_etfra = mean(d$fe_etfra)
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
#yhat <- apply(yhat, 2, function(y){y + rnorm(n_sims, 0, sim_theta@sigma)})
# Disregards random effects b/c intercept variance does not
# interfere with fixed effect on regimetype

alpha <- .1
tmp <- apply(yhat[, 1:6], 2, function(x){
  unscaled_x <- x
  mu <- mean(unscaled_x)
  lower <- quantile(unscaled_x, alpha/2, names = FALSE)
  upper <- quantile(unscaled_x, 1-alpha/2, names = FALSE)
  out <- c(mu = mu, lower = lower, upper = upper)
  out <- (.5 * out + 1)^(1/.5)
  return(out)
  }
)
tmp <- data.frame(t(tmp))
tmp[, 'regime_type'] <- c(
  'monarchy', 'ideocracy', 'oneparty', 'personalist',
  'military', 'electoral'
)
# relative effect sizes - How strongly does legitimacy matter?
mu <- (.5*mean(yhat[, 7])+1)^(1/.5) # 74.02

(.5*mean(yhat[, 1])+1)^(1/.5) # 64.13 - monarchies
((.5*mean(yhat[, 1])+1)^(1/.5) - mu)/mu # -13.4 

(.5*mean(yhat[, 2])+1)^(1/.5) # 66.97 - com. ideocracy
((.5*mean(yhat[, 2])+1)^(1/.5) - mu)/mu # -9.5

(.5*mean(yhat[, 3])+1)^(1/.5) # 91.82 - oneparty
((.5*mean(yhat[, 3])+1)^(1/.5) - mu)/mu # 24.0

p <- ggplot(
  data = tmp,
  aes(
    x = reorder(regime_type, mu), 
    y = mu, ymin = lower, ymax = upper
  )
) +
  geom_hline( # hide major tick line
    yintercept = mu,
    size = .3, colour = 'white'
  ) +
  geom_hline(
    yintercept = mu,
    size = .3, linetype = 'dashed'
  ) +
  geom_pointrange() +
  scale_x_discrete(labels = c(
    'ideocracy' = 'Kommunistische\nIdeokratie',
    'monarchy' = 'Monarchie',
    'electoral' = 'Elektorale\nAutokratie',
    'military' = 'Militär-\nautokratie',
    'personalist' = 'Personalistische\nAutokratie',
    'oneparty' = 'Einpartei-\nautokratie'
    )
  ) +
  labs(y = 'Säuglingssterblichkeit pro 1.000 Lebendgeburten') +
  scale_y_continuous(
    limits = c(40, 120),
    breaks = c(40, 60, mu, 80, 100, 120),
    labels = c('40', '60', expression(E(hat(y)) %~~% 74), '80', '100', '120')
  ) +
  theme_minimal(base_family = 'CMU Sans Serif') +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_blank(),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  )
options(OutDec = ',')
ggsave(
  plot = p,
  file = file.path(pathOut, 'dotchart_substantiveEffect_infantMortality.png'),
  width = plot_size, height = plot_size/1.618,
  dpi = 1200
)
# housekeeping =============================================
for(i in rev(hlm_packages)){detach(paste0('package:', i), character.only = TRUE)}
options(OutDec = '.')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
