# Substantive effect regime types ==========================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ', 't_cub', 't_qud',
    "dur_lin", "dur_squ", 'dur_cub',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military",
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    'fe_etfra'
  )
)
d <- na.omit(d)

# hit hlm model --------------------------------------------
hlm_varyingControls <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
  growth_mad_gdppch + lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000, # rents
  data = d, REML = FALSE
)
hlm_regimeTypes <- update(
  hlm_varyingControls, . ~ . +
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military
)

# simulate expected outcome --------------------------------
n_sims <- 2000
sim_theta <- sim(hlm_regimeTypes, n_sims)

pred_dta <- data.frame(
  intercept = 1,
  t_lin = 0,
  t_squ = 0,
  growth_mad_gdppch =       0, #mean(d$growth_mad_gdppch),
  lag_mad_gdppch =          0, #mean(d$lag_mad_gdppch),
  lag_wdi_agrvagdp =        0, #mean(d$lag_wdi_agrvagdp),
  lag_ross_oil_value_2000 = 0, #mean(d$lag_ross_oil_value_2000),
  lag_ross_gas_value_2000 = 0, #mean(d$lag_ross_gas_value_2000),
  d_monarchy    = c(1, 0, 0, 0, 0, -1),
  d_ideocracy   = c(0, 1, 0, 0, 0, -1),
  d_oneparty    = c(0, 0, 1, 0, 0, -1),
  d_personalist = c(0, 0, 0, 1, 0, -1),
  d_military    = c(0, 0, 0, 0, 1, -1)
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
# Disregards random effects b/c intercept variance does not
# interfere with fixed effect on regimetype

delta <- yhat[, 3] - yhat[, 2] # oneparty - ideocracy
delta <- (.5*delta + 1)^(1/.5)
delta_dens <- density(delta)
delta_dens <- data.frame(x = delta_dens[['x']], y = delta_dens[['y']])
delta_quan <- quantile(x = delta, probs = c(.05, .95))

options(OutDec = ',')
p <- ggplot() +
  geom_vline(xintercept = 0, colour = 'white') + # mask grid line
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_area(
    data = subset(
      delta_dens, x >= delta_quan[1] & x <= delta_quan[2]
    ),
    aes(x=x, y = y), stat = 'identity', fill = 'grey', alpha = .6
  ) +
  geom_line(data = delta_dens, aes(x = x, y = y)) +
  labs(
    title = '(a) Differenz der erwarteten Säuglingssterblichkeit',
    x = expression(hat(y)['Einparteiautokratie']-hat(y)['Kommunistische Ideokratie']),
    y = 'Dichte'
  ) +
  theme_minimal(base_family = 'CMU Sans Serif') +
  theme(
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  )
ggsave(
  plot = p,
  file = file.path(pathOut, 'area_infantMortality_deltaOnepartyIdeocracy.png'),
  width = plot_size, height = plot_size/1.618,
  dpi = 300
)
options(OutDec = '.')
# housekeeping =============================================
for(i in rev(hlm_packages)){detach(paste0('package:', i), character.only = TRUE)}
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
