# Extract intercept deviates                   =============
#rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
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
hlm_allControls <- update(hlm_regimeTypes,
  . ~ . +
  lp_catho80 + lp_muslim80 + lp_protmg80 +
  d_eap + d_eca + d_lac + d_mena + d_sa +
  fe_etfra
)
# simulate intercept deviates ==============================
n_sims <- 2000
sim_list <- lapply(
  list(hlm_varyingControls, hlm_regimeTypes, hlm_allControls),
  sim, n.sims = n_sims
)
intercept_deviates <- lapply(sim_list,
  function(x){ranef(x)[['spell_id']][, , 1]}
)
intercept_deviates <- do.call(rbind.data.frame, intercept_deviates)
intercept_deviates[['model']] <- rep(1:3, each = n_sims)

# retrieve information for plot ----------------------------
mu <- aggregate(intercept_deviates[, -192],
  by = list(model = intercept_deviates[, 'model']),
  mean
)
mu <- gather(mu, 'spell_id', 'mu', 2:192)
mu <- mu[with(mu, order(model, mu)), ]
mu <- within(mu, intercept_order <- ave(mu, model, FUN = seq_along))
upper <- aggregate(intercept_deviates[, -192],
  by = list(model = intercept_deviates[, 'model']),
  quantile, .95, names = FALSE
)
upper <- gather(upper, 'spell_id', 'upper', 2:192)
lower <- aggregate(intercept_deviates[, -192],
  by = list(model = intercept_deviates[, 'model']),
  quantile, .05, names = FALSE
)
lower <- gather(lower, 'spell_id', 'lower', 2:192)

pdta <- inner_join(mu, upper, by = c('model', 'spell_id'))
pdta <- inner_join(pdta, lower, by = c('model', 'spell_id'))
pdta <- within(pdta, model <- factor(model, 1:3, labels = c('Referenz', 'Regimetypen', 'Voll')))

# plot data ------------------------------------------------
p <- ggplot(
  data = pdta, 
  aes(x = intercept_order, 
    y = mu, ymin = lower, ymax = upper
  )
) +
  geom_smooth(method = 'lm', colour = 'black', se = FALSE, linetype = 'dashed', size = .3) +
  geom_point(size = .2, alpha = .6) +
  geom_linerange(size = .1, alpha = .6) +
  labs(y = 'Interceptabweichung', title = '(a) Säuglingssterblichkeit pro 1.000 Lebendgeburten') +
  facet_grid(~ model) +
  theme_minimal(base_family = 'CMU Sans Serif') +
  theme(
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  )
ggsave(plot = p, file = file.path(pathOut, 'caterpillar_hlm_InfantMortality_allModels.png'),
  width = plot_size, height = plot_size/1.618, dpi = 300
)
# housekeeping =============================================
for(i in rev(hlm_packages)){detach(paste0('package:', i), character.only = TRUE)}
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])