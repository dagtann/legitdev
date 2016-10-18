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
    "lag_mad_gdppch",
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
  lag_mad_gdppch + # state of economy
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
  lag_mad_gdppch =          mean(d$lag_mad_gdppch),
  lag_wdi_agrvagdp =        mean(d$lag_wdi_agrvagdp),
  lag_ross_oil_value_2000 = mean(d$lag_ross_oil_value_2000),
  lag_ross_gas_value_2000 = mean(d$lag_ross_gas_value_2000),
  d_monarchy    = c(1, 0, 0, 0, 0, -1),
  d_ideocracy   = c(0, 1, 0, 0, 0, -1),
  d_oneparty    = c(0, 0, 1, 0, 0, -1),
  d_personalist = c(0, 0, 0, 1, 0, -1),
  d_military    = c(0, 0, 0, 0, 1, -1)
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
yhat <- apply(yhat, 2, function(y){
  out <- y + rnorm(n_sims, 0, sim_theta@sigma)
  out <- (.5 * out + 1)^(1/.5)
  return(out)
  }
)
# Disregards random effects b/c intercept variance does not
# interfere with fixed effect on regimetype

delta <- cbind(
  c(yhat[, 2] - yhat[, 3]),           # ideocracy - oneparty
  c(yhat[, 1] - yhat[, 4]),           # monarchy - personalist
  c(yhat[, 1] - yhat[, 2]),           # monarchy - ideocracy
  c(yhat[, 1] - yhat[, 6])            # monarchy - electoral
)
summary(delta)
apply(delta, 2, function(x){1 - sum(x < 0)/length(x)})
alpha <- .10
dens_list <- lapply(
  1:ncol(delta), function(x){
  dens <- density(delta[, x], n = 2^11)
  dens <- data.frame(
    comparison = x, x = dens[['x']], y = dens[['y']],
    lower = quantile(delta[, x], alpha/2, names = FALSE),
    upper = quantile(delta[, x], 1-alpha/2, names = FALSE)
  )
  return(dens)
  }
)
dens_list <- do.call(rbind.data.frame, dens_list)
dens_list <- within(dens_list,
  comparison <- factor(comparison,
  levels = 1:ncol(delta),
  labels = c(
    "(1) Kommunistische Ideokratie - Einparteiautokratie",
    "(2) Monarchie - Personalistische Autokratie",
    "(3) Monarchie - Kommunistische Ideokratie",
    "(4) Monarchie - Elektorale Autokratie"
  ) 
  )
)
options(OutDec = ',')
p <- ggplot(data = dens_list) +
  geom_vline(xintercept = 0, colour = 'white', size = .3) + # mask grid line
  geom_vline(xintercept = 0, linetype = 'dashed', size = .3) +
  geom_line(aes(x = x, y = y), size = .3) +
  geom_area(
    data = subset(dens_list, x >= lower & x <= upper),
    aes(x = x, y = y), alpha = .4
  ) +
  scale_x_continuous(breaks = seq(-75, 75, 25)) +
  labs(y = "Dichte", x = "Differenz der vorhergesagten Säuglingssterblichkeit") +
  theme_minimal(base_family = 'CMU Sans Serif') +
  facet_wrap(~comparison) +
  theme(
    # panel.grid.major = element_line(colour = 'grey85'),
    # panel.grid.minor = element_line(colour = 'grey90'),
    strip.background = element_rect(fill = 'grey95', colour = 'transparent'),
    panel.border = element_rect(fill = 'transparent', colour = 'grey95'),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  )
ggsave(
  plot = p,
  file = file.path(pathOut, 'area_infantMortality_deltaMultipleComparisons.png'),
  width = plot_size, height = plot_size/1.618,
  dpi = 300
)
options(OutDec = '.')
# housekeeping =============================================
for(i in rev(hlm_packages)){detach(paste0('package:', i), character.only = TRUE)}
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
