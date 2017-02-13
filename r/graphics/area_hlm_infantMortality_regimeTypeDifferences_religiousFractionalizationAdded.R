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
    'fe_etfra', 'refra'
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
  d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military +
  refra + fe_etfra
)

# simulate expected outcome --------------------------------
n_sims <- 2000
sim_theta <- sim(hlm_regimeTypes, n_sims)
str(sim_theta)

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
  d_military    = c(0, 0, 0, 0, 1, -1),
  refra = mean(d$refra),
  fe_etfra = mean(d$fe_etfra)
)
pred_dta <- as.matrix(pred_dta)
yhat <- tcrossprod(fixef(sim_theta), pred_dta)
yhat <- apply(yhat, 2, function(y){
  out <- (.5 * y + 1)^(1/.5)    
  return(out)
  }
)
# Disregards random effects b/c intercept variance does not
# interfere with fixed effect on regimetype

delta <- cbind(
  c(yhat[, 2] - yhat[, 3]),           # ideocracy - oneparty
  c(yhat[, 1] - yhat[, 4]),           # monarchy - personalist
  c(yhat[, 1] - yhat[, 5]),           # monarchy - military
  c(yhat[, 1] - yhat[, 2]),           # monarchy - ideocracy
  c(yhat[, 1] - yhat[, 6])            # monarchy - electoral
)
summary(delta)
apply(delta, 2, function(x){1 - sum(x < 0)/length(x)})

alpha <- .1
dens_list <- lapply(
  1:ncol(delta), function(x){
  #dens <- density(delta[, x], n = 2^11)
  dens <- data.frame(
    comparison = x, #x = dens[['x']], y = dens[['y']],
    lower = quantile(delta[, x], alpha/2, names = FALSE),
    upper = quantile(delta[, x], 1-alpha/2, names = FALSE),
    mu = mean(delta[, x])
  )
  return(dens)
  }
)
dens_list <- do.call(rbind.data.frame, dens_list)
dens_list <- within(dens_list, {
  comparison <- factor(comparison,
  levels = 1:ncol(delta),
  labels = c(
    "Kommunistische Ideokratie\nim Vergleich zu\nEinparteiautokratie",
    "Personalistische Autokratie",
    "Militärautokratie",
    "Kommunistische Ideokratie",
    "Elektorale Autokratie"
    ) 
  )
  hypotheses <- ifelse(
    comparison == "Kommunistische Ideokratie\nim Vergleich zu\nEinparteiautokratie",
    'H3', 'H4: Monarchie im Vergleich zu ...'
  )
  }
)
options(OutDec = ',')
p <- ggplot(data = dens_list) +
  geom_vline(xintercept = 0, colour = 'white', size = .3) + # mask grid line
  geom_vline(xintercept = 0, linetype = 'dashed', size = .3) +
  geom_point(aes(y = reorder(comparison, mu), x = mu)) +
  geom_segment(
    aes(
      y = reorder(comparison, mu),
      yend = reorder(comparison, mu),
      x = lower, xend = upper
    )
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-50, 25), breaks = seq(-50, 25, 25)) +
  labs(x = "Differenz der vorhergesagten Säuglingssterblichkeit") +
  theme_minimal(base_family = 'CMU Sans Serif') +
  theme(
    strip.background = element_rect(fill = 'grey95', colour = 'transparent'),
    panel.border = element_rect(fill = 'transparent', colour = 'grey95'),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines'),
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = .5)
    # axis.ticks.y = element_line(colour = "grey20"),
    # axis.ticks.length = unit(12/2, "pt")
  ) +
  facet_grid(hypotheses ~ ., scales = 'free_y', space = 'free_y')
ggsave(
  plot = p,
  file = file.path(pathOut, 'pointrange_infantMortality_deltaMultipleComparisons.png'),
  width = plot_size, height = plot_size/1.618,
  dpi = 1200
)
options(OutDec = '.')
# housekeeping =============================================
for(i in rev(hlm_packages)){detach(paste0('package:', i), character.only = TRUE)}
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
