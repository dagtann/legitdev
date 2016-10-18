# Build lower level regression model =======================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# functions ------------------------------------------------
inv_boxcox <- function(x, lambda = .5){
  if(lambda == 0){
    exp(x)
  } else {
    (lambda * x + 1)^(1 / lambda)
  }
}

# constants ------------------------------------------------
n_sims <- 2000

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 'cowcode',
    't_lin', 't_squ', 't_cub', 't_qud',
    "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    'fe_etfra'
  )
)
d <- na.omit(d)

hlm_baseline <- lmer(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id),
  data = d, REML = FALSE
)
hlm_varyingControls <- update(hlm_baseline,
  . ~ . +
  lag_mad_gdppch + # state of economy
  lag_wdi_agrvagdp + # structure of economy
  lag_ross_oil_value_2000 + lag_ross_gas_value_2000 # rents
)

# sim_theta <- sim(hlm_varyingControls, n_sims)
# pred_dta <- model.frame(
#   cbind(intercept = 1,
#     d[, c('t_lin', 't_squ', 'lag_mad_gdppch', 'lag_wdi_agrvagdp',
#       'lag_ross_oil_value_2000', 'lag_ross_gas_value_2000')
#     ]
#   )
# )

d[, 'yhat'] <- predict(hlm_varyingControls)
d <- within(d, {
  y <- inv_boxcox(infant_mortality)
  yhat <- inv_boxcox(yhat)
  }
)

library(countrycode)
d[, 'country_name'] <- countrycode(d[['cowcode']], 'cown', 'country.name')
detach(package:countrycode)

sample_spells <- sample(unique(d$cowcode), 12, replace = FALSE)

ggplot(data = d, # subset(d, cowcode %in% sample_spells),
  aes(x = t_lin*10+1990, y = y)) +
  geom_point(size = .7) +
  geom_line(aes(y = yhat, group = spell_id),  alpha = 1, colour = 'red') +
  labs(
    y = "Säuglingssterblichkeit pro 1.000 Lebendgeburten",
    title = "Geschätzte Wachstumskurven\n(Zufällige Länderauswahl)"
  ) +
  facet_wrap(~ country_name) +
  theme_minimal(base_family = 'CMU Sans Serif') +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = .6 * 12), 
    strip.background = element_rect(fill = 'grey95', colour = 'transparent'),
    panel.border = element_rect(fill = 'transparent', colour = 'grey95')
  )