library(lme4)

d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 'year',
    't_lin', 't_squ',
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

# formula objects ------------------------------------------
formula_list <- list(
  varying_controls = formula(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
    lag_mad_gdppch + # state of economy
    lag_wdi_agrvagdp + # structure of economy
    lag_ross_oil_value_2000 + lag_ross_gas_value_2000
  ),
  regime_types = formula(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
    lag_mad_gdppch + # state of economy
    lag_wdi_agrvagdp + # structure of economy
    lag_ross_oil_value_2000 + lag_ross_gas_value_2000 +
    d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military
  ),
  all_controls = formula(
  infant_mortality ~ t_lin + t_squ + (t_lin + t_squ | spell_id) +
    lag_mad_gdppch + # state of economy
    lag_wdi_agrvagdp + # structure of economy
    lag_ross_oil_value_2000 + lag_ross_gas_value_2000 +
    d_monarchy + d_ideocracy + d_oneparty + d_personalist + d_military +
    lp_catho80 + lp_muslim80 + lp_protmg80 +
    d_eap + d_eca + d_lac + d_mena + d_sa +
    fe_etfra  
  )
)

observation_years <- sort(unique(base[['year']]))
observation_years <- observation_years[which(observation_years %in% 1960:2010)]

library('parallel')
num_workers <- detectCores()-1
sigma_e_a <- mclapply(
  formula_list, function(f, data = d){
    vapply(observation_years, FUN = function(i){
      data[, 't_lin'] <- (data[, 'year'] - i) / 10
      data[, 't_squ'] <- data[, 't_lin']^2
      mod <- lmer(f, data = data, REML = FALSE)
      out <- c(
        year = i,
        sigma_e = sigma(mod)^2,
        sigma_alpha = VarCorr(mod)[['spell_id']][1, 1],
        icc = VarCorr(mod)[['spell_id']][1, 1] /
          (VarCorr(mod)[['spell_id']][1, 1] + sigma(mod)^2)
      )
      return(out)
      },
      numeric(4)
    )
  }
)

pdf(file = file.path(pathOut, 'line_vpc_infantMortality.pdf'),
  width = plot_size, height = plot_size/1.618
)
options(OutDec = ',')
plot(
  observation_years, seq(.99, 1, length.out = length(observation_years)),
  type = 'n', las = 1,
  xlab = "Beobachtungsjahr",
  ylab = "Variance partititon coefficient (VPC)",
  main = "Erklärungsvorteil bei\nsteigender Modellkomplexität",
  bty = 'n'
)
colours <- c('#1b9e77', '#d95f02', '#7570b3')
  #'#a6cee3', '#1f78b4', '#b2df8a')
for(i in 1:length(sigma_e_a)){
  lines(sigma_e_a[[i]][1, ], sigma_e_a[[i]][4, ], col = colours[i])
}
legend(
  x = 'bottomright',
  legend = c('Referenzmodell', ' Referenz mit Typen', 'Alle Kontextfaktoren'),
  col = colours, pch = 19, bty = 'n', horiz = TRUE
)
dev.off()