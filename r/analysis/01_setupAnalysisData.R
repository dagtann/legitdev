# Generate data.frame for analyses =========================

# Drop irrelevant vars
analysis <- subset(
  base,
  select = c(
    "cowcode", "year", "regime_type", "spell_id",
    'start_year', 'end_year', "SI.POV.DDAY", 
    "lag_mad_gdppch", "growth_mad_gdppch", "wdi_agrvagdp", "wdi_pop65",
    "fe_etfra", "ross_gas_value_2000", "ross_oil_value_2000"
  )
)

# adjust scales of % vars
for(i in c('wdi_agrvagdp', 'wdi_pop65', 'SI.POV.DDAY')){
  analysis[, i] <- analysis[, i] / 100
}

# transform skewed vars
for(i in c("lag_mad_gdppch", "ross_gas_value_2000", "ross_oil_value_2000")){
  analysis[, i] <- log(analysis[, i]+1, base = 10)
}

# generate lags
analysis <- group_by(analysis, cowcode) %>%
  mutate(lag_wdi_agrvagdp = lag(wdi_agrvagdp, order_by = year)) %>%
  mutate(lag_wdi_pop65 = lag(wdi_pop65, order_by = year)) %>%
  mutate(lag_ross_oil_value_2000 = lag(ross_oil_value_2000, order_by = year)) %>%
  mutate(lag_ross_gas_value_2000 = lag(ross_gas_value_2000, order_by = year))
analysis <- ungroup(analysis)
analysis <- subset(analysis, 
  select = c(
    'cowcode', 'year', 'regime_type', 'fe_etfra',
    'start_year', 'end_year', 'SI.POV.DDAY', 'growth_mad_gdppch',
    names(analysis)[grep(pattern = 'lag', x = names(analysis), fixed = TRUE)]
  )
)

# generate process indicators
analysis <- within(analysis, {
  t_lin <-  year - 1990
  t_squ <- t_lin^2
  t_cub <- t_lin^3/1000 # NOTE 1
  dur_lin <- year - start_year
  dur_squ <- dur_lin^2
  dur_cub <- dur_lin^3/1000 # NOTE 1
  }
)

# transform dv
summary(analysis$SI.POV.DDAY)
analysis <- within(analysis,
  absolute_poverty <- boot::logit(SI.POV.DDAY+.01)
)

str(analysis)

# Housekeeping =============================================
save.image(file = file.path(pathOut, 'analysis.RData'))
cleanWorkSpace <- c(cleanWorkSpace, 'analysis')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
## END
# NOTE 1: numeric stability, c.f. Carter & Signorino 2010