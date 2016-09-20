# Generate data.frame for analyses =========================

# Drop irrelevant vars
analysis <- subset(
  base,
  select = c(
    "cowcode", "year", "regime_type", "spell_id",
    'start_year', 'end_year', "SI.POV.DDAY", 
    "lag_mad_gdppch", "growth_mad_gdppch", "wdi_agrvagdp", "wdi_pop65",
    "fe_etfra", "ross_gas_value_2000", "ross_oil_value_2000", "region"
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
    'cowcode', 'year', 'region', 'regime_type', 'spell_id', 'fe_etfra',
    'start_year', 'end_year', 'SI.POV.DDAY', 'growth_mad_gdppch',
    names(analysis)[grep(pattern = 'lag', x = names(analysis), fixed = TRUE)]
  )
)

# generate process indicators
# analysis <- data.frame(  # create & attach cubic t to data.frame
#   analysis, poly(analysis[['year']] - 1990, degree = 3)[, 1:3]
# )
# names(analysis)[(ncol(analysis)-2):ncol(analysis)] <- c('t_lin', 't_squ', 't_cub')
analysis <- within(analysis, {
  t_lin <-  (year - 1990)/10 
  t_squ <- t_lin^2
  t_cub <- t_lin^3 # NOTE 1
  dur_lin <- year - start_year
  dur_squ <- dur_lin^2
  dur_cub <- dur_lin^3/1000 # NOTE 1
  postColdWar <- ifelse(start_year >= 1990, 1, 0)
  }
)

# manual dummy difference coding
# NOTE: When using deviation coding R drops the -1 category.
#   Interpretation/communication of results becomes more
#   difficult.
# library('car')     # uncomment to generate reference table
# x <- base$regime_type
# contrasts(x) <- 'contr.Sum'
# contrasts(x)
# rm(x); detach(package:car)
#             [S.Electoral] [S.Military] [S.Monarchy] [S.Oneparty] [S.Ideocracy]
# Electoral               1            0            0            0             0
# Military                0            1            0            0             0
# Monarchy                0            0            1            0             0
# Oneparty                0            0            0            1             0
# Ideocracy               0            0            0            0             1
# Personalist            -1           -1           -1           -1            -1
analysis <- within(analysis, {
  # NOTE: Changed reference to Electoral b/c most obs in dta
  d_military <- ifelse(regime_type == 'Military', 1, 
    ifelse(regime_type == 'Electoral', -1, 0)
  )
  d_personalist  <- ifelse(regime_type == 'Personalist', 1, 
    ifelse(regime_type == 'Electoral', -1, 0)
  )
  d_oneparty  <- ifelse(regime_type == 'Oneparty', 1, 
    ifelse(regime_type == 'Electoral', -1, 0)
  )
  d_ideocracy  <- ifelse(regime_type == 'Ideocracy', 1, 
    ifelse(regime_type == 'Electoral', -1, 0)
  )
  d_monarchy  <- ifelse(regime_type == 'Monarchy', 1, 
    ifelse(regime_type == 'Electoral', -1, 0)
  )
  }
)
# summary(analysis[, grep('d_', names(analysis), fixed = TRUE)])

# transform dv
summary(analysis$SI.POV.DDAY)
analysis <- within(analysis,
  absolute_poverty <- boot::logit(SI.POV.DDAY/100+.001)
)

# summarize results
str(analysis)
summary(analysis)

# Housekeeping =============================================
save.image(file = file.path(pathOut, 'analysis.RData'))
cleanWorkSpace <- c(cleanWorkSpace, 'analysis')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
## END
# NOTE 1: numeric stability, c.f. Carter & Signorino 2010: 283