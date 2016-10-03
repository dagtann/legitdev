# Load Maddison economic development data
# Generate growth rate in gdp per capita
# Merge indicators into base ===============================
# load econ development data -------------------------------
qog <- collect(
  tbl(qog_db, sql("
    SELECT ccodecow, year, mad_gdppc
    FROM qog
    "
    )
  )
)
qog <- rename(qog, cowcode = ccodecow) %>%
  filter(!is.na(cowcode))   ## remove unidentified countries
# generate vars --------------------------------------------
qog <- group_by(qog, cowcode) %>%
  mutate(lag_mad_gdppch = lag(mad_gdppc, order_by = year)) %>%
  mutate(
    growth_mad_gdppch = 100 * (mad_gdppc-lag_mad_gdppch)/lag_mad_gdppch
  ) %>%
  ungroup(qog)
summary(qog[, 'growth_mad_gdppch'])            # in [-1, 1]?
with(qog,                                     # signs match?
  sum(
    sign(mad_gdppc - lag_mad_gdppch) != sign(growth_mad_gdppch),
  na.rm = TRUE
  )
)
# merge data -----------------------------------------------
base <- left_join(base, qog, by = c('cowcode', 'year'))
# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
# END