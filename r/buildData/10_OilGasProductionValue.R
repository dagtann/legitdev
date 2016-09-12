# Ross 1932 - 2014 oil & gas data (resource dependence) ====
qog <- collect(
  tbl(qog_db, sql("
    SELECT ccodecow AS cowcode, year, ross_gas_value_2000,
      ross_oil_value_2000
    FROM qog"
    )
  )
)
# merge data -----------------------------------------------
base <- left_join(
  base, qog, by = c('cowcode', 'year')
)
# housekeeping =============================================
rm(qog)               # keep qog_db link for quick reference
## END