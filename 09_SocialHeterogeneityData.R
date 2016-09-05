# Load social heterogeneity information ====================
# Load data ------------------------------------------------
# % population aged 65 and older, ethnic fractionalization
qog <- table(
  collect(
    sql("
      SELECT ccodecow AS cowcode, year, 'wdi_pop65', 'fe_etfra'
      FROM qog"
    )
  )
)
# merge data -----------------------------------------------
base <- left_join(base, qog, by = c('cowcode', 'year'))
# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])