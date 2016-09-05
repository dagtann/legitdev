# Add share agriculture in GDP to data =====================
# load data from qog ---------------------------------------
qog <- collect(
  tbl(qog_db, 
    sql("
      SELECT ccodecow AS cowcode, year, wdi_agrvagdp
      FROM qog
    "
    )
  )
)
# merge to base data ---------------------------------------
base <- left_join(base, qog, by = c('cowcode', 'year'))
# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])