# Load share of religious denomination in 1980 data ========
qog <- collect(
  tbl(qog_db, sql("
    SELECT ccodecow AS cowcode, year, lp_catho80,
      lp_muslim80, lp_protmg80, lp_no_cpm80
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