# Load additional absolute poverty data sources ============
# Includes:
#   1. UN $2/day poverty headcount
#   2. Older WDI poverty headcounts for comparison
qog <- collect(
  tbl(qog_db, sql("
    SELECT ccodecow, year, une_pov, wdi_povhc190, wdi_povhc310
    FROM qog
    "
    )
  )
)
# check cow compatibility ----------------------------------
# setdiff(base$cowcode, qog$ccodecow) # ctries not covered?
# setdiff(                   # ctries and years not coverered?
#   with(base, paste(cowcode, year, sep = ':')),
#   with(qog, paste(ccodecow, year, sep = ':'))
# )
# subset qog for merge -------------------------------------
qog <- rename(qog, cowcode = ccodecow)

# merge data -----------------------------------------------
base <- left_join(
  base, qog, by = c('cowcode', 'year')
)
# housekeeping =============================================
rm(qog)               # keep qog_db link for quick reference