qog_db <- src_sqlite(
  file.path(pathData, 'qogJan16.sqlite'), create = FALSE
)
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
# END