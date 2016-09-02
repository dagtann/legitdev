## Generate a csv to be shared with my coauthors
## Dag Tanneberg, 2016/09/02. ==============================
out <- select(
  base,
  cowcode, year, regime_type, spell_id, spell_no,
  start_year, end_year, regime_change, SI.POV.DDAY
)
write.csv(
  out,
  file = file.path(
    pathOut,
    'data_legitimacy_development_ver2016-09-02.csv'
  ),
  row.names = FALSE
)
## NOTE: File symlinked to ~/Dropbox/publikation/2016/zfvp/
# Housekeeping =============================================
rm(out)
## END