# Establish an SQL link to latest Quality of Government data
# archive ==================================================
# Requires an sqlite version of the QoG data which was
# generated for a different project.
qog_db <- src_sqlite(
  file.path(pathData, 'qogJan16.sqlite'), create = FALSE
)
cleanWorkSpace <- c(cleanWorkSpace, 'qog_db')
# END