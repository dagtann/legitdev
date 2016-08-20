# This script pulls regime type data from the original
# manuscript DS and converts into a clean format.
# provide core dataset head ================================
library('foreign')
original_data <- read.dta(                    # stata import
  file.path(
    pathData,
    'Kailitz, Wurster, Autocratic Regime legitimation and social policy outcomes april 2016 kai.dta'
  )
)
stata_import_attributes <- c(
  "datalabel", "time.stamp", "formats", "types",
  "val.labels", "var.labels", "version", "label.table",
  "expansion.table", "expansion.fields"
)
attributes(original_data)[stata_import_attributes] <- NULL
detach(package:foreign)

base <- select(
  original_data,
  cowcode, year, d_electoralautocracy,
  d_militaryregime, d_monarchy, d_onepartyautocracy,
  d_communistideocracy, d_personalistregime, AnteilabsolutArme
)
rm(original_data)

# preliminary tests on data structure ======================
with(base,                            # panel uniquely id'd?
  table(duplicated(paste(cowcode, year, sep = ':')))
) # Yes
                            # each panel single regime type?
regime_type_dummies <- grep(pattern = 'd_', names(base), fixed = TRUE)
summary(rowSums(base[, regime_type_dummies]))
# max 1, but unexpected NA's
summary(base[regime_type_dummies]) # identical # of NA's
write.csv2(base, file.path(pathOut, 'base.csv'), row.names = FALSE)
# NA's == democracy
base <- filter(base, !is.na(d_electoralautocracy))

# convert regime types to factor ===========================
base <- within(base, {
  regime_type <- ifelse(
    d_electoralautocracy == 1, 1, ifelse(
      d_militaryregime == 1, 2, ifelse(
        d_monarchy == 1, 3, ifelse(
          d_onepartyautocracy == 1, 4, ifelse(
            d_communistideocracy == 1, 5, ifelse(
              d_personalistregime == 1, 6, NA
            )
          )
        )
      )
    )
  )
  regime_type <- factor(regime_type,
    levels = sort(unique(regime_type)),
    labels = c(
      'Electoral', 'Military', 'Monarchy', 'Oneparty',
      'Ideocracy', 'Personalist'
    ),
    ordered = FALSE
  )
  }
)
table(base$regime_type[base$year %in% 1960:2010])
# matches 1960 - 2010 raw counts in manuscript
base <- select(base, cowcode, year, regime_type, AnteilabsolutArme)

# Housekeeping =============================================
cleanWorkSpace <- c(cleanWorkSpace, 'base')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
## END