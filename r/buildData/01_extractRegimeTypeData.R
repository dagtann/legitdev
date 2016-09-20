# This script pulls regime type data from the original
# manuscript DS and converts into a clean format.
# provide core dataset head ================================
library('foreign')
original_data <- read.dta(                    # stata import
  file.path(pathData, '2016-09-20_regimeCodings.dta')
)
stata_import_attributes <- c(
  "datalabel", "time.stamp", "formats", "types",
  "val.labels", "var.labels", "version", "label.table",
  "expansion.table", "expansion.fields"
)
attributes(original_data)[stata_import_attributes] <- NULL
detach(package:foreign)

valid_entries <- c(  # Drop DEM, transition, civil war, etc.
  "Monarchy", "Personalist Regime", "Electoral Autocracy",
  "Communist Ideocracy", "One-party Autocracy",
  "Military Regime"
)
original_data <- subset(
  original_data, regkai %in% valid_entries
)
table(original_data$regkai)

library('countrycode')
original_data <- within(original_data, {
  cowcode <- countrycode(country, "country.name", "cown", warn = TRUE)
  cowcode <- ifelse(country == "Serbia and Montenegro", 345, cowcode)
  }
)
# Single warning
# View(original_data[original_data$country == 'Serbia and Montenegro', ])
# Serbia & Montegro 1993-1999, recode cowcode to 345
detach(package:countrycode)

base <- select(
  original_data, cowcode, year, 
  d_electoralautocracy, d_militaryregime, d_monarchy,
  d_onepartyautocracy, d_communistideocracy,
  d_personalistregime, start_year, end_year
)
rm(original_data, valid_entries)

# preliminary tests on data structure ======================
with(base,                            # panel uniquely id'd?
  table(duplicated(paste(cowcode, year, sep = ':')))
) # Yes

       # each panel single regime type? (max on rowsum == 1)
regime_type_dummies <- grep(
  pattern = '^d_', names(base), value = TRUE
)
summary(rowSums(base[, regime_type_dummies]))          # Yes

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
base <- select(base, cowcode, year, regime_type,
  start_year, end_year
)

# add country region dummies ===============================
library(countrycode)
base <- within(base, {
  region <- countrycode(cowcode, 'cown', 'region', warn = TRUE)
  }
)
detach(package:countrycode)
# Housekeeping =============================================
cleanWorkSpace <- c(cleanWorkSpace, 'base')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
## END