# Load Haber & Menaldo resource reliant dummy ==============
# Data downloaded from
#   http://stephen-haber.com/wp-content/uploads/2014/02/
#   Haber_Menaldo_2011_APSR_Dataset.xls, last access:
#   October 20, 2016.
haber <- read.csv2(
  file.path(pathData, 'Haber_Menaldo_2011_APSR_Dataset.csv'),
  sep = ';', stringsAsFactors = FALSE
)

haber <- within(haber, {
  cnamehabmen <- ifelse(cnamehabmen == 'Pakisan', 'Pakistan', cnamehabmen)
  cnamehabmen <- ifelse(cnamehabmen == 'Serbia RB', 'Serbia', cnamehabmen)
  # Serbia historial irrelevant; no COWCODE 4 Serbia
  cnamehabmen <- ifelse(
    cnamehabmen == '' & hmccode == 360 & year == 2008,
    'Romania', cnamehabmen
  )
  cnamehabmen <- ifelse(
    cnamehabmen == '' & hmccode == 705 &  year %in% 2007:2008,
    'Kazakhstan', cnamehabmen
  )
  cowcode <- countrycode::countrycode(
    cnamehabmen, origin = 'country.name', destination = 'cown', warn = TRUE
  )
  }
)

# Merge data -----------------------------------------------
haber_merge <- select(haber,
  cowcode, cnamehabmen, year, Fiscal_Reliance
)
haber_merge <- filter(
  haber_merge, year >= 1950 & !is.na(cowcode)
)

# quick check data structure
with(haber_merge,                            # panel unique?
  table(duplicated(paste(cowcode, year, sep = ':')))
)
haber_merge <- haber_merge[-10377, ]
# TANZANIA 2007 double entry with completely NA
setdiff(unique(base$cowcode), unique(haber_merge$cowcode))
# Do panel entries match?
# Haber Menaldo missing Cape Verde, Seychelles, Sao Tome,
# Yemen (North)

base <- left_join(base, haber_merge, by = c('cowcode', 'year'))
base <- within(base, {
  cname <- countrycode::countrycode(cowcode, 'cown', 'country.name')
  discrepancy <- cnamehabmen != cname
  }
)
with(base, table(discrepancy))              # 438 mismatches
tmp <- aggregate(
  cowcode ~ cname + cnamehabmen,
  data = subset(base, discrepancy == TRUE),
  FUN = unique
)
tmp; rm(tmp)       # Mismatches refer to identical countries
drop <- which(names(base) %in% c('cnamehabmen', 'discrepancy', 'cname'))
base <- base[, -drop]; rm(drop)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])