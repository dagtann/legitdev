# Load Haber & Menaldo resource reliant dummy
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

# Merge data ===============================================
haber_merge <- select(haber, cowcode, year, Fiscal_Reliance)
haber_merge <- filter(
  haber_merge, year >= 1950 & !is.na(cowcode)
)

with(haber_merge,                            # panel unique?
  table(duplicated(paste(cowcode, year, sep = ':')))
)
haber_merge <- haber_merge[-10377, ]
# TANZANIA 2007 double entry with completely NA
setdiff(unique(base$cowcode), unique(haber_merge$cowcode))
# Panel entries match?
# Haber Menaldo lack Cape Verde, Seychelles, Sao Tome,
# Yemen (North)

base <- left_join(base, haber_merge, by = c('cowcode', 'year'))

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])

# haber_errata <- read.csv2(
#   file.path(pathData, 'Haber_Menaldo_2011_APSR_Dataset_errata.csv'),
#   sep = ';', stringsAsFactors = FALSE, skip = 3
# )
# haber_errata <- rename(haber_errata, 
#   year = Year, hmccode = HabMenCCode, 
# )
# str(haber_errata)