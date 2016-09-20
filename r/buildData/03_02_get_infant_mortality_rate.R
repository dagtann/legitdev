# Download WDI Infant Mortality Rate / 1000 ================
#   (% of population) ======================================
if('WDI' %in% installed.packages()[, 'Package'] == FALSE){
  install.packages('WDI', dependencies = TRUE)
  library('WDI')  
} else {
  library('WDI')
}

# Download data --------------------------------------------
# Uncomment to replicate indicator selection
# wdi_poverty <- WDI::WDIsearch(
#   string = "Infant Mortality", field = "name", short = FALSE,
#   cache = WDIcache()
# )
# write.csv(wdi_poverty,
#   file.path(pathOut, 'wdi_infantMortality_searchResults.csv'),
#   row.names = FALSE
# )
# NOTE: Retrieved indicator name not found in database
#   looked up at
#   "http://databank.worldbank.org/data/reports.aspx?source=2&series=SP.DYN.IMRT.IN&country="
#   on September 20, 2016

# if not already downloaded
filename <- 'wdi_infant_mortality.RData'
if(filename %in% dir(file.path(pathData)) == FALSE){
  infant_mortality <- WDI(
    country = unique(
      countrycode::countrycode(
        base$cowcode, origin = 'cown', destination = 'iso2c'
      )
    ),
    indicator = c('SP.DYN.IMRT.IN'),
    start = min(base$year), end = max(base$year), extra = FALSE
  )
  save(infant_mortality, file = file.path(pathData, filename))
} else {
  load(file.path(pathData, filename))
}
# countrycode::countrycode(
#   c('CS', 'YU', 'TW'), # Data missing on countries
#   origin = 'iso2c', destination = 'country.name'
# ) # not listed in WDI
infant_mortality <- within(infant_mortality,
  cowcode <- countrycode::countrycode(
    iso2c, "iso2c", "cown", warn = TRUE
  )
)

# Rudimentary tests on panel structure ---------------------
if(any(is.na(infant_mortality$cowcode))){ ## All cowcodes valid?
  stop("Error in infant_mortality: Some cowcodes are NA")
}
if( ## Any non-unique entries in WDI?
  with(infant_mortality, sum(duplicated(paste(cowcode, year, sep = ':')))) > 0
){
  stop("Error in infant_mortality: Some cowcode:year not unique")
}

# Join data frames -----------------------------------------
infant_mortality <- select(infant_mortality, cowcode, year, SP.DYN.IMRT.IN)
base <- left_join(base, infant_mortality, by = c('cowcode', 'year'))

# housekeeping =============================================
save(
  x = infant_mortality,
  file = file.path(pathOut, 'wdi_infant_mortality.RData')
)
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
detach(package:WDI)
## END
