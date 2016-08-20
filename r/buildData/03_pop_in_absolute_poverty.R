# Download WDI Poverty headcount ratio at $2 a day =========
#   (% of population) ======================================
if('WDI' %in% installed.packages()[, 'Package'] == FALSE){
  install.packages('WDI', dependencies = TRUE)
  library('WDI')  
} else {
  library('WDI')
}

# Download data --------------------------------------------
wdi_poverty <- WDIsearch(
  string = "poverty", field = "name", short = TRUE
)
View(wdi_poverty) # Result: SI.POV.2DAY
poverty <- WDI(
  country = unique(
    countrycode::countrycode(
      base$cowcode, origin = 'cown', destination = 'iso2c'
    )
  ),
  indicator = c("SI.POV.2DAY", "SI.POV.DDAY"),
  start = min(base$year), end = max(base$year), extra = FALSE
)
# countrycode::countrycode(
#   c('CS', 'YU', 'NA', 'TW'), # Data missing on countries
#   origin = 'iso2c', destination = 'country.name'
# )
# Cross-checked with original source. All NA but few CS entries
# at <= 1969
poverty <- within(poverty,
  cowcode <- countrycode::countrycode(
    iso2c, "iso2c", "cown", warn = TRUE
  )
)

# Rudimentary tests on panel structure ---------------------
if(any(is.na(poverty$cowcode))){ ## All cowcodes valid?
  stop("Error in absolute poverty: Some cowcodes are NA")
}
if( ## Any non-unique entries in WDI?
  with(poverty, sum(duplicated(paste(cowcode, year, sep = ':')))) > 0
){
  stop("Error in absolute poverty: Some cowcode:year not unique")
}

# Join data frames -----------------------------------------
poverty <- select(poverty, cowcode, year, SI.POV.2DAY, SI.POV.DDAY)
base <- left_join(base, poverty, by = c('cowcode', 'year'))

# housekeeping =============================================
save(
  x = poverty,
  file = file.path(pathOut, 'wdi_absolutePoverty.RData')
)
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
detach(package:WDI)
## END
