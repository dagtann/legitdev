# Download WDI Poverty headcount ratio at $2 a day =========
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
#   string = "Poverty Headcount", field = "name", short = FALSE,
#   cache = WDIcache()
# )
# write.csv(wdi_poverty,
#   file.path(pathOut, 'wdi_poverty_searchResults.csv'),
#   row.names = FALSE
# )

filename <- 'wdi_poverty_headcount.RData'
if(filename %in% dir(file.path(pathData)) == FALSE){
  poverty <- WDI(
    country = unique(
      countrycode::countrycode(
        base$cowcode, origin = 'cown', destination = 'iso2c'
      )
    ),
    indicator = c(
      'SI.POV.25DAY', 'SI.POV.2DAY', 'SI.POV.4DAY', 
      'SI.POV.5DAY', 'SI.POV.DDAY'
      # Poverty headcount ratio at $2.5 a day (PPP) (% of population)
      # Poverty headcount ratio at $3.10 a day (2011 PPP) (% of population)
      # Poverty headcount ratio at $4 a day (PPP) (% of population)
      # Poverty headcount ratio at $5 a day (PPP) (% of population)
      # Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
      # Downloads .2DAY & DDAY
    ),
    start = min(base$year), end = max(base$year), extra = TRUE
  )
  save(poverty, file = file.path(pathData, filename))
} else {
  load(file.path(pathData, filename))
}
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

# Update region data
table(poverty[['region']], exclude = NULL)   # 102 NA values
table(poverty[['iso2c']][is.na(poverty[['region']])])
# KP == Korea, North | CV == Cabo Verde
poverty <- within(poverty, {
  region <- as.character(region)
  region <- ifelse(
    is.na(region) & iso2c == 'KP',
    'East Asia & Pacific (all income levels)',
    region
  )
  region <- ifelse(
    is.na(region) & iso2c == 'CV',
    'Sub-Saharan Africa (all income levels)',
    region
  )
  region <- as.factor(region)
  }
)
table(base$region)
# Join data frames -----------------------------------------
poverty <- select(
  poverty, cowcode, year, region, SI.POV.2DAY, SI.POV.DDAY
)
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
