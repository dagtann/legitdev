# Configure workspace ======================================
rm(list = ls())
if(Sys.info()[['user']]=='dag'){
  pathData <- '/Users/dag/github/legitdev/data'
  pathOut <- '/Users/dag/github/legitdev/out'
  pathCode <- '/Users/dag/github/legitdev/r'
  options(help_type = 'html')
}

# load packages and fonts ----------------------------------
basicPacks <- c(
  'RSQLite', 'dplyr', 'tidyr', 'ggplot2', 'extrafont'
)
stopifnot(            # Are all required packages installed?
  any(basicPacks %in% installed.packages()[, 'Package'])
)
invisible(
  lapply(basicPacks, library, character.only = TRUE)
)
rm(basicPacks)

loadfonts(quiet=TRUE)

plot_size <- 7

# set convenience hooks ====================================
cleanWorkSpace <- c(ls(), 'cleanWorkSpace')

# Execute specialized scripts ==============================
## Build data ----------------------------------------------
for(i in dir(file.path(pathCode, 'buildData'))){
  source(file.path(pathCode, 'buildData', i))
}
save.image(file.path(pathOut, 'base.RData'))

## Run analysis ============================================
source( # transform variables, difference coding regime types
  file.path(pathCode, 'analysis', '01_setupAnalysisData.R')
)
# Data sharing ---------------------------------------------
# Uncomment to generate an up-to-date version of the dataset
# on the fly
# write.csv2(
#   base,
#   file = file.path(
#     pathOut,
#     paste0('data_legitimacy_development_ver', Sys.Date(), '.csv')
#   ),
#   row.names = FALSE
# )
# foreign::write.dta(
#   base,
#   file = file.path(
#     pathOut,
#     paste0('data_legitimacy_development_ver', Sys.Date(), '.dta')
#   ),
#   convert.factors = 'string'
# )
## END