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

plot.size <- 7

# set convenience hooks ====================================
cleanWorkSpace <- c(ls(), 'cleanWorkSpace')

# Execute specialized scripts ==============================

## Build data ----------------------------------------------
source(file.path(pathCode, 'buildData', '01_extractRegimeTypeData.R'))
source(file.path(pathCode, 'buildData', '02_id_panel_structure.R'))
source(file.path(pathCode, 'buildData', '03_pop_in_absolute_poverty.R'))
summary(base)
## END