# This script identifies unique spells of authoritarian
# regimes within countries. ================================

# Each authoritarian regime has a distinct type and
# types do not change over time. Therefore, the data
# can be meaningfully grouped into distinct authoritarian
# regimes of unique type. This script recovers unique
# authoritarian spells from the data. Identification is
# conditioned on variable start_year.

# Identify panel entries -----------------------------------
base <- base[with(base, order(cowcode, year)), ]
base <- group_by(base, cowcode)
base <- mutate(
  base, regime_change = ifelse(year == start_year, 1, 0)
) %>%
  mutate(spell_no = cumsum(regime_change)) %>%
  mutate(spell_id = factor(paste(cowcode, spell_no, sep = ':')))
base <- ungroup(base)

# Uncomment to view coding suspicious regime counts
# base <- within(base, {
#   min_year <- ave(year, spell_id, FUN = min)
#   max_year <- ave(year, spell_id, FUN = max)
#   discrepancy <- ifelse(start_year != min_year | end_year != max_year, 1, 0)
#   }
# )
# summary(base) # discrepancies all 0
# View(base[base$discrepancy == 1, ])
# Problem solved after resubmission of original regime type data

# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
## END