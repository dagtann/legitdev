# This script identifies unique spells of authoritarian
# regimes within countries. ================================

# Authoritarian regimes are defined as:
# Each authoritarian regimes has a distinct regime type and
# regime types do not change over time. Therefore, the data
# can be meaningfully grouped into distinct authoritarian
# regimes that carry unique regime types. To uncover unique
# authoritarian spells this script identifies each
# authoritarian regime in the data. Identification is conditioned
# on variable start_year.

# Identify panel entries -----------------------------------
base <- base[with(base, order(cowcode, year)), ]
base <- within(base,               # fix coding burkina faso
  start_year <- ifelse(
    cowcode == 439 & year %in% 1983:1990, 1983, start_year
  )
)
base <- group_by(base, cowcode)
base <- mutate(base, regime_change = ifelse(year == start_year, 1, 0)) %>%
  mutate(spell_no = cumsum(regime_change)) %>%
  mutate(spell_id = factor(paste(cowcode, spell_no, sep = ':')))
base <- ungroup(base)

# Uncomment to View coding suspicious regime counts
# base <- within(base, {
#   min_year <- ave(year, spell_id, FUN = min)
#   max_year <- ave(year, spell_id, FUN = max)
#   discrepancy <- ifelse(start_year != min_year | end_year != max_year, 1, 0)
#   }
# )
# summary(base)
# View(base[base$discrepancy == 1, ])

# basic descriptives ---------------------------------------
# length(unique(base$spell_id))         # 360 regimes in total
# with(base, range(ave(year, spell_id, FUN = seq_along))) # 1-65 years

# tmp <- aggregate(                 # plot obs freq by country
#   rep(1, nrow(base)) ~ cowcode + spell_no, data = base, FUN = length
# )
# names(tmp)[3] <- 'count'
# tmp <- within(tmp, {total <- ave(count, cowcode, FUN = sum)})
# qplot(data = tmp, 
#   x = reorder(factor(cowcode), total), xend = reorder(factor(cowcode), total), y = 0, yend = total, geom = 'segment'
# )
# rm(tmp)

# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
## END