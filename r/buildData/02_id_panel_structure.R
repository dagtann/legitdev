# This script identifies unique spells of authoritarian
# regimes within countries. ================================

# The date define authoritarian regimes as:
# Each authoritarian regimes has a distinct regime type and
# regime types do not change over time. This script tries to
# uniquely identify each authoritarian regime in the data,
# assuming:
##  a) any change in regime type indicates a new regime spell,
##  b) any within-country increment > 1 in YEAR indicates that
##    the regime has dropped out of the sample. This is only
##    possible if the regime type changes (autocracy -> democracy).

# Identify panel entries -----------------------------------
base <- group_by(base, cowcode)
base <- mutate(        # generate leads to tag regime change
    base, lead_year = lead(year, order_by = year)
  ) %>%
  mutate(delta = lead_year - year) %>%
  mutate(
    lead_regime_type = lead(regime_type, order_by = year)
  ) %>%
  mutate(
    regime_change = ifelse(
      lead_regime_type != regime_type |           # cond. a)
      delta != 1,                                 # cond. b)
      1, 0
    )
  ) %>%
  mutate( # code change 0 in last year (NAs result from lead)
    regime_change = ifelse(
      is.na(regime_change), 0, regime_change
    )
  ) %>%
  mutate(delta = ifelse(is.na(delta), 1, delta)) %>%
  mutate(spell_no = lag( # count regime changes within ctry
    cumsum(regime_change), order_by = year)
  ) %>%
  mutate(
    spell_no = ifelse(is.na(spell_no), 0, spell_no) + 1
  ) %>%
  # define spell id
  mutate(spell_id = paste(cowcode, spell_no, sep = ':')) %>%
  select(
    cowcode, year, regime_type, regime_change, spell_no,
    spell_id, AnteilabsolutArme
  )
base <- ungroup(base)

# basic descriptives ---------------------------------------
length(unique(base$spell_id))         # 357 regimes in total
with(base, range(ave(year, spell_id, FUN = seq_along))) # 1-65 years

tmp <- aggregate(                 # plot obs freq by country
  rep(1, nrow(base)) ~ cowcode + spell_no, data = base, FUN = length
)
names(tmp)[3] <- 'count'
tmp <- within(tmp, {total <- ave(count, cowcode, FUN = sum)})
qplot(data = tmp, 
  x = reorder(factor(cowcode), total), xend = reorder(factor(cowcode), total), y = 0, yend = total, geom = 'segment'
)
rm(tmp)

# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
save.image(file.path(pathOut, 'base.RData'))
## END