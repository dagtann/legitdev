# Simple distribution summary ==============================
summary(base[, "SP.DYN.IMRT.IN"]) #                .2 rates?
sum(!is.na(base[, "SP.DYN.IMRT.IN"]))    # 3641 observations

aggregate(
  cbind(
    SP.DYN.IMRT.IN = base[['SP.DYN.IMRT.IN']]
  ),
  by = list(regime_type = base[['regime_type']]),
  FUN = mean, na.rm = TRUE
)
# Tremendous differences between regimes

# data availability ----------------------------------------
tmp <- aggregate(
  SP.DYN.IMRT.IN ~ spell_id, data = base,
  FUN = function(x){ sum( !is.na(x) ) }
)
summary(tmp)
tmp2 <- aggregate(
  rep(1, nrow(base)) ~ spell_id, data = base,
  FUN = sum
)
summary(tmp2)
# All authoritarian regimes at least 1 measurement
# At most 51 obs.

# Bivariate distributions by regime type -------------------
ggplot(data = base, aes(
    x = reorder(regime_type, SP.DYN.IMRT.IN, median, na.rm = TRUE),
    y = SP.DYN.IMRT.IN
  )
) + geom_boxplot() + geom_jitter()
aggregate(
  SP.DYN.IMRT.IN ~ regime_type,
  data = base,
  FUN = function(x){ sum(!is.na(x)) }
)
ggplot(data = base, aes(x = SP.DYN.IMRT.IN)) +
  geom_density() + geom_rug() + facet_wrap(~regime_type)


ggplot(data = base,
  aes(
    x = reorder(regime_type, SP.DYN.IMRT.IN, mean, na.rm = TRUE),
    y = SP.DYN.IMRT.IN)
  ) +
  stat_summary(fun.y = mean, geom = 'point') +
  geom_hline(yintercept = mean(base$SP.DYN.IMRT.IN, na.rm = TRUE)) +
  coord_flip()

tmp <- base
tmp <- within(tmp, valid <- !is.na(SP.DYN.IMRT.IN))
with(tmp, table(valid, regime_type))


tmp <- aggregate(valid  ~ spell_id, data = tmp, FUN = sum)
p <- ggplot(data = tmp, aes(x = valid)) + 
  stat_ecdf() +
  labs(
    x = 'Gesamtzahl gültiger Beobachtungen eines Regimes',
    y = 'Kumulative Häufigkeitsverteilung'
  )
ggsave(
  plot = p,
  file = file.path(pathOut, 'ecdf_validObsInfantMortality.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(tmp, p)

ggplot(data = base, aes(x = SP.DYN.IMRT.IN)) +
  geom_histogram(, binwidth = .5) +
  scale_x_continuous(breaks = seq(0, 250, 10))

p <- ggplot(data = base, aes(x = SP.DYN.IMRT.IN)) +
  stat_density(geom = 'line') + geom_rug() + 
  labs(
    x = "Infant mortality rate per 1,000 life births",
    y = 'Dichte'
  )
ggsave(
  plot = p,
  file = file.path(pathOut, 'density_InfantMortality.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(p)

p <- ggplot(data = base,
  aes(x = SP.DYN.IMRT.IN)
) +
  stat_density(geom = 'line') + geom_rug() + 
  labs(
    x = "Infant mortality rate per 1,000 life births",
    y = 'Dichte'
  ) +
  facet_wrap( ~ regime_type)
ggsave(
  plot = p,
  file = file.path(pathOut, 'density_InfantMortalityXregimetype.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(p)

p <- ggplot(
  data = base, 
  aes(x = year, y = SP.DYN.IMRT.IN, group = spell_id)) +
  geom_line() + geom_point() + geom_rug(sides = 'l') +
  geom_smooth(
    aes(group = NULL, colour = "x + x^2 + x^3"),
    formula = y ~ x + I(x^2) + I(x^3), method = 'lm',
    se = FALSE
  ) + 
  geom_smooth(
    aes(group = NULL, colour = "LOESS"),
    method = 'loess',
    se = FALSE
  ) +
  scale_x_continuous(limits = c(1980, 2010)) +
  labs(
    x = 'Jahr', y = 'Infant mortality rate per 1,000 life births',
    colour = 'Smoother'
  )
ggsave(plot = p, file = file.path(pathOut, 'scatter_infantMortality.png'),
  width = plot_size, height = plot_size / 1.618
)
rm(p)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
