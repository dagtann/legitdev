# Marginal information on candidate indicators =============
summary(
  base[, c(
    'SI.POV.2DAY', 'SI.POV.DDAY', 'une_pov',
    'wdi_povhc190', 'wdi_povhc310'
    )
  ]
)
apply(                                # how many nonmissing?
  base[,
    c('SI.POV.2DAY', 'SI.POV.DDAY', 'une_pov',
      'wdi_povhc190', 'wdi_povhc310', 'AnteilabsolutArme'
    )
  ],
  2, 
  FUN = function(x){ sum(!is.na(x)) }
)
## DDAY closest conceptually, least missing

sapply(base[, c('AnteilabsolutArme', 'SI.POV.DDAY')], summary)
# marginal distributions differ tremendously

aggregate(
  cbind(
    SI.POV.DDAY = base[['SI.POV.DDAY']],
    AnteilAbsolutArme = base[['AnteilabsolutArme']]
  ),
  by = list(regime_type = base[['regime_type']]),
  FUN = mean, na.rm = TRUE
)
# Tremendous differences b/w WDI raw and paper data

cor(
  base[,
    c('SI.POV.2DAY', 'SI.POV.DDAY', 'une_pov',
      'wdi_povhc190', 'wdi_povhc310', 'AnteilabsolutArme'
    )
  ],
  use = 'pairwise.complete.obs'
)
with(base, sum(wdi_povhc190 == SI.POV.DDAY, na.rm = TRUE))
with(base, sum(wdi_povhc310 == SI.POV.2DAY, na.rm = TRUE))
# Mistake must be in AnteilabsolutArme b/c WDI information
# from different sources match perfectly
# Go with SI.POV.DDAY

# data availability ----------------------------------------
tmp <- aggregate(
  SI.POV.DDAY ~ spell_id, data = base,
  FUN = function(x){ sum( !is.na(x) ) }
)
summary(tmp)
tmp2 <- aggregate(
  rep(1, nrow(base)) ~ spell_id, data = base,
  FUN = sum
)
summary(tmp2)
# All authoritarian regimes at least 1 measurement
# At most 15 obs. -> probably no regime completely covered

# Bivariate distributions by regime type -------------------
ggplot(data = base, aes(
    x = reorder(regime_type, SI.POV.DDAY, median, na.rm = TRUE),
    y = SI.POV.DDAY
  )
) + geom_boxplot() + geom_jitter()
aggregate(
  SI.POV.DDAY ~ regime_type,
  data = base,
  FUN = function(x){ sum(!is.na(x)) }
)
ggplot(data = base, aes(x = SI.POV.DDAY)) +
  geom_density() + geom_rug() + facet_wrap(~regime_type)


ggplot(data = base,
  aes(
    x = reorder(regime_type, SI.POV.DDAY, mean, na.rm = TRUE),
    y = SI.POV.DDAY)
  ) +
  stat_summary(fun.y = mean, geom = 'point') +
  geom_hline(yintercept = mean(base$SI.POV.DDAY, na.rm = TRUE)) +
  coord_flip()

tmp <- base
tmp <- within(tmp, valid <- !is.na(SI.POV.DDAY))
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
  file = file.path(pathOut, 'ecdf_validObsAbsPoverty.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(tmp, p)

ggplot(data = base, aes(x = SI.POV.DDAY)) +
  geom_histogram(, binwidth = .1)

p <- ggplot(data = base, aes(x = SI.POV.DDAY)) +
  stat_density(geom = 'line') + geom_rug() + 
  labs(
    x = "Poverty headcount ratio at $1.90 a day",
    y = 'Dichte'
  )
ggsave(
  plot = p,
  file = file.path(pathOut, 'density_AbsPoverty.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(p)

p <- ggplot(data = base,
  aes(x = SI.POV.DDAY)
) +
  stat_density(geom = 'line') + geom_rug() + 
  labs(
    x = "Poverty headcount ratio at $1.90 a day",
    y = 'Dichte'
  ) +
  facet_wrap( ~ regime_type)
ggsave(
  plot = p,
  file = file.path(pathOut, 'density_AbsPovertyXregimetype.png'),
    width = plot_size, height = 1/1.618 * plot_size
)
rm(p)

p <- ggplot(
  data = base, 
  aes(x = year, y = SI.POV.DDAY, group = spell_id)) +
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
    x = 'Jahr', y = 'Poverty headcount ratio at $1.90 a day',
    colour = 'Smoother'
  )
ggsave(plot = p, file = file.path(pathOut, 'scatter_absPovTime.png'),
  width = plot_size, height = plot_size / 1.618
)

