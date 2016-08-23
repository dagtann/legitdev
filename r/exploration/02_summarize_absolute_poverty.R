summary(base$SI.POV.2DAY)
summary(base$SI.POV.DDAY)
summary(base$AnteilabsolutArme)
summary(
  base[, c(
    'SI.POV.2DAY', 'SI.POV.DDAY', 'une_pov',
    'wdi_povhc190', 'wdi_povhc310'
    )
  ]
)

t(sapply(base[, c('AnteilabsolutArme', 'SI.POV.DDAY')], summary))
aggregate(SI.POV.2DAY ~ regime_type, data = base, mean, na.rm = TRUE)
aggregate(SI.POV.DDAY ~ regime_type, data = base, mean, na.rm = TRUE)
aggregate(AnteilabsolutArme ~ regime_type, data = base, mean, na.rm = TRUE)

aggregate(une_pov ~ regime_type, data = base, mean, na.rm = TRUE)
aggregate(wdi_povhc190 ~ regime_type, data = base, mean, na.rm = TRUE)
apply(
  base[,
    c('SI.POV.2DAY', 'SI.POV.DDAY', 'une_pov',
      'wdi_povhc190', 'wdi_povhc310', 'AnteilabsolutArme'
    )
  ],
  2, 
  FUN = function(x){ sum(!is.na(x)) }
)
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

# bootstrap
rm(unique_regimes)

ggplot(data = base, aes(
  x = reorder(regime_type, SI.POV.DDAY, median, na.rm = TRUE), y = SI.POV.DDAY)) +
  geom_boxplot() + geom_jitter()
ggplot(data = base, aes(x = SI.POV.DDAY)) +
  geom_density() + geom_rug() + facet_wrap(~regime_type)
