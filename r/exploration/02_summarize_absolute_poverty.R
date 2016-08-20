summary(base$SI.POV.2DAY)
summary(base$SI.POV.DDAY)
summary(base$AnteilabsolutArme)

t(sapply(base[, c('AnteilabsolutArme', 'SI.POV.DDAY')], summary))
aggregate(SI.POV.2DAY ~ regime_type, data = base, mean, na.rm = TRUE)
aggregate(SI.POV.DDAY ~ regime_type, data = base, mean, na.rm = TRUE)
aggregate(AnteilabsolutArme ~ regime_type, data = base, mean, na.rm = TRUE)