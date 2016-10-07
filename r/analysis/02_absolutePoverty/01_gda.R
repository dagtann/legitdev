# Exploratory data analysis on infant mortality rate =======
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
dep_var <- 'SI.POV.DDAY'

# Univariate distribution ==================================
summary(analysis[[dep_var]])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   3.275  17.980  27.130  48.170  88.320    3551 

boxplot(analysis[[dep_var]], horizontal = TRUE)
plot(density(analysis[[dep_var]], na.rm = TRUE))
# strong positive skew
# plateau b/w 40 - 60
# peak at 60

car::symbox(analysis[[dep_var]]+.01)
summary(car::powerTransform(analysis[[dep_var]]+.01))
# indicates bcPower transformation using 1/3
# PROBLEM: Still requires logistic transformation,
# Variable is non-normal by definition
analysis[, paste0('bc_', dep_var)] <- car::bcPower(
  U = analysis[[dep_var]]+.01,
  lambda = car::powerTransform(analysis[[dep_var]]+.01)[['roundlam']]
)
dep_var <- paste0('bc_', dep_var) # SWITCH to transformed var
summary(analysis[[dep_var]])
boxplot(analysis[[dep_var]], horizontal = TRUE)
plot(density(analysis[[dep_var]], na.rm = TRUE))
# strong negative skew now, still bimodal. BoxCox useless

dep_var <- 'SI.POV.DDAY'
# data availability over time
aggregate(
  analysis[[dep_var]],
  list(regime_type = analysis[['regime_type']]),
  FUN = function(x){ sum(!is.na(x)) }
)
tmp <- aggregate(
  analysis[, c('spell_id', dep_var)],
  list(t = analysis[['year']]),
  FUN = function(x){ sum(!is.na(x)) }
)
tmp[, 'ndep2nspell_ratio'] <- tmp[[dep_var]]/tmp[['spell_id']]
ggplot(
  data = tmp, aes(x = t, y = ndep2nspell_ratio)
) + geom_point()
# No data before 1981
# Data covers at most 30 per cent of autocracies
# Systematic increase in data availability over time.

# trend in dv over time?
ggplot(data = analysis,
  aes_string(x = 'year', y = dep_var)
) + geom_point(alpha = .1) + geom_line() + facet_wrap(~ cowcode)

ggplot(
  data = analysis, aes_string(x = 'year', y = dep_var)
) +
  geom_point(alpha = .2) +
  geom_smooth(
    method = 'lm', formula = y ~ x + I(x^2) + + I(x^3),
    aes_string(group = 'spell_id'),
    se = FALSE, size = .7, alpha = .2
  )
# sample downward trend, but some upward movement visible
# regimes with higher intercepts seem to have stronger slopes

# distribution against regime type
boxplot(
  analysis[, dep_var] ~ analysis[, 'regime_type'],
  horizontal = TRUE, las = 1
)
par(mfrow = c(2, length(levels(analysis$regime_type))/2))
for(i in levels(analysis$regime_type)){
  plot(
    ecdf(analysis[[dep_var]][analysis[['regime_type']] == i]),
    main = i
  )
}
# All regime types cover the entire range
# N for Oneparty, personalist and Monarchy very low
for(i in levels(analysis$regime_type)){
  plot(
    density(
      analysis[[dep_var]][analysis[['regime_type']] == i],
      na.rm = TRUE
    ),
    main = i
  )
}
# no distribution is simple. all are visibly skewed
# and bimodal.
summary(                  # pick a different transformation?
  car::powerTransform(
    analysis[[dep_var]]+.01 ~ analysis[['regime_type']]
  )
)
# lambda is stable at 1/3
dev.off()
# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])