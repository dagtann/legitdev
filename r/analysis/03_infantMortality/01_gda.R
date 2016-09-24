# Exploratory data analysis on infant mortality rate =======
rm(list = ls()[ls() %in% cleanWorkSpace = FALSE])

dep_var <- 'SP.DYN.IMRT.IN'

# Univariate distribution ==================================
summary(analysis[[dep_var]])
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  2.20   39.10   72.30   78.66  111.80  240.50     370 

boxplot(analysis[[dep_var]], horizontal = TRUE)
plot(density(analysis[[dep_var]], na.rm = TRUE))
# always >= 0
# strong positive skew
# multimodal distribution

car::symbox(analysis[[dep_var]])
car::powerTransform(analysis[[dep_var]])
# indicates bcPower transformation using 1/2
analysis[, paste0('bc_', dep_var)] <- car::bcPower(
  U = analysis[[dep_var]],
  lambda = car::powerTransform(analysis[[dep_var]])[['roundlam']]
)
dep_var <- paste0('bc_', dep_var) # SWITCH to transformed var
summary(analysis[[dep_var]])
boxplot(analysis[[dep_var]], horizontal = TRUE)
plot(density(analysis[[dep_var]], na.rm = TRUE))
# less skew, still bimodal and visibly asymmetric

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
tmp[, 'ndep2nspell_ratio'] <- with(tmp,
  bc_SP.DYN.IMRT.IN/spell_id
)
ggplot(
  data = tmp, aes(x = t, y = ndep2nspell_ratio)
) + geom_point()
# coverage increases systematically over time 

# trend in dv over time?
ggplot(data = analysis,
  aes_string(x = 'year', y = dep_var)
) + geom_line() + facet_wrap(~ cowcode)

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
# there are two noticeable plateaus for Oneparty
# (at about 10) and ideocracies (at about 22)
for(i in levels(analysis$regime_type)){
  plot(
    density(
      analysis[[dep_var]][analysis[['regime_type']] == i],
      na.rm = TRUE
    ),
    main = i
  )
}
# no distribution is simple. all are either visibly skewed
# or multimodal.
summary(                  # pick a different transformation?
  car::powerTransform(
    analysis[['SP.DYN.IMRT.IN']] ~ analysis[['regime_type']]
  )
)
# lambda is stable at .5
dev.off()
# Housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])