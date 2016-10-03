# This script generates a dot chart that compares regime ===
# type means to the sample mean. Aggregate values and ======
# confidence intervals are derived from a clustered ========
# bootstrap which uses a stratified sampling scheme to =====
# ensure that all regime types are present in all ==========
# bootstrap samples. =======================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])

# global constants -----------------------------------------
dep_var <- 'SP.DYN.IMRT.IN'
regime_types <- c( 
  # manually sort factor levels b/c simplifies work with
  # numerous plotting objects simultanously
  'Oneparty', 'Military', 'Personalist', 'Electoral',
  'Monarchy', 'Ideocracy'
)
n_regime_type <- length(levels(base$regime_type))
alpha <- .1                            # significance level

# data objects ---------------------------------------------
poolingData <- na.omit(            # sample from poolingData
  base[
    base[['year']] %in% 1960:2010,
    c('spell_id', 'regime_type', dep_var)
  ]
)
poolingData <- within(poolingData, # coerce regime type order
  regime_type <- factor(
    as.character(regime_type), levels = regime_types
  )
)
spell_list <- lapply(regime_types, function(x){
  spell_id <- poolingData[['spell_id']][poolingData[, 'regime_type'] == x]
  spell_id <- unique(spell_id)
  n <- length(spell_id)
  out <- list(spell_id = spell_id, n = n)
  return(out)
  }
)
names(spell_list) <- regime_types

# execute cluster bootstrap on mu and mu|regime_type -------

# bootstrap constants
nIter <- 5000
set.seed(5034)

# bootstrap objects
mu <- vector(length = nIter)                  # sample means
mu_regime_type <- matrix(                # regime type means
  FALSE,
  nrow = nIter,
  ncol = n_regime_type
)
dimnames(mu_regime_type) <- list(
  NULL, regime_types
)

for(i in 1:nIter){               # execute cluster bootstrap
  if(i %% 500 == 0){ print(paste0('Bootstrap iteration ', i)) }
  assign(                           # sample from regime ids
    'sampled_spells',
    lapply(
      spell_list, function(k){
        out <- sample(k[['spell_id']], k[['n']], replace = TRUE)
        return(as.character(out))
      }
    )
  )
  sampled_spells <- unlist(sampled_spells, use.names = FALSE)
  # NOTE: Sampling ensures all regime types present
  #   Sampling implicitly weighted by frequency of regime type
  #   Within regime type all spells have equal weight
  assign('d',                                # generate data
    poolingData[poolingData[['spell_id']] %in% sampled_spells, ]
  )
  mu[i] <- mean(d[[dep_var]])
  mu_regime_type[i, ] <- sapply(
    regime_types,
    function(x){mean(d[[dep_var]][d[['regime_type']] == x])}
  )
  rm(sampled_spells, d) 
}

# check results
summary(mu); summary(mu_regime_type)
ggplot(data.frame(mu = mu), aes(x = mu)) +
  geom_density() +
  geom_vline(xintercept = mean(poolingData[[dep_var]]))
# normal & minimal bias on mu

plot(x = c(0, 150), y = c(0, .11), type = 'n')
for(i in 1:ncol(mu_regime_type)){
  lines(density(mu_regime_type[, i]), col = i)
}
legend(
  x = 'topright', lty = 1, col = 1:ncol(mu_regime_type),
  legend = colnames(mu_regime_type)
)
# positive skew on ideocracy & personalist
# slight negative skew on oneparty
# monarchy bimodal

delta_mu <- mu_regime_type - mu # regime type difference from mu
plot(x = c(-60, 60), y = c(0, .14), type = 'n')
for(i in 1:ncol(delta_mu)){
  lines(density(delta_mu[, i]), col = i)
}
legend(
  x = 'topright', lty = 1, col = 1:ncol(delta_mu),
  legend = colnames(delta_mu)
)
# Ideocracies, monarchies, and personalist autocracies
# positively skewed. Monarchies bimodal.
# bca confidence intervals advised.

# generate bca confidence intervals ------------------------
# sample mean ----------------------------------------------
# mu constants
uniqueSpellIds <- unique(poolingData[['spell_id']])

# (1) bias correction
mu_zhat0 <- qnorm(
  sum(mu < mean(poolingData[[dep_var]]))/nIter
)

# (2) skewness correction
thetahat_minusi <- vapply(
  as.character(uniqueSpellIds),
  function(x){
    mean(
      poolingData[[dep_var]][poolingData[, 'spell_id'] != x]
    )
  },
  numeric(1)
)
thetahat_marginal <- mean(thetahat_minusi)
enumerator <- sum((thetahat_marginal - thetahat_minusi)^3)
denominator <- 6 * sum((thetahat_marginal - thetahat_minusi)^2)^(3/2)
mu_ahat <- enumerator / denominator
mu_alphahat <- pnorm(
  mu_zhat0 + ((mu_zhat0 + qnorm(alpha))/(1-mu_ahat*(mu_zhat0 + qnorm(alpha))))
)
# regime type means ----------------------------------------

# (1) skewness correction
mu_regime_type_zaht0 <- vapply(
  regime_types,
  function(j){
    sum(
      mu_regime_type[, j] < mean(poolingData[[dep_var]][poolingData[, 'regime_type'] == j])
    ) / nIter
  }, numeric(1)
)

# (2) bias correction
thetahat_minusi <- lapply(
  regime_types,
  function(k){
    sapply(spell_list[[k]][[1]],
      function(i){
        mean(
          poolingData[[dep_var]][poolingData[, 'regime_type'] == k & poolingData[, 'spell_id'] != as.character(i)]
        )
      }
    )
  }
)
names(thetahat_minusi) <- regime_types
thetahat_marginal <- vapply(regime_types, 
  function(x){
    mean(thetahat_minusi[[x]])
  },
  numeric(1)
)
enumerator <- vapply(regime_types,
  function(x){
    sum((thetahat_marginal[x] - thetahat_minusi[[x]])^3)
  },
  numeric(1)
)
denominator <- vapply(
  regime_types,
  function(x){
    6 * sum((thetahat_marginal[x] - thetahat_minusi[[x]])^2)^(3/2)
  },
  numeric(1)
)
mu_ahat <- enumerator / denominator
mu_regime_type_alphahat <- pnorm(
  mu_zhat0 + ((mu_zhat0 + qnorm(alpha))/(1-mu_ahat*(mu_zhat0 + qnorm(alpha))))
)

# create plotting objects ----------------------------------
mu <- data.frame(
  SP.DYN.IMRT.IN = mean(mu), 
  ymin = quantile(mu, mu_alphahat), 
  ymax = quantile(mu, 1-mu_alphahat),
  regime_type = factor('Ideocracy', levels = regime_types)
)
# regime_type_mean_ci <- apply(
#   mu_regime_type, 2,
#   function(x){
#     out <- cbind(
#       y = mean(x), ymin = quantile(x, alpha/2), ymax = quantile(x, 1-alpha/2)
#     )
#     return(out)
#   }
# )
regime_type_mean_ci <- vapply(
  regime_types,
  function(x){
    out <- cbind(
      mean(mu_regime_type[, x]), 
      quantile(mu_regime_type[, x], mu_regime_type_alphahat[x]), 
      quantile(mu_regime_type[, x], 1-mu_regime_type_alphahat[x])
    )
    return(out)
  },
  numeric(3)
)
rownames(regime_type_mean_ci) <- c(dep_var, 'ymin', 'ymax')
regime_type_mean_ci <- data.frame(t(regime_type_mean_ci))
regime_type_mean_ci[, 'regime_type'] <- factor(
  rownames(regime_type_mean_ci),
  levels = regime_types
)

# generate means plot --------------------------------------
p <- ggplot(
  data = poolingData, 
  aes(x = regime_type, y = SP.DYN.IMRT.IN)
) +
  geom_ribbon(                  # sample mean ci across plot
    data = data.frame(     # coerce ribbon to fill plot area
      SP.DYN.IMRT.IN = 1,
      ymin = mu$ymin, ymax = mu$ymax,
      x = seq(0.6, 6.4, length.out = 100)
    ),
    aes(ymin = ymin, ymax = ymax, x = x), 
    fill = 'gray45', alpha = .3
  ) +
  geom_point(                                  # sample mean
    data = mu, aes(shape = 'Gesamt', x = 0.6)
  ) +
  geom_linerange(                           # sample mean CI
    data = mu, 
    aes(ymin = ymin, ymax = ymax, x = 0.6)
  ) +
  geom_jitter(                   # country year observations
    position = position_jitter(width = .2, height = 0),
    size = .8, alpha = .4
  ) +
  geom_linerange(                    # mean|regime_type ci's
    data = regime_type_mean_ci, 
    aes(ymin = ymin, ymax = ymax), colour = 'white'
  ) +
  geom_point(                             # mean|regime_type
    data = regime_type_mean_ci,
    aes(shape = 'Regimetyp'), 
    colour = 'white', fill = 'black'
  ) +
  scale_shape_manual(values = c(15, 21)) +
  guides(
    shape = 'none'
    # guide_legend(
    #   override.aes = list(
    #     colour = 'black', fill = 'black'
    #   )
    # )
  ) +
  scale_x_discrete(
    labels = c(
      'Ideocracy' = 'Kommunistische\nIdeokratie',
      'Electoral' = 'Elektorale\nAutokratie',
      'Military' = 'Militärautokratie',
      'Monarchy' = 'Monarchie',
      'Oneparty' = 'Einparteiautokratie',
      'Personalist' = 'Personalistische\nAutokratie'
    )
  ) +
  labs(
    y = 'Säuglingssterblichkeit pro 1.000 Lebendgeburten',
    x = 'Regimetyp', shape = 'Mittelwert'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.y = element_blank(), axis.title.x = element_blank(),
    legend.position = 'top',
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'),
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  ) +
  coord_flip()

# save to file ---------------------------------------------
ggsave(
  plot = p, 
  file = file.path(pathOut, 'dotchart_InfantMortality.png'),
  width = plot_size, height = plot_size/1.618, dpi = 300
)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])