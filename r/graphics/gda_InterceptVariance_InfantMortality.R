# Generate an exploratory figure 
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])

# generate aggregate data for plot -------------------------
poolingData <- na.omit(
  base[, c('spell_id', 'regime_type', 'SP.DYN.IMRT.IN')]
)
poolingData <- within(poolingData, {
  regime_type <- as.character(regime_type)
  regime_type <- factor(regime_type,    # coerce level order
    levels = rev(c('Electoral', 'Monarchy', 'Ideocracy', 'Military', 'Oneparty', 'Personalist'))
  )
  }
)

# cluster bootstrap on mu and mu|regime_type
uniqueSpellIds <- unique(poolingData[['spell_id']])
nUniqueSpellIds <- length(uniqueSpellIds)      # 304 regimes
nIter <- 10000
mu <- vector(length = nIter)             # hold sample means
mu_regime_type <- matrix(           # hold regime type means
  FALSE,
  nrow = nIter,
  ncol = length(levels(poolingData$regime_type))
)
dimnames(mu_regime_type) <- list(
  NULL, levels(poolingData$regime_type)
)
set.seed(4797)#48823)

for(i in 1:nIter){               # execute cluster bootstrap
  if(i %% 500 == 0){ print(paste0('Iteration ', i)) }
  assign(                           # sample from regime ids
    'spellSet',
    sample(uniqueSpellIds, length(uniqueSpellIds), replace = TRUE)
  )
  assign('d',                                # generate data
    poolingData[poolingData[['spell_id']] %in% spellSet, ]
  )
  mu[i] <- mean(d[['SP.DYN.IMRT.IN']])
  mu_regime_type[i, ] <- sapply(
    levels(d$regime_type),
    function(x){mean(d$SP.DYN.IMRT.IN[d$regime_type == x])}
  ) 
}

# check results
summary(mu); summary(mu_regime_type)
ggplot(data.frame(mu = mu), aes(x = mu)) +
  geom_density() +
  geom_vline(xintercept = mean(poolingData$SP.DYN.IMRT.IN))
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

# generate plotting objects --------------------------------
alpha <- .05                            # significance level
mu <- data.frame(
  y = mean(mu),
  ymin = quantile(mu, alpha/2), ymax = quantile(mu, 1-alpha/2)
)
regime_type_mean_ci <- apply(
  mu_regime_type, 2,
  function(x){
    out <- cbind(
      y = mean(x), ymin = quantile(x, alpha/2), ymax = quantile(x, 1-alpha/2)
    )
    return(out)
  }
)
rownames(regime_type_mean_ci) <- c('y', 'ymin', 'ymax')
regime_type_mean_ci <- data.frame(t(regime_type_mean_ci))
regime_type_mean_ci[, 'regime_type'] <- factor(
  rownames(regime_type_mean_ci),
  levels = levels(poolingData[['regime_type']])
)

# generate means plot --------------------------------------
p <- ggplot() +
  geom_point(                                  # sample mean
    data = mu, 
    aes(y = y, x = 0.46, shape = 'Gesamt')
  ) +
  geom_linerange(                           # sample mean CI
    data = mu, 
    aes(ymin = ymin, ymax = ymax, x = 0.46)
  ) +
  geom_ribbon(                  # sample mean ci across plot
    data = data.frame(     # coerce ribbon to fill plot area
      ymin = mu$ymin, ymax = mu$ymax,
      x = seq(0.46, 6.5, length.out = 100)
    ),
    aes(ymin = ymin, ymax = ymax, x = x), alpha = .3
  ) +
  geom_jitter(                   # country year observations
    data = poolingData, 
    aes(
      x = regime_type, y = SP.DYN.IMRT.IN
    ),
    position = position_jitter(width = .2, height = 0),
    size = .6, alpha = .2, fill = 'grey65', shape = 21
  ) +
  geom_linerange(                    # mean|regime_type ci's
    data = regime_type_mean_ci, 
    aes(
      x = regime_type, 
      ymin = ymin, ymax = ymax
    )
  ) +
  geom_point(                             # mean|regime_type
    data = regime_type_mean_ci,
    aes(
      x = regime_type,
      y = y, shape = 'Regimetyp'
    )
  ) +
  scale_shape_manual(values = c(15, 19)) +
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
    y = expression('Kindersterblichkeitsrate pro 1.000 Lebendgeburten'),
    x = 'Regimetyp', shape = 'Mittelwert'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.y = element_blank(),
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

# generate difference from mean plot -----------------------
delta_mean_ci <- apply(
  delta_mu, 2,
  function(x){
    out <- cbind(
      y = mean(x), ymin = quantile(x, alpha), ymax = quantile(x, 1-alpha/2)
    )
    return(out)
  }
)
rownames(delta_mean_ci) <- c('y', 'ymin', 'ymax')
delta_mean_ci <- data.frame(t(delta_mean_ci))
delta_mean_ci[, 'regime_type'] <- factor(
  rownames(delta_mean_ci),
  levels = levels(poolingData[['regime_type']])
)

p2 <- ggplot(data = delta_mean_ci,
  aes(x = regime_type, y = y, ymin = ymin, ymax = ymax)
) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
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
    y = expression(mu[k]^'*'-mu^'*'),
    title = 'Differenz zum Gesamtmittelwert'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.y = element_blank(),
    legend.position = 'top',
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'),
    plot.margin = grid::unit(c(.5, .5, 0, 0)+.1, 'lines')
  ) +
  coord_flip()
ggsave(
  plot = p2, 
  file = file.path(pathOut, 'dotchart_InfantMortality_deltaToMu.png'),
  width = plot_size, height = plot_size/1.618, dpi = 300
)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])