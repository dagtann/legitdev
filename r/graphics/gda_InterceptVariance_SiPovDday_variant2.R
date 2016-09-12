# Generate an exploratory figure 
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])

# generate aggregate data for plot -------------------------

# Bootstrap complete pooling mean & PI
poolingData <- na.omit(
  base[, c('spell_id', 'regime_type', 'SI.POV.DDAY')]
)
poolingData <- within(poolingData, {
  regime_type <- as.character(regime_type)
  regime_type <- factor(regime_type,
    levels = rev(c('Electoral', 'Monarchy', 'Ideocracy', 'Military', 'Oneparty', 'Personalist'))
  )
  }
)
uniqueSpellIds <- unique(poolingData[['spell_id']])
nUniqueSpellIds <- length(uniqueSpellIds)       # 86 regimes

nIter <- 5000
mu <- vector(length = nIter)        # hold bootstrap results
set.seed(39486)
for(i in 1:nIter){
  if(i %% 500 == 0){ print(i) }
  assign(
    'spellSet',
    sample(uniqueSpellIds, length(uniqueSpellIds), replace = TRUE)
  )
  assign('d',
    poolingData[poolingData[['spell_id']] %in% spellSet, ]
  )
  mu[i] <- mean(d[['SI.POV.DDAY']])
}
hist(mu)
plot(density(mu))
mu <- data.frame(
  y = mean(mu), ymin = quantile(mu, .05), ymax = quantile(mu, .95)
)

# Bootstrap means & PI for means by regime type
set.seed(16458)
regime_type_mean_ci <- lapply(
  levels(poolingData$regime_type), function(k){
    d <- poolingData[poolingData[['regime_type']] == k, ]
    uniqueSpells <- unique(d[['spell_id']])
    nUniqueSpells <- length(uniqueSpells)
    out <- vector(length = nIter)
    for(i in 1:nIter){
      assign(
        'bootSpells',
        sample(uniqueSpells, nUniqueSpells, replace = TRUE)
      )
      assign('boot_d', d[d[['spell_id']] %in% bootSpells, ])
      out[i] <- mean(boot_d[['SI.POV.DDAY']])
    }
    return(out)
  }
)
names(regime_type_mean_ci) <- levels(poolingData$regime_type)
str(regime_type_mean_ci)
lapply(regime_type_mean_ci, summary)
aggregate(SI.POV.DDAY ~ regime_type, data = poolingData, FUN = mean)

regime_type_mean_ci <- lapply(
  regime_type_mean_ci,
  function(x){
    out <- cbind(y = mean(x), ymin = quantile(x, .05), ymax = quantile(x, .95))
    return(out)
  }
)
regime_type_mean_ci <- do.call(rbind.data.frame, regime_type_mean_ci)
regime_type_mean_ci[, 'regime_type'] <- factor(
  rownames(regime_type_mean_ci),
  levels = levels(poolingData$regime_type)
)

# generate plot --------------------------------------------
p <- ggplot() +
  geom_jitter(
    data = poolingData, 
    aes(
      x = regime_type, y = SI.POV.DDAY
    ),
    position = position_jitter(width = .2, height = 0), size = .8,
    alpha = .4, fill = 'grey65', shape = 21
  ) +
  geom_linerange(
    data = mu, 
    aes(ymin = ymin, ymax = ymax, x = 0.46)
  ) +
  geom_point(
    data = mu, 
    aes(y = y, x = 0.46, shape = 'Gesamt')
  ) +
  geom_hline(data = mu, aes(yintercept = y), linetype = 'dashed') +
  geom_linerange(
    data = regime_type_mean_ci, 
    aes(
      x = regime_type, 
      ymin = ymin, ymax = ymax
    )
  ) +
  geom_point(
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
    y = expression('Bevölkerungsanteil unter der Armutsgrenze von 1,90'~over('$', 'Tag')),
    x = 'Regimetyp', shape = 'Mittelwert'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.y = element_blank(),
    legend.position = 'top',
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'),
    plot.margin = grid::unit(rep(0, 4)+.1, 'lines')
  ) +
  coord_flip()

# save to file ---------------------------------------------
ggsave(
  plot = p, 
  file = file.path(pathOut, 'dotchart_PovertyRegimeMeans.png'),
  width = plot_size, height = plot_size/1.618, dpi = 300
)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])