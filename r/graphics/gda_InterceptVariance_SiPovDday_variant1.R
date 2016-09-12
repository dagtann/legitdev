# Generate an exploratory figure 
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])

# generate data for plot -----------------------------------
mu <- mean(base[['SI.POV.DDAY']], na.rm = TRUE)
tmp <- aggregate(
  SI.POV.DDAY ~ spell_id + regime_type, data = base,
  FUN = mean, na.rm = TRUE
)
set.seed(16458)
regime_type_mean_ci <- lapply(
  levels(tmp$regime_type), function(x){
    data.frame(
      regime_type = x,
      mean_cl_boot(
        tmp[tmp[, 'regime_type'] == x, 'SI.POV.DDAY'],
        conf.int = .89, B = 2500
      )
    )
  }
)
regime_type_mean_ci <- do.call(rbind.data.frame, regime_type_mean_ci)

# generate plot --------------------------------------------
p <- ggplot() +
  geom_jitter(
    data = tmp, 
    aes(
      x = reorder(regime_type, -SI.POV.DDAY, mean),
      y = SI.POV.DDAY, shape = 'No pooling'
    ),
    position = position_jitter(width = .2, height = 0),
    alpha = .6, fill = 'grey65'
  ) +
  geom_hline(yintercept = mu, linetype = 'dashed') +
  geom_linerange(
    data = regime_type_mean_ci, 
    aes(
      x = reorder(regime_type, -y, mean), 
      ymin = ymin, ymax = ymax
    )
  ) +
  geom_point(
    data = regime_type_mean_ci,
    aes(
      x = reorder(regime_type, -y, mean), 
      y = y, shape = 'Regimetyp'
    )
  ) +
  geom_text(data = data.frame(), aes(y = mu, x = 1), label = 'Complete pooling',
    nudge_x = -.25, nudge_y = -10, family = 'CMU Sans Serif'
  ) +
  scale_shape_manual(values = c(21, 19)) +
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
    x = 'Regimetyp'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = 'top',
    panel.grid.major = element_line(colour = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90')
  ) +
  coord_flip()

# save to file ---------------------------------------------
ggsave(
  plot = p, 
  file = file.path(pathOut, 'dotchart_PovertyRegimeMeans.png'),
  width = plot_size, height = plot_size/1.618
)

# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])