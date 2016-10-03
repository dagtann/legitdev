# From the varying effects baseline model generate a ribbon
# chart that summarizes ICC as a function of the varying ===
# coefficient on t_squ =====================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('rethinking')
load(file.path(pathOut, 'stan_ranefGrowthCurve2_2016-09-29.RData'))

# generate plot data ---------------------------------------
N <- 5000
post <- extract.samples(fit_ranefGrowthCurve2, n = N)

# generate predictor range and matrix to hold ICC for each
# sample from the model
t_squ <- range(analysis$t_squ)
t_squ <- seq(t_squ[1], t_squ[2], length.out = 100)
tau <- matrix(FALSE, nrow = N, ncol = length(t_squ))

# calculate ICC for each sample and value of t_squ
sigma_e <- post[['sigma_e']]
sigma_regime <- post[['sigma_regime']][, 1]
sigma_tsqu <- post[['sigma_regime']][, 2]
rho <- post[['Rho']][, 1, 2]

for(i in 1:length(t_squ)){
  tau[, i] <- sigma_regime^2 + 
    2 * rho * sigma_regime * sigma_tsqu * t_squ[i] + 
    sigma_tsqu^2 * t_squ[i]^2
  tau[, i] <- tau[, i] / (tau[, i]  + sigma_e^2)
}

# transform results to plotting object ---------------------
pdta <- data.frame(t_squ, t(tau))
pdta <- gather(pdta, 'sample', 'value', 2:ncol(pdta))
pdta_mu <- aggregate(value ~ t_squ, data = pdta, FUN = mean)
pdta_pi <- aggregate( # 90% credible intervals
  value ~ t_squ, data = pdta, FUN = quantile, prob = c(.05, .95)
)
pdta <- inner_join(pdta_mu, pdta_pi, by = 't_squ')
names(pdta)[2:3] <- c('mu', 'pi')

p <- ggplot(
  data = pdta,
  aes(x = t_squ, y = mu, ymin = pi[, 1], ymax = pi[, 2])
) +
  geom_ribbon(fill = 'grey85', alpha = .6) + geom_line() +
  scale_y_continuous(
    limits = c(.95, 1), breaks = seq(.95, 1, .01),
    labels = scales::percent
  ) +
  labs(x = expression('t'^2), y = expression('ICC'['Regime']),
    title = "Varianzpartitionierung des Referenzmodells") +
  theme_minimal(base_family = "CMU Sans Serif") +
  theme(
    plot.margin = grid::unit(c(0, .5, 0, 0)+.1, 'lines')
  )
ggsave(
  plot = p,
  file = file.path(pathOut,'ribbon_ICC_baseline_Ranef_t2.png'),
  width = plot_size, height = plot_size/1.618, dpi = 300
)
# housekeeping =============================================
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
detach(package:rethinking)
detach(package:rstan)