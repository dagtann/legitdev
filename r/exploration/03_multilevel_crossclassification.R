# Find an appropriate baseline for longitudinal multilevel
# modeling of % absolute poverty in country.
# ----------------------------------------------------------

# Prepare workspace ----------------------------------------
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('rethinking')

# Define variables and apply transformations ---------------
d <- within(base, {
  # numeric spell identifier for 2nd lvl
  spell_id <- as.numeric(factor(base$spell_id))
  # transform dv such that linear predictor maps to [0, 1]
  # start .001: qnorm(0) == -Inf
  # probit_poverty <- qnorm(base$SI.POV.DDAY/100+.001)
  log_poverty <- log(SI.POV.DDAY/100+.001)
  }
)

# track time
t <- with(base, poly(year - 1990, 3))          # observation
colnames(t) <- c('t_lin', 't_squ', 't_cub')
duration <- with(base, poly(seq_along(spell_id), 3)) # process
colnames(duration) <- c('dur_lin', 'dur_squ', 'dur_cub')

d <- data.frame(d, t, duration)
d <- select(d,
  log_poverty, t_lin, t_squ, t_cub, dur_lin, dur_squ,
  dur_cub, year, spell_id, cowcode # cow = country level
)

d <- d[complete.cases(d), ]
class(d) <- 'data.frame'
summary(d); dim(d)

# baseline modeling ----------------------------------------
m1 <- map2stan(                  # full model with t and dur
  alist(
    log_poverty ~ dnorm(mu, sigma),
    mu <- a + a_regime[spell_id] + 
      beta1 * t_lin + beta2 * t_squ + beta3 * t_cub +
      beta4 * dur_lin + beta5 * dur_squ + beta6 * dur_cub,
    a ~ dnorm(0, 10),
    beta1 ~ dnorm(0, 10),
    beta2 ~ dnorm(0, 10),
    beta3 ~ dnorm(0, 10),
    beta4 ~ dnorm(0, 10),
    beta5 ~ dnorm(0, 10),
    beta6 ~ dnorm(0, 10),
    a_regime[spell_id] ~ dnorm(0, sigma_regime),
    sigma ~ dcauchy(0, 1),
    sigma_regime ~ dcauchy(0, 1)
  ),
  data = d, warmup = 2000, iter = 8e3, chains = 1, cores = 1
)
precis(m1, depth = 2)
# NOTE: all duration polynomial HPDs include 0
plot(m1)

# Full model diagnostics
post <- extract.samples( m1 )
pairs(as.data.frame(post[paste0('beta', 1:6)]))
# shows comp. strong correlation b/w beta1:beta3

cor(d[, c("t_lin", "t_squ", "t_cub", "dur_lin", "dur_squ", "dur_cub")])
cor(data.frame(t, duration))
# polynomials correlate b/c listwise deletion

# PLOTTING FAILS, DEBUG AND PUSH AGAIN
# # complete pooling observation process
# tvars <- c('t_lin', 't_squ', 't_cub')
# y <- data.frame(poly(-10:20, 3)); names(y) <- tvars
# with(subset(d, year >= 1980), plot(t_lin, log_poverty))
# for(i in 1:length(tvars)) {
#   if(i == 1){
#     fm <- lm(
#       formula(paste0('log_poverty ~', tvars[i])),
#       data = d
#     )
#   } else {
#     fm <- lm(
#       formula(paste0('log_poverty ~', paste(tvars[1:i], collapse = '+'))),
#       data = d
#     )
#   }
#   lines(d[, 't_lin'], predict(fm), col = i)
# }
# #rm(y, i, fm)
# # visible curvature, but
# # t^3 nearly identical to t^2

# # complete pooling regime duration
# base <- within(base, dur <- ave(year, spell_id, FUN = seq_along))
# summary(base)
# y <- 1:65
# with(subset(base, year >= 1980), plot(dur, log_poverty))
# for(i in 1:3) {
#   fm <- lm(log_poverty ~ poly(dur, degree = i), data = base)
#   lines(y, predict(fm, newdata = data.frame(dur = y)), col = i)
# }
# ## associations look weak and beyond dur >= 30 highly uncertain

# alternative baseline models ------------------------------
m1_1 <- map2stan(           # remove process time indicators
  alist(
    log_poverty ~ dnorm(mu, sigma),
    mu <- a + a_regime[spell_id] + 
      beta1 * t_lin + beta2 * t_squ + beta3 * t_cub,
    a ~ dnorm(0, 10),
    beta1 ~ dnorm(0, 10),
    beta2 ~ dnorm(0, 10),
    beta3 ~ dnorm(0, 10),
    a_regime[spell_id] ~ dnorm(0, sigma_regime),
    sigma ~ dcauchy(0, 1),
    sigma_regime ~ dcauchy(0, 1)
  ),
  data = d, warmup = 2000, iter = 8e3, chains = 1, cores = 1
)
compare(m1, m1_1)
# basically identical fit with 3 less parameters
# process and observation time effects cannot be separated
# DECISION: drop duration indicators

m1_2 <- map2stan(      # remove observation cubic polynomial
  alist(
    log_poverty ~ dnorm(mu, sigma),
    mu <- a + a_regime[spell_id] + 
      beta1 * t_lin + beta2 * t_squ,
    a ~ dnorm(0, 10),
    beta1 ~ dnorm(0, 10),
    beta2 ~ dnorm(0, 10),
    a_regime[spell_id] ~ dnorm(0, sigma_regime),
    sigma ~ dcauchy(0, 1),
    sigma_regime ~ dcauchy(0, 1)
  ),
  data = d, warmup = 2000, iter = 8e3, chains = 1, cores = 1
)
m1_3 <- map2stan(    # remove observation squared polynomial
  alist(
    log_poverty ~ dnorm(mu, sigma),
    mu <- a + a_regime[spell_id] + 
      beta1 * t_lin,
    a ~ dnorm(0, 10),
    beta1 ~ dnorm(0, 10),
    a_regime[spell_id] ~ dnorm(0, sigma_regime),
    sigma ~ dcauchy(0, 1),
    sigma_regime ~ dcauchy(0, 1)
  ),
  data = d, warmup = 2000, iter = 8e3, chains = 1, cores = 1
)

compare(m1_1, m1_2, m1_3)
sapply(list(m1_1, m1_2, m1_3), DIC)
# both information criteria show m1_1 best

# Check multiple chain convergence -------------------------
m1_4 <- map2stan(
  m1_1, iter = 8000, warmup = 2000, chains = 3, cores = 3
)
plot(m1_4) # all chains converge on the same distribution
precis(m1_4, depth = 2)
post <- extract.samples(m1_4)
pairs(as.data.frame(post[paste0('beta', 1:3)]))
# beta2 and beta3 still highly correlated
rm(post)

# extract samples ------------------------------------------
m1_samples <- map2stan(
  m1_1, iter = 20000, warmup = 2000, chains = 1, cores = 1
)
precis(m1_samples)
plot(m1_samples)
post <- extract.samples(m1_samples)
pdta <- do.call(data.frame, post[-5])
pdta <- within(pdta, id <- 1:nrow(pdta))
pdta <- gather(pdta, "coef", "sample", 1:6)
pdta <- within(pdta, coef <- factor(
  as.character(coef),
  levels = c('a', 'beta1', 'beta2', 'beta3', 'sigma', 'sigma_regime'),
  labels = c('Intercept', 'beta[1]', 'beta[2]', 'beta[3]', 'sigma[epsilon]', 'sigma[i]')
  )
)
p <- ggplot(data = pdta, aes(x = sample)) + 
  stat_density(geom = 'line') +
  labs(
    x = '', y = 'Dichte', 
    title = expression(
      log('Anteil absolut Arme'[it]+0.001) == 
        alpha + alpha[i] +
        beta[1]*t[it] + beta[2]*t[it]^2 + beta[3]*t[it]^3 +
        epsilon[it]
    )
  ) +
  facet_wrap(~coef, scales = 'free', labeller = label_parsed) +
  theme_bw()
ggsave(
  p, file = file.path(pathOut, 'density_HML_baseline.png'),
  width = plot.size, height = plot.size/1.618
)
rm(post, pdta)
