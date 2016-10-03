# Define a baseline model using STAN and RETHINKING map2stan
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
library('rethinking')

d <- subset(
  analysis,
  select = c(
    'infant_mortality', 'spell_id', 'year',
    't_lin', 't_squ', 't_cub'
  )
)
d <- na.omit(d)
d$infant_mortality <- log(d$infant_mortality)

m_baseline <- map2stan(
  alist(
    infant_mortality ~ dnorm(mu, sigma_e),
    mu <- a + a_regime[spell_id] + 
      b_t1 * t_lin + b_t2 * t_squ + b_t3 * t_cub,
    a ~ dnorm(0, 25),
    a_regime[spell_id] ~ dnorm(0, sigma_regime),
    c(b_t1, b_t2, b_t3) ~ dnorm(0, 25),
    sigma_e ~ dcauchy(0, 1),
    sigma_regime ~ dcauchy(0, 1)
  ),
  data = d, 
  warmup = 2000, iter = 6000,
  chains = 4, cores = 3
)
precis(m_baseline, depth = 2)
plot(m_baseline)
cor(d[, c('dur_lin', 'dur_squ', 'dur_cub')])
pairs(d[, c('dur_lin', 'dur_squ', 'dur_cub')])
unique(d$spell_id)[268]
View(d[d$spell_id == "710:2", ])
post <- extract.samples(m_baseline)
str(post)
dens(exp(post$a))
tmp <- vapply(
  c('b_t1', 'b_t2', 'b_t3'),
  function(x){post[[x]]},
  numeric(16000)
)
str(tmp)
pairs(tmp)
cor(tmp)
round(
  cor(
    d[, c('t_lin', 't_squ', 't_cub', 'dur_lin', 'dur_squ', 'dur_cub')]
  ),
  digits = 2
)

ggplot(d, aes(x = t_lin, y = dur_lin, group = spell_id)) +
  geom_line(alpha = .4)

ggplot(data = subset(analysis, year - start_year > 3),
  aes(y = infant_mortality, group = spell_id) 
) + 
  geom_smooth(
    aes(x = (year-1990)),
    method = 'lm', formula = y ~ x + I(x^2) + I(x^3), colour = 'red',
    se = FALSE
  ) + 
  geom_smooth(
    aes(x = (year-start_year)),
    method = 'lm', formula = y ~ x + I(x^2) + I(x^3), se = FALSE
  )
