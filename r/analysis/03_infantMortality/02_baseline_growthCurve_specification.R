# Generate & compare baseline candidate models =============
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])
hlm_packages <- c('lme4', 'arm')
for(i in hlm_packages){library(i, character.only = TRUE)}

# Data objects ---------------------------------------------
d <- subset(                                    # model data
  analysis,
  select = c(
    'infant_mortality', 'spell_id',
    't_lin', 't_squ', 't_cub', 't_qud',
    "dur_lin", "dur_squ", 'dur_cub',
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp",
    "lag_ross_oil_value_2000", "lag_ross_gas_value_2000",
    "lp_catho80", "lp_muslim80", "lp_protmg80",
    "d_monarchy", "d_ideocracy", "d_oneparty", "d_personalist", "d_military", 
    "d_eap", "d_eca", "d_lac", "d_mena", "d_sa",
    'fe_etfra'
  )
)
d <- na.omit(d)
names(analysis)

# Baseline objects -----------------------------------------
completely_pooled <- lm(infant_mortality ~ 1, data = d)
ran_intercept <- lmer(
  infant_mortality ~ 1 + (1 | spell_id),
  data = d, REML = FALSE
)
lmtest::lrtest(completely_pooled, ran_intercept)
rm(completely_pooled)
# multilevel needed

# Growth curve candidates ----------------------------------
hlm_t1 <- update(ran_intercept, . ~ . + t_lin)
hlm_t2 <- update(hlm_t1, . ~ . + t_squ)
hlm_t3 <- update(hlm_t2, . ~ . + t_cub)
hlm_t4 <- update(hlm_t3, . ~ . + t_qud)

hlm_t1r <- update(hlm_t1, . ~ . - (1 | spell_id) + (1 + t_lin | spell_id))
hlm_t2r <- update(hlm_t2, . ~ . - (1 | spell_id) + (1 + t_lin + t_squ | spell_id))
hlm_t3r <- update(hlm_t3, . ~ . - (1 | spell_id) + (1 + t_lin + t_squ + t_cub | spell_id))
hlm_t4r <- update(hlm_t4, . ~ . - (1 | spell_id) + (1 + t_lin + t_squ + t_cub + t_qud | spell_id))

# Model comparisons ----------------------------------------
anova(hlm_t1, hlm_t2, hlm_t3, hlm_t4, hlm_t1r, hlm_t2r, hlm_t3r, hlm_t4r)
lapply(
  list(hlm_t1, hlm_t2, hlm_t3, hlm_t4, hlm_t1r, hlm_t2r, hlm_t3r, hlm_t4r),
  function(x){cbind(AIC = extractAIC(x)[[2]], BIC = BIC(x), DIC = extractDIC(x))}
)
# Each test favors more complex polynomials with random effects

# Model implications ---------------------------------------
sampled_spells <- sample(unique(d$spell_id), size = 30, replace = FALSE)
yhat <- data.frame(
  yhat1 = predict(hlm_t1r), yhat2 = predict(hlm_t2r),
  yhat3 = predict(hlm_t3r), yhat4 = predict(hlm_t4r)
)
ggplot(data = subset(data.frame(d, yhat), spell_id %in% sampled_spells)) +
  geom_point(aes(y = infant_mortality, x = t_lin), alpha = .4, size = .5) +
  geom_line(aes(y = yhat1, x = t_lin, colour = '1')) +
  geom_line(aes(y = yhat2, x = t_lin, colour = '2')) +
  geom_line(aes(y = yhat3, x = t_lin, colour = '3')) +
  geom_line(aes(y = yhat4, x = t_lin, colour = '4')) +
  facet_wrap(~ spell_id)
dev.off()
rm(sampled_spells, yhat)
# Predicted growth curves differ only for really long spells.
# Second degree polynomial is sufficient.

# Simulate model implied distributions of DV ---------------
dev.new()
par(mfrow = c(1, 4))
n_sims <- 1500
n_spells <- length(unique(d$spell_id))

y_hat <- lapply(
  list(hlm_t1r, hlm_t2r, hlm_t3r, hlm_t4r), function(m){
    #m <- hlm_t1r
    n_coef <- length(fixef(m))
    pred_dta <- cbind(
      model.matrix(m),
      spell_id = as.numeric(d$spell_id)
    )
    beta_hat <- array(
      data = numeric(1),
      c(n_spells, n_sims, n_coef),
      dimnames = list(
        as.character(unique(pred_dta[, 'spell_id'])),
        paste0('sim', 1:n_sims),
        paste0('beta', 1:n_coef)
      )
    )
    sim_theta <- sim(m, n_sims)
    for(i in 1:n_spells){
      beta_hat[i, , ] <- sim_theta@fixef + sim_theta@ranef[['spell_id']][, i, ]
    }
    out <- lapply(
      unique(pred_dta[, 'spell_id']),
      function(x){
        d <- pred_dta[pred_dta[, 'spell_id'] == x, 1:(ncol(pred_dta)-1)]
      if(class(d) == "matrix"){              # some only 1 obs
        y_hat <- tcrossprod(beta_hat[as.character(x), , ], d)
      } else {
        y_hat <- beta_hat[as.character(x), , ] %*% d
      }
      y_hat <- y_hat + rnorm(n_sims, 0, sim_theta@sigma)
      return(y_hat)
      }
    )
  return(out)
  }
)
y_hat <- lapply(y_hat, function(m){do.call(cbind, m)})

for(m in 1:length(y_hat)){
  plot(
    density(d[, 'infant_mortality']),
    type = 'n', xlim = c(-10, 40), ylim = c(0, .12),
    main = paste0("Polynomial of degree ", m)
  )
  for(i in 1:n_sims){
    lines(
      density(y_hat[[m]][i, ]), col = scales::alpha('black', 0.2)
      )
  }
  lines(density(d$infant_mortality), col = 'red')
}

# Every polynomial gets the peak at about 16, but all implied
# distributions differ systematically from the data. Reasons:
# (a) There is a kink in the data at about 5, which is followed
#   by a plateau. All polynomials have a hard time getting 
#   this kink.
# (b) Upper tails of polynomials > 2 are too fat and go
#   far beyond the observed data.
# Summary: A linear trend underfits the growth curve in many
#   instances. Higher polynomials tend to overfit more durable
#   regimes. All polynomials tend to capture the density
#   over the observed outcome. A second degree polynomial
#   is selected as a compromise.
# housekeeping =============================================
lapply(rev(hlm_packages), function(i){
  detach(paste0('package:', i), character.only = TRUE)
  }
)
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])