dep_var <- 'SP.DYN.IMRT.IN'

summary(analysis[[dep_var]])
boxplot(analysis[[dep_var]], horizontal = TRUE)
# all positive
# strong positive skew

ggplot(data = analysis,
  aes_string(x = 'year', y = dep_var)
) + geom_line() + facet_wrap(~ cowcode)

spell_sample <- sample(unique(analysis$spell_id), 20, replace = FALSE)
ggplot(data = subset(analysis, spell_id %in% spell_sample),
  aes_string(x = 'year', y = dep_var)
) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 1)) +
  facet_wrap(~ spell_id)
