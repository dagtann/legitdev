# This script generates basic frequency distributions &
# plots  on regime types
# ==========================================================
length(unique(base$cowcode))          # 121 unique countries


length(unique(base$spell_id))             # 360 distinct ATR

tmp <- aggregate(spell_id ~ year, data = base, FUN = length)
range(tmp$spell_id)              # 34 to 94 regimes per year
mean(tmp$spell_id)          # on average 69 regimes per year
qplot(                              # ATR frequency per year
  data = tmp,
  x = year, xend = year, y = 0, yend = spell_id,
  geom = 'segment'
)
rm(tmp)

table(base$regime_type)

p <- qplot(data = base, x = year, fill = regime_type, binwidth = 1) +
  scale_x_continuous(breaks = seq(1945, 2010, 5)) +
  labs(x = '', y = 'Anzahl autoritärer Regime', fill = 'Regimetyp')
ggsave(file.path(pathOut, 'count_regimesXtype.png'),
  width = plot_size, height = plot_size/1.618
)
# No. ATR increases sharply by 1960 b/c electoral
# autocracies increase -> decolonization effect?
# Personalist mostly absent before 1960,
#   increasingly frequent 1960-1990
# Electoral most frequent but late 1970s

