# This script generates basic frequency distributions &
# plots  on regime types
# ==========================================================
length(unique(base$cowcode)) # 121 unique countries
length(unique(base$cowcode[base$year >= 1960])) # 120 countries

length(unique(base$spell_id))             # 357 distinct ATR

tmp <- aggregate(spell_id ~ year, data = base, FUN = length)
range(tmp$spell_id)              # 34 to 94 regimes per year
mean(tmp$spell_id)          # on average 69 regimes per year
qplot(                              # ATR frequency per year
  data = tmp,
  x = year, xend = year, y = 0, yend = spell_id,
  geom = 'segment'
)


table(base$regime_type)
#  Electoral    Military    Monarchy    Oneparty   Ideocracy Personalist 
#       1575         600         675         479         699         430 
# Electoral modal category

p <- qplot(data = base, x = year, fill = regime_type, binwidth = 1) +
  scale_x_continuous(breaks = seq(1945, 2010, 5)) +
  labs(x = '', y = 'Anzahl autoritärer Regime', fill = 'Regimetyp')
ggsave(file.path(pathOut, 'count_regimesXtype.png'),
  width = plot.size, height = plot.size/1.618
)
# No. ATR increases sharply by 1960 b/c electoral
# autocracies increase -> decolonization effect?
# Personalist mostly absent before 1960,
#   increasingly frequent 1960-1990
# Electoral most frequent but late 1970s

