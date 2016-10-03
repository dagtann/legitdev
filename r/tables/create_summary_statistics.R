# Generate summary statistics ==============================
# This provides all information necessary to create a
# table of summary statistics in Word.
options(OutDec = ',')

# time varying controls ------------------------------------
mySummary <- function(x){ # 5-point summary
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  q25 <- quantile(x, .25, na.rm = TRUE, names = FALSE)
  q50 <- quantile(x, .5, na.rm = TRUE, names = FALSE)
  q75 <- quantile(x, .75, na.rm = TRUE, names = FALSE)
  n <- sum(!is.na(x))
  out <- c(min = min, q25 = q25, q50 = q50, q75 = q75, max = max, n = n)
  return(round(out, 2))
}
residual_table <- vapply(
  analysis[, c(
    "SP.DYN.IMRT.IN", "SI.POV.DDAY",
    "growth_mad_gdppch", "lag_mad_gdppch",
    "lag_wdi_agrvagdp", "lag_ross_oil_value_2000",
    "lag_ross_gas_value_2000", 't_lin', 't_squ'
  )
  ],
  FUN = mySummary,
  numeric(6)
)
t(residual_table)

# time constant variables ----------------------------------
tmp <- aggregate(                             # numeric vars
  analysis[, c(
    "fe_etfra", "lp_muslim80", "lp_catho80", "lp_protmg80") 
  ],
  by = list(spell_id = analysis[, 'spell_id']),
  FUN = mean, na.rm = TRUE
)
timeConstant_numeric <- apply(
  tmp[, -1], 2, mySummary
)
t(timeConstant_numeric)

tmp <- subset(analysis,                   # categorical vars
  select = c("spell_id", 'regime_type', 'region')
)
tmp <- aggregate(
  tmp[, c('regime_type', 'region') ],
  by = list(spell_id = tmp[, 'spell_id']),
  FUN = unique
)
table(tmp$regime_type)
table(tmp$region)
# housekeeping ---------------------------------------------
options(OutDec = '.')
rm(list = ls()[ls() %in% cleanWorkSpace == FALSE])