# Proyecto residuos o shocks
df <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/gdd_yc.rds")
df <- df %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    tmin_yce_lag1 = dplyr::lag(tmin_yce, 1),
    tmin_yce_lag2 = dplyr::lag(tmin_yce, 2),
    tmin_yce_future = dplyr::lead(tmin_yce, 2),
    
    tmax_yce_lag1 = dplyr::lag(tmax_yce, 1),
    tmax_yce_lag2 = dplyr::lag(tmax_yce, 2),
    tmax_yce_future = dplyr::lead(tmax_yce, 2),
    
    tas_yce_lag1 = dplyr::lag(tas_yce, 1),
    tas_yce_lag2 = dplyr::lag(tas_yce, 2),
    tas_yce_future = dplyr::lead(tas_yce, 2)
  ) %>%
  ungroup()

model_tmin <- feols(tmin_yce_future ~ tmin_yce_lag1 + tmin_yce_lag2 | country,
               data = df)
summary(model_tmin)

model_tmax <- feols(tmax_yce_future ~ tmax_yce_lag1 + tmax_yce_lag2 | country,
                    data = df)
summary(model_tmax)

model_tas <- feols(tas_yce_future ~ tas_yce_lag1 + tas_yce_lag2 | country,
                    data = df)
summary(model_tas)

df_2 <- df %>%
  mutate(
    tmin_yce_cf = predict(model_tmin, newdata = df),
    tmax_yce_cf = predict(model_tmin, newdata = df),
    tas_yce_cf = predict(model_tas, newdata = df),
    
    tmin_shock_yce = tmin_yce - tmin_yce_cf,
    tmax_shock_yce = tmax_yce - tmax_yce_cf,
    tas_shock_yce = tas_yce - tas_yce_cf,
    
    year = as.factor(year)
  ) %>%
  drop_na()
