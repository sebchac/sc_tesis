# ==============================================================================
# estimation_diff.R
# Estimación diferenciada: Arábica y Robusta como bienes sustitutos
#
# Pipeline:
#   [0] Preliminares
#   [1] Datos en formato ancho (un año por fila, ambas especies)
#   [2] Sistema de demanda inversa conjunta (SUR / 2SLS-SUR)
#   [3] Costos marginales implícitos por especie
#   [4] Datos contrafactuales por especie
#   [5] Simulación de equilibrio por especie (Stackelberg + Cournot)
#   [6] Resumen y guardado de resultados
#
# Outputs (bld/data/):
#   demand_diff.rds, data_cf_arabica.rds, data_cf_robusta.rds,
#   results_y_arabica.rds, results_yc_arabica.rds,
#   results_y_robusta.rds, results_yc_robusta.rds,
#   summary_arabica.rds, summary_c_arabica.rds,
#   summary_robusta.rds, summary_c_robusta.rds
# ==============================================================================


# ------------------------------------------------------------------------------
# [0] Preliminares
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ivreg)
library(sandwich)
library(lmtest)
library(broom)
library(fixest)
library(plm)
library(nleqslv)
library(purrr)
library(ggplot2)
library(lubridate)
library(lfe)
library(slider)
library(systemfit)    # SUR / 2SLS-SUR

ruta_proyecto <- "/Users/sebastianchacon/Desktop/sc_tesis/"
bld_data      <- paste0(ruta_proyecto, "bld/data/")

data_raw <- readRDS(paste0(bld_data, "data_by_type.rds"))


# ==============================================================================
# [1] Datos en formato ancho
#     Un año por fila, con variables de Arábica y Robusta en columnas separadas
# ==============================================================================

# ── Subconjuntos anuales (una fila por año, nivel mundial) ────────────────────
annual_ara <- data_raw %>%
  filter(dummy_y_arabica == 1) %>%
  select(
    year, date,
    qe_y_ara    = qe_y,
    price_y_ara = price_y,
    importGDP_y, teaPrices_y, fert_y,
    tmax_ye_ara = tmax_ye_mean,
    tmin_ye_ara = tmin_ye_mean,
    heatExp_y_ara  = heatExp_y_mean,
    frostExp_y_ara = frostExp_y_mean,
    optExp_y_ara   = optExp_y_mean
  ) %>%
  mutate(year = as.double(year))

annual_rob <- data_raw %>%
  filter(dummy_y_robusta == 1) %>%
  select(
    year,
    qe_y_rob    = qe_y,
    price_y_rob = price_y,
    tmax_ye_rob = tmax_ye_mean,
    tmin_ye_rob = tmin_ye_mean,
    heatExp_y_rob  = heatExp_y_mean,
    frostExp_y_rob = frostExp_y_mean,
    optExp_y_rob   = optExp_y_mean
  ) %>%
  mutate(year = as.double(year))

# ── Unir en formato ancho ─────────────────────────────────────────────────────
data_wide <- inner_join(annual_ara, annual_rob, by = "year") %>%
  arrange(year) %>%
  mutate(
    trend    = floor(year - min(year, na.rm = TRUE)),
    trend5y  = floor((year - min(year, na.rm = TRUE)) / 5),
    trend3y  = floor((year - min(year, na.rm = TRUE)) / 3)
  ) %>%
  drop_na()

cat("\n[1] Datos ancho: ", nrow(data_wide), "años (",
    min(data_wide$year), "-", max(data_wide$year), ")\n")


# ==============================================================================
# [2] Sistema de demanda inversa conjunta (SUR / 2SLS-SUR)
#
# Especificación (demanda inversa, igual que estimation_y.R):
#   p_ara = α_ara + β_ara · q_ara + δ_ara · q_rob + γ · controls + ε₁
#   p_rob = α_rob + β_rob · q_rob + δ_rob · q_ara + γ · controls + ε₂
#
# Instrumentos: temperatura y calor de cada especie como desplazadores de oferta
# ==============================================================================

eq_ara <- price_y_ara ~ qe_y_ara + qe_y_rob + importGDP_y + teaPrices_y
eq_rob <- price_y_rob ~ qe_y_rob + qe_y_ara + importGDP_y + teaPrices_y

system_eqs <- list(arabica = eq_ara, robusta = eq_rob)

# ── [2.1] OLS-SUR ─────────────────────────────────────────────────────────────
fit_sur_ols <- systemfit(
  system_eqs,
  method = "SUR",
  data   = data_wide
)
summary(fit_sur_ols)

# ── [2.2] 2SLS-SUR (instrumentos climáticos) ──────────────────────────────────
# Instrumentos: variación climática de cada especie desplaza su oferta
# (excluye la temperatura propia del precio propio para satisfacer exclusión)
inst_formula <- ~ importGDP_y + teaPrices_y +
                  tmax_ye_ara + tmin_ye_ara +
                  tmax_ye_rob + tmin_ye_rob

fit_sur_iv <- systemfit(
  system_eqs,
  method = "2SLS",
  inst   = inst_formula,
  data   = data_wide
)
summary(fit_sur_iv)

# ── [2.3] Extraer coeficientes ────────────────────────────────────────────────
coeff_ara <- coef(fit_sur_iv)[grepl("^arabica_", names(coef(fit_sur_iv)))]
coeff_rob <- coef(fit_sur_iv)[grepl("^robusta_",  names(coef(fit_sur_iv)))]

# Limpiar prefijos para compatibilidad con price_from_demand()
names(coeff_ara) <- sub("^arabica_", "", names(coeff_ara))
names(coeff_rob) <- sub("^robusta_",  "", names(coeff_rob))

# Pendientes propias de la demanda inversa (dP/dQ_i)
beta_ara <- coeff_ara["qe_y_ara"]   # negativo: más cantidad → menor precio
beta_rob <- coeff_rob["qe_y_rob"]

# Efectos cruzados (dP_ara/dQ_rob y dP_rob/dQ_ara)
delta_ara <- coeff_ara["qe_y_rob"]  # efecto de cantidad Robusta sobre precio Arábica
delta_rob <- coeff_rob["qe_y_ara"]  # efecto de cantidad Arábica sobre precio Robusta

cat("\n[2] Demanda inversa conjunta (2SLS-SUR)\n")
cat("  β_ara  (dP_ara/dQ_ara) =", round(beta_ara, 4), "\n")
cat("  β_rob  (dP_rob/dQ_rob) =", round(beta_rob, 4), "\n")
cat("  δ_ara  (dP_ara/dQ_rob) =", round(delta_ara, 4), "\n")
cat("  δ_rob  (dP_rob/dQ_ara) =", round(delta_rob, 4), "\n")

# ── [2.4] Elasticidades propias y cruzadas (evaluadas en medias) ──────────────
mean_p_ara <- mean(data_wide$price_y_ara, na.rm = TRUE)
mean_q_ara <- mean(data_wide$qe_y_ara,   na.rm = TRUE)
mean_p_rob <- mean(data_wide$price_y_rob, na.rm = TRUE)
mean_q_rob <- mean(data_wide$qe_y_rob,   na.rm = TRUE)

# Elasticidad propia: e_ii = (1/β_ii) * (P_i / Q_i)
e_own_ara  <- (1 / beta_ara)  * (mean_p_ara / mean_q_ara)
e_own_rob  <- (1 / beta_rob)  * (mean_p_rob / mean_q_rob)
# Elasticidad cruzada: e_ij = (1/δ_ij) * (P_i / Q_j)  [demanda directa]
e_cross_ara <- (1 / delta_ara) * (mean_p_ara / mean_q_rob)  # dQ_ara/dP_rob aprox.
e_cross_rob <- (1 / delta_rob) * (mean_p_rob / mean_q_ara)

cat("  Elasticidad propia Arábica  :", round(e_own_ara, 3), "\n")
cat("  Elasticidad propia Robusta  :", round(e_own_rob, 3), "\n")
cat("  Elasticidad cruzada (ara→rob):", round(e_cross_ara, 3), "\n")
cat("  Elasticidad cruzada (rob→ara):", round(e_cross_rob, 3), "\n")

# ── [2.5] Guardar dataset de demanda ──────────────────────────────────────────
demand_diff <- data_wide %>%
  mutate(
    price_hat_ara = predict(fit_sur_iv)$arabica,
    price_hat_rob = predict(fit_sur_iv)$robusta,
    e_own_ara  = (1 / beta_ara)  * (price_y_ara / qe_y_ara),
    e_own_rob  = (1 / beta_rob)  * (price_y_rob / qe_y_rob)
  )

saveRDS(demand_diff, paste0(bld_data, "demand_diff.rds"))


# ==============================================================================
# [3] Costos marginales implícitos por especie
#
# Para cada especie:
#   (a) Filtrar country × year correspondiente
#   (b) Calcular ingreso marginal bajo Stackelberg y Cournot usando beta_i
#   (c) Estimar MC con FE: mr_yc ~ trend5y + fert_y + heatExp_yc | country
# ==============================================================================

# ── Funciones auxiliares de preparación ──────────────────────────────────────

prepare_cost_data <- function(data_full, dummy_yc_var, beta_own,
                              mc_col_s = "mc_yc_hat_s",
                              mc_col_c = "mc_yc_hat_c") {
  # Filtra, calcula MR, lags climáticos
  d <- data_full %>%
    filter(.data[[dummy_yc_var]] == 1, year %in% 1990:2019) %>%
    mutate(
      year    = as.double(year),
      trend   = floor(year - min(year, na.rm = TRUE)),
      trend5y = floor((year - min(year, na.rm = TRUE)) / 5),
      trend3y = floor((year - min(year, na.rm = TRUE)) / 3)
    ) %>%
    mutate(
      mr_yc  = price_y + beta_own * qe_yc,                          # Cournot
      mr_yc3 = (price_y + beta_own * qe_yc * (1 / (1 + n_follower_y))) *
                dummy_leader +
               (price_y + beta_own * qe_yc) * dummy_follower        # Stackelberg
    ) %>%
    # Reemplaza MR negativo por percentil 25
    mutate(
      mr_yc  = ifelse(mr_yc  < 0,
                      quantile(mr_yc[mr_yc   >= 0], 0.25, na.rm = TRUE), mr_yc),
      mr_yc3 = ifelse(mr_yc3 < 0,
                      quantile(mr_yc3[mr_yc3 >= 0], 0.25, na.rm = TRUE), mr_yc3)
    ) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      heat_lag1_yc  = dplyr::lag(heatExp_yc, 1),
      frost_lag1_yc = dplyr::lag(frostExp_yc, 1),
      diff_heat     = heatExp_yc - dplyr::lag(heatExp_yc, 3)
    ) %>%
    ungroup() %>%
    filter(!is.na(mr_yc3), !is.na(mr_yc), !is.na(fert_y),
           !is.na(heatExp_yc), !is.na(heat_lag1_yc))

  d
}

run_cost_models <- function(data_yc, type_label) {
  df_plm <- pdata.frame(data_yc, index = c("country", "year"))

  # Stackelberg (todos, líderes, seguidores)
  mc_all <- feols(mr_yc3 ~ trend5y + fert_y + heatExp_yc + heat_lag1_yc | country,
                  data = df_plm, cluster = ~country)
  mc_l   <- feols(mr_yc3 ~ trend5y + fert_y + heatExp_yc + heat_lag1_yc | country,
                  data = df_plm %>% filter(dummy_leader == 1), cluster = ~country)
  mc_f   <- feols(mr_yc3 ~ trend5y + fert_y + heatExp_yc + heat_lag1_yc | country,
                  data = df_plm %>% filter(dummy_leader == 0), cluster = ~country)

  # Cournot
  mc_cournot <- feols(mr_yc ~ trend5y + fert_y + heatExp_yc + heat_lag1_yc | country,
                      data = df_plm, cluster = ~country)

  cat("\n[3]", type_label, "– MC Stackelberg (todos):\n")
  print(summary(mc_all))

  list(all = mc_all, leader = mc_l, follower = mc_f, cournot = mc_cournot)
}

# ── [3.1] Arábica ─────────────────────────────────────────────────────────────
data_yc_ara <- prepare_cost_data(data_raw, "dummy_yc_arabica", beta_ara)
cost_models_ara <- run_cost_models(data_yc_ara, "ARÁBICA")

# ── [3.2] Robusta ─────────────────────────────────────────────────────────────
data_yc_rob <- prepare_cost_data(data_raw, "dummy_yc_robusta", beta_rob)
cost_models_rob <- run_cost_models(data_yc_rob, "ROBUSTA")


# ==============================================================================
# [4] Datos contrafactuales por especie
#     Baseline climático: promedio de heatExp_yc en 1970-1989
# ==============================================================================

build_cf_data <- function(data_yc, data_full, dummy_yc_var, cost_models) {
  mc_all     <- cost_models$all
  mc_cournot <- cost_models$cournot

  # Calor promedio 1970-1989 por país (mismo tipo)
  mean_heat_base <- data_full %>%
    filter(.data[[dummy_yc_var]] == 1, year %in% 1970:1989) %>%
    group_by(country) %>%
    summarise(mean_heat = mean(heatExp_yc, na.rm = TRUE), .groups = "drop")

  data_cf <- data_yc %>%
    left_join(mean_heat_base, by = "country") %>%
    rename(heatExp_yc_cf = mean_heat) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      heat_lag1_yc_cf = dplyr::lag(heatExp_yc_cf, 1),
      year = as.factor(year)
    ) %>%
    ungroup()

  # Predicciones de costos
  data_yc$mc_yc_hat_s   <- predict(mc_all)
  data_yc$mc_yc_hat_c   <- predict(mc_cournot)

  data_yc$mc_yc_hat_s_cf1 <- predict(
    mc_all,
    newdata = data_cf %>% mutate(heatExp_yc = heatExp_yc_cf,
                                  heat_lag1_yc = heat_lag1_yc_cf)
  )

  data_yc$mc_yc_hat_c_cf1 <- predict(
    mc_cournot,
    newdata = data_cf %>% mutate(heatExp_yc = heatExp_yc_cf,
                                  heat_lag1_yc = heat_lag1_yc_cf)
  )

  data_yc <- data_yc %>%
    filter(!is.na(mc_yc_hat_s_cf1) & !is.na(mc_yc_hat_c_cf1))

  list(data_yc = data_yc, data_cf = data_cf)
}

cf_ara <- build_cf_data(data_yc_ara, data_raw, "dummy_yc_arabica", cost_models_ara)
cf_rob <- build_cf_data(data_yc_rob, data_raw, "dummy_yc_robusta", cost_models_rob)

data_yc_ara <- cf_ara$data_yc
data_yc_rob <- cf_rob$data_yc

saveRDS(data_yc_ara, paste0(bld_data, "data_cf_arabica.rds"))
saveRDS(data_yc_rob, paste0(bld_data, "data_cf_robusta.rds"))

cat("\n[4] Datos CF Arábica:", nrow(data_yc_ara), "obs\n")
cat("    Datos CF Robusta: ", nrow(data_yc_rob), "obs\n")


# ==============================================================================
# [5] Funciones de equilibrio (parametrizadas con beta)
#
# Cada función recibe `beta` como argumento explícito en lugar de global.
# El efecto cruzado (delta) entra en la demanda inversa mediante Q_cross,
# que es la cantidad observada del otro bien en cada año.
# ==============================================================================

# ── Demanda inversa con efecto cruzado ───────────────────────────────────────
price_from_demand_diff <- function(Q_own, Q_cross, coef_demand,
                                   tea_price, import_gdp, delta) {
  coef_demand["(Intercept)"] +
    coef_demand["qe_y_ara"] * Q_own +   # nombre del regresor propio
    delta * Q_cross +                   # efecto cruzado (cantidad del otro bien)
    coef_demand["importGDP_y"] * import_gdp +
    coef_demand["teaPrices_y"] * tea_price
}

# Helper: dado coef_demand de la SUR, adaptar nombres para Arabica/Robusta
make_coef_for_sim <- function(coef_raw, own_qty_var) {
  # Renombra el coeficiente propio a "qe_y_ara" para compatibilidad
  names(coef_raw)[names(coef_raw) == own_qty_var] <- "qe_y_ara"
  coef_raw
}

# ── Reacción de seguidores ────────────────────────────────────────────────────
follower_reaction_diff <- function(data_market, Q_leaders_total, coef_demand,
                                   beta, delta, Q_cross_year) {
  followers  <- data_market %>% filter(dummy_follower == 1)
  n_followers <- nrow(followers)
  b <- -beta

  price <- price_from_demand_diff(
    Q_leaders_total, Q_cross_year, coef_demand,
    data_market$teaPrices_y[1], data_market$importGDP_y[1], delta
  )

  q_follower <- (price - followers$mc_yc_hat_s) / ((n_followers + 1) * b)

  data.frame(
    id = followers$id,
    q_follower = q_follower,
    Q_followers_total = sum(q_follower),
    mc_hat = followers$mc_yc_hat_s
  )
}

# ── FOC líder ────────────────────────────────────────────────────────────────
leader_optim_diff <- function(Q_leaders, data_market, coef_demand,
                              beta, delta, Q_cross_year) {
  leaders     <- data_market %>% filter(dummy_leader == 1)
  n_followers <- sum(data_market$dummy_follower == 1)
  b <- -beta

  Q_leaders_total <- sum(Q_leaders)
  followers <- follower_reaction_diff(
    data_market, Q_leaders_total, coef_demand, beta, delta, Q_cross_year
  )
  Q_followers_total <- sum(followers$q_follower)
  Q_total <- Q_leaders_total + Q_followers_total

  price <- price_from_demand_diff(
    Q_total, Q_cross_year, coef_demand,
    data_market$teaPrices_y[1], data_market$importGDP_y[1], delta
  )

  mr_leaders <- price - b * Q_leaders +
    b * Q_leaders * (n_followers / (n_followers + 1)) -
    leaders$mc_yc_hat_s

  data.frame(
    q_leader = Q_leaders,
    id       = leaders$id,
    mc_hat   = leaders$mc_yc_hat_s,
    price    = price,
    q_total  = Q_total,
    foc_leader = mr_leaders
  )
}

# ── Resolver Stackelberg ──────────────────────────────────────────────────────
find_stackelberg_diff <- function(data_market, coef_demand, beta, delta,
                                  Q_cross_year, initial_guess = NULL) {
  leaders <- data_market %>% filter(dummy_leader == 1)
  if (nrow(leaders) == 0) stop("No hay líderes en el mercado")
  if (is.null(initial_guess)) initial_guess <- leaders$qe_yc
  if (length(initial_guess) != nrow(leaders))
    stop("El guess inicial debe tener la misma longitud que el número de líderes")

  obj_fn <- function(Q_vec) {
    leader_optim_diff(Q_vec, data_market, coef_demand,
                      beta, delta, Q_cross_year)$foc_leader
  }

  sol <- nleqslv::nleqslv(
    x = initial_guess, fn = obj_fn,
    control = list(ftol = 1e-10, xtol = 1e-10, maxit = 1000,
                   trace = 0, allowSingular = TRUE)
  )

  if (sol$termcd != 1) {
    warning(paste("Stackelberg no convergió. Código:", sol$termcd,
                  "| Norma:", round(sqrt(sum(sol$fvec^2)), 4)))
    return(NULL)
  }

  eq <- sol$x
  names(eq) <- leaders$id
  res <- leader_optim_diff(eq, data_market, coef_demand, beta, delta, Q_cross_year)
  res$convergence <- TRUE
  res$iterations  <- sol$iter
  res$fnorm       <- sqrt(sum(sol$fvec^2))
  res
}

# ── Simulación completa de mercado (Stackelberg) ──────────────────────────────
simulate_market_diff <- function(data_market, coef_demand, beta, delta,
                                 Q_cross_year) {
  leaders <- data_market %>% filter(dummy_leader == 1)
  if (nrow(leaders) == 0) return(NULL)

  leaders_opt <- find_stackelberg_diff(
    data_market, coef_demand, beta, delta, Q_cross_year, leaders$qe_yc
  )
  if (is.null(leaders_opt)) {
    warning("No se pudo encontrar equilibrio Stackelberg")
    return(NULL)
  }

  Q_leaders_total   <- sum(leaders_opt$q_leader)
  followers_opt     <- follower_reaction_diff(
    data_market, Q_leaders_total, coef_demand, beta, delta, Q_cross_year
  )
  Q_followers_total <- sum(followers_opt$q_follower)
  Q_total           <- Q_leaders_total + Q_followers_total

  price <- price_from_demand_diff(
    Q_total, Q_cross_year, coef_demand,
    unique(data_market$teaPrices_y), unique(data_market$importGDP_y), delta
  )

  profits <- data_market %>%
    left_join(
      bind_rows(
        leaders_opt  %>% select(id, opt_quantity = q_leader),
        followers_opt %>% select(id, opt_quantity = q_follower)
      ),
      by = "id"
    ) %>%
    mutate(
      price    = price,
      quantity = ifelse(is.na(opt_quantity), 0, opt_quantity),
      revenue  = (quantity * price * 60 * 2.20462) / 100,
      profit   = (quantity * (price - mc_yc_hat_s) * 60 * 2.20462) / 100,
      type     = case_when(
        dummy_leader == 1  ~ "leader",
        dummy_follower == 1 ~ "follower",
        TRUE               ~ "other"
      )
    ) %>%
    select(date, country, id, dummy_leader, type, quantity, revenue,
           profit, mc_yc_hat_s, price)

  profit_leaders   <- (sum(leaders_opt$q_leader *
                            (price - leaders_opt$mc_hat)) * 60 * 2.20462) / 100
  profit_followers <- (sum(followers_opt$q_follower *
                            (price - followers_opt$mc_hat)) * 60 * 2.20462) / 100

  price_intercept <- price_from_demand_diff(
    0, Q_cross_year, coef_demand,
    unique(data_market$teaPrices_y), unique(data_market$importGDP_y), delta
  )
  cs <- ((0.5 * (price_intercept - price) * Q_total) * 60 * 2.20462) / 100

  list(
    market_summary = tibble(
      date             = unique(data_market$date),
      year             = unique(data_market$year),
      month_num        = unique(data_market$month_num),
      Q_leaders_total  = Q_leaders_total,
      Q_followers_total= Q_followers_total,
      Q_total          = Q_total,
      price            = price,
      profit_leaders   = profit_leaders,
      profit_followers = profit_followers,
      consumer_surplus = cs,
      total_surplus    = profit_leaders + profit_followers + cs
    ),
    country_profits = profits
  )
}

# ── FOC simultáneo (Cournot) ──────────────────────────────────────────────────
firm_foc_diff <- function(Q_firms, data_market, coef_demand, beta, delta,
                          Q_cross_year) {
  b       <- -beta
  Q_total <- sum(Q_firms)
  price   <- price_from_demand_diff(
    Q_total, Q_cross_year, coef_demand,
    data_market$teaPrices_y[1], data_market$importGDP_y[1], delta
  )
  foc <- price - b * Q_firms - data_market$mc_yc_hat_s
  data.frame(q_firm = Q_firms, id = data_market$id,
             mc_hat = data_market$mc_yc_hat_s,
             price = price, q_total = Q_total, foc = foc)
}

find_cournot_diff <- function(data_market, coef_demand, beta, delta,
                              Q_cross_year) {
  obj_fn <- function(Q_vec) {
    firm_foc_diff(Q_vec, data_market, coef_demand, beta, delta, Q_cross_year)$foc
  }
  sol <- nleqslv::nleqslv(
    x = data_market$qe_yc, fn = obj_fn,
    control = list(ftol = 1e-10, xtol = 1e-10, maxit = 1000,
                   trace = 0, allowSingular = TRUE)
  )
  if (sol$termcd != 1) {
    warning(paste("Cournot no convergió. Código:", sol$termcd))
    return(NULL)
  }
  res <- firm_foc_diff(sol$x, data_market, coef_demand, beta, delta, Q_cross_year)
  res$convergence <- TRUE
  res
}

simulate_market_cournot_diff <- function(data_market, coef_demand, beta, delta,
                                         Q_cross_year) {
  if (nrow(data_market) == 0) return(NULL)
  firms_opt <- find_cournot_diff(data_market, coef_demand, beta, delta, Q_cross_year)
  if (is.null(firms_opt)) return(NULL)

  Q_total <- sum(firms_opt$q_firm)
  price   <- unique(firms_opt$price)

  profits <- data_market %>%
    left_join(firms_opt %>% select(id, opt_quantity = q_firm), by = "id") %>%
    mutate(
      price    = price,
      quantity = ifelse(is.na(opt_quantity), 0, opt_quantity),
      revenue  = (quantity * price * 60 * 2.20462) / 100,
      profit   = (quantity * (price - mc_yc_hat_s) * 60 * 2.20462) / 100,
      type     = "cournot_firm"
    ) %>%
    select(date, country, id, type, quantity, revenue, profit, mc_yc_hat_s, price)

  price_intercept <- price_from_demand_diff(
    0, Q_cross_year, coef_demand,
    unique(data_market$teaPrices_y), unique(data_market$importGDP_y), delta
  )
  cs <- ((0.5 * (price_intercept - price) * Q_total) * 60 * 2.20462) / 100

  list(
    market_summary = tibble(
      date             = unique(data_market$date),
      year             = unique(data_market$year),
      month_num        = unique(data_market$month_num),
      n_firms          = nrow(data_market),
      Q_total          = Q_total,
      price            = price,
      total_profit     = sum(profits$profit),
      consumer_surplus = cs,
      total_surplus    = sum(profits$profit) + cs
    ),
    country_profits = profits
  )
}


# ==============================================================================
# Función que corre los 4 escenarios para un tipo de café
# ==============================================================================

run_counterfactuals <- function(data_yc_type, coef_demand, beta, delta,
                                Q_cross_lookup, type_label) {
  cat("\n[5] Contrafactuales –", type_label, "\n")

  # Preparar lista de mercados anuales (con al menos un líder)
  market_list <- data_yc_type %>%
    group_by(year) %>%
    filter(sum(dummy_leader) > 0) %>%
    ungroup() %>%
    group_by(year) %>%
    group_split()

  run_scenario <- function(mc_col, sim_fn) {
    map(market_list, function(mkt) {
      yr <- as.character(unique(mkt$year))
      Q_cross <- Q_cross_lookup[[yr]]
      if (is.null(Q_cross) || is.na(Q_cross)) Q_cross <- 0
      mkt$mc_yc_hat_s <- mkt[[mc_col]]
      sim_fn(mkt, coef_demand, beta, delta, Q_cross)
    })
  }

  cat("  CF0 – Stackelberg baseline\n")
  cf0 <- run_scenario("mc_yc_hat_s",   simulate_market_diff)
  cat("  CF1 – Stackelberg contrafactual\n")
  cf1 <- run_scenario("mc_yc_hat_s_cf1", simulate_market_diff)
  cat("  CF2 – Cournot baseline\n")
  cf2 <- run_scenario("mc_yc_hat_c",   simulate_market_cournot_diff)
  cat("  CF3 – Cournot contrafactual\n")
  cf3 <- run_scenario("mc_yc_hat_c_cf1", simulate_market_cournot_diff)

  # Extraer resultados
  extract_y  <- function(cf, sc) map_dfr(cf, ~.x$market_summary,   .id = "market_id") %>% mutate(scenario = sc)
  extract_yc <- function(cf, sc) map_dfr(cf, ~.x$country_profits, .id = "market_id") %>% mutate(scenario = sc)

  cf0_y  <- extract_y(cf0,  "CF0");  cf0_yc  <- extract_yc(cf0,  "CF0")
  cf1_y  <- extract_y(cf1,  "CF1");  cf1_yc  <- extract_yc(cf1,  "CF1")
  cf2_y  <- extract_y(cf2,  "CF2");  cf2_yc  <- extract_yc(cf2,  "CF2")
  cf3_y  <- extract_y(cf3,  "CF3");  cf3_yc  <- extract_yc(cf3,  "CF3")

  # Añadir dummy_leader para CF2/CF3 (Cournot no lo tiene en country_profits)
  add_leader <- function(df) {
    df %>%
      mutate(dummy_leader = case_when(
        country %in% c("Brazil", "Colombia", "Viet Nam") ~ 1L,
        TRUE ~ 0L
      ))
  }
  cf2_yc <- add_leader(cf2_yc)
  cf3_yc <- add_leader(cf3_yc)

  all_y  <- bind_rows(cf0_y,  cf1_y,  cf2_y,  cf3_y)  %>% arrange(date, scenario)
  all_yc <- bind_rows(cf0_yc, cf1_yc, cf2_yc, cf3_yc) %>% arrange(date, country, scenario)

  list(results_y = all_y, results_yc = all_yc)
}


# ==============================================================================
# [5] Correr contrafactuales por especie
#
# Para cada año, Q_cross es la cantidad observada del otro tipo en ese año.
# Esto mantiene el efecto cruzado de la demanda constante al nivel observado,
# de modo que la simulación de cada mercado es condicional al otro mercado.
# ==============================================================================

# Construir lookup de cantidades cruzadas por año
q_cross_ara <- data_wide %>%
  select(year, Q_cross = qe_y_rob) %>%
  split(.$year) %>%
  lapply(function(x) x$Q_cross[1])

q_cross_rob <- data_wide %>%
  select(year, Q_cross = qe_y_ara) %>%
  split(.$year) %>%
  lapply(function(x) x$Q_cross[1])

# ── Ajustar coeficientes para la función de precio ────────────────────────────
# La función price_from_demand_diff espera "qe_y_ara" como nombre del regresor propio
coeff_ara_sim <- coeff_ara   # ya tiene "qe_y_ara" como nombre (renombrado en sección 2)
coeff_rob_sim <- coeff_rob
names(coeff_rob_sim)[names(coeff_rob_sim) == "qe_y_rob"] <- "qe_y_ara"  # alias para compatibilidad

# ── Correr ────────────────────────────────────────────────────────────────────
results_ara <- run_counterfactuals(
  data_yc_type  = data_yc_ara,
  coef_demand   = coeff_ara_sim,
  beta          = beta_ara,
  delta         = delta_ara,
  Q_cross_lookup = q_cross_ara,
  type_label    = "ARÁBICA"
)

results_rob <- run_counterfactuals(
  data_yc_type  = data_yc_rob,
  coef_demand   = coeff_rob_sim,
  beta          = beta_rob,
  delta         = delta_rob,
  Q_cross_lookup = q_cross_rob,
  type_label    = "ROBUSTA"
)


# ==============================================================================
# [6] Resumen y guardado
# ==============================================================================

build_summary <- function(results_y, results_yc) {
  summary_y <- results_y %>%
    group_by(scenario) %>%
    summarise(
      avg_price        = mean(price,            na.rm = TRUE),
      quantity         = mean(Q_total,          na.rm = TRUE),
      profit_leader    = mean(profit_leaders,   na.rm = TRUE),
      profit_followers = mean(profit_followers, na.rm = TRUE),
      consumer_surplus = mean(consumer_surplus, na.rm = TRUE),
      total_surplus    = mean(total_surplus,    na.rm = TRUE),
      .groups = "drop"
    )

  summary_c <- results_yc %>%
    group_by(country, dummy_leader, scenario) %>%
    summarise(
      profit   = sum(profit,   na.rm = TRUE),
      quantity = sum(quantity, na.rm = TRUE),
      mc       = mean(mc_yc_hat_s, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    pivot_wider(
      id_cols     = c(country, dummy_leader),
      names_from  = scenario,
      values_from = c(profit, quantity, mc)
    )

  list(summary_y = summary_y, summary_c = summary_c)
}

sum_ara <- build_summary(results_ara$results_y, results_ara$results_yc)
sum_rob <- build_summary(results_rob$results_y, results_rob$results_yc)

cat("\n[6] Guardando RDS en", bld_data, "\n")

saveRDS(results_ara$results_y,  paste0(bld_data, "results_y_arabica.rds"))
saveRDS(results_ara$results_yc, paste0(bld_data, "results_yc_arabica.rds"))
saveRDS(sum_ara$summary_y,      paste0(bld_data, "summary_arabica.rds"))
saveRDS(sum_ara$summary_c,      paste0(bld_data, "summary_c_arabica.rds"))

saveRDS(results_rob$results_y,  paste0(bld_data, "results_y_robusta.rds"))
saveRDS(results_rob$results_yc, paste0(bld_data, "results_yc_robusta.rds"))
saveRDS(sum_rob$summary_y,      paste0(bld_data, "summary_robusta.rds"))
saveRDS(sum_rob$summary_c,      paste0(bld_data, "summary_c_robusta.rds"))

cat("  results_y_arabica.rds\n")
cat("  results_yc_arabica.rds\n")
cat("  summary_arabica.rds\n")
cat("  summary_c_arabica.rds\n")
cat("  results_y_robusta.rds\n")
cat("  results_yc_robusta.rds\n")
cat("  summary_robusta.rds\n")
cat("  summary_c_robusta.rds\n")
cat("  demand_diff.rds\n")
cat("\n[estimation_diff.R] Completado.\n")
