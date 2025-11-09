# ------------------------------------------------------------------------------
# [0] Preliminary
# ------------------------------------------------------------------------------

# Packages
library(dplyr)
library(tidyr)
library(ivreg)
library(sandwich)
library(lmtest)
library(broom)
library(openxlsx)
library(fixest)
library(stargazer)
library(plm)
library(nleqslv)
library(purrr)
library(ggplot2)
library(lubridate)
library(lfe)
library(slider) 
library(nleqslv)
library(modelsummary)

# Data
  data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data.rds")

  data <- data %>%
    filter(year >= 1991)

# ------------------------------------------------------------------------------
# [1] Demand estimation
# ------------------------------------------------------------------------------
{
# [1.1.] Data for demand estimation
  # Filters
  demand_data <- data %>%
    filter(dummy_ym == 1) %>%
    select(date, year, month_num, price_ym, qe_ym, importGDP_ym, teaPrices_ym, farm_prices_ym,
      tas_ym, oni_value, fert_ym, gdd_ym, hdd_ym, fdd_ym, 
      gdd_brazil_ym, gdd_colombia_ym, gdd_indonesia_ym, gdd_vietnam_ym, gdd_leaders_ym,
      disease1, disease2, frost1, frost2, droughts, ica_lapse) %>%
    drop_na()

# [1.2.] Demand estimation models
# [1.2.1.] OLS
  ols1 <- lm(price_ym ~ qe_ym , 
          data = demand_data)
  # Store coefficients
    coeff_ols1 <- coef(ols1)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols1 <- predict(ols1)
    demand_data$edemand_ols1 <- (1/coeff_ols1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols1 <- mean(demand_data$edemand_ols1, na.rm = TRUE)

  ols2 <- lm(price_ym ~ qe_ym  + importGDP_ym,
          data = demand_data)
  # Store coefficients
    coeff_ols2 <- coef(ols2)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols2 <- predict(ols2)
    demand_data$edemand_ols2 <- (1/coeff_ols2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols2 <- mean(demand_data$edemand_ols2, na.rm = TRUE)

  ols3 <- lm(price_ym ~ qe_ym  + importGDP_ym + teaPrices_ym,
          data = demand_data)
  # Store coefficients
    coeff_ols3 <- coef(ols3)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols3 <- predict(ols3)
    demand_data$edemand_ols3 <- (1/coeff_ols3["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols3 <- mean(demand_data$edemand_ols3, na.rm = TRUE)

# [1.2.2.] IV
  iv1 <- ivreg(price_ym ~ qe_ym |
          farm_prices_ym + 
          hdd_ym + fdd_ym,
          #gdd_ym,
          data = demand_data)
  # Store coefficients
    coeff_iv1 <- coef(iv1)
  # Predict and elasticity
    demand_data$q_demand_fitted_iv1 <- predict(iv1)
    demand_data$edemand_iv1 <- (1/coeff_iv1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_iv1 <- mean(demand_data$edemand_iv1, na.rm = TRUE)

  iv2 <- ivreg(price_ym ~ qe_ym  + importGDP_ym |
          farm_prices_ym + 
          #gdd_ym  +
          hdd_ym + fdd_ym +
          importGDP_ym,
          data = demand_data)
  # Store coefficients
    coeff_iv2 <- coef(iv2)
  # Predict and elasticity
    demand_data$q_demand_fitted_iv2 <- predict(iv2)
    demand_data$edemand_iv2 <- (1/coeff_iv2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_iv2 <- mean(demand_data$edemand_iv2, na.rm = TRUE)

  iv3 <- ivreg(price_ym ~ qe_ym  + importGDP_ym + teaPrices_ym |
          farm_prices_ym + 
          #gdd_ym +
          hdd_ym + 
          #fdd_ym +
          importGDP_ym + teaPrices_ym,
          data = demand_data)
  summary(iv3)
  # Store coefficients
    coeff_iv3 <- coef(iv3)
  # Predict and elasticity
    demand_data$q_demand_fitted_iv3 <- predict(iv3)
    demand_data$edemand_iv3 <- (1/coeff_iv3["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_iv3 <- mean(demand_data$edemand_iv3, na.rm = TRUE)

# [1.2.3.] Heterocedasticidad. Autocorrelación. Cluster
  cov_ols1 <- vcovHC(ols1, method = "arellano", cluster = "year", type = "HC3")
  cov_ols2 <- vcovHC(ols2, method = "arellano", cluster = "year", type = "HC3")
  cov_ols3 <- vcovHC(ols3, method = "arellano", cluster = "year", type = "HC3")
  cov_iv1 <- vcovHC(iv1, method = "arellano", cluster = "year", type = "HC3")
  cov_iv2 <- vcovHC(iv2, method = "arellano", cluster = "year", type = "HC3")
  cov_iv3 <- vcovHC(iv3, method = "arellano", cluster = "year", type = "HC3")

stargazer(ols1, ols2, ols3, iv1, iv2, iv3,
      se = list(sqrt(diag(cov_ols1)), sqrt(diag(cov_ols2)), sqrt(diag(cov_ols3)),
                    sqrt(diag(cov_iv1)), sqrt(diag(cov_iv2)), sqrt(diag(cov_iv3))),
      title = "Estimaciones de la Demanda Mundial de Café",
      #align = TRUE,
      dep.var.labels = "Precio (price_ym)",
      column.labels = c("OLS1", "OLS2", "OLS3", "IV1", "IV2", "IV3"),
      covariate.labels = c("$Q_{t}$ (Exportaciones de café)", "$ICA$ ($0/1$)", 
                            "$X_{t}$ (PIB de importadores)", "$Z_{t}$ (Precio del té)", 
                            "Constante"),
      #omit.stat = c("f", "ser"),
      notes = c("Errores estándar robustos clusterizados por año entre paréntesis.",
                "*p<0.1; **p<0.05; ***p<0.01"),
      #notes.append = TRUE,
      type = "latex")
}
# ------------------------------------------------------------------------------
# [2] Implicit marginal costs
# ------------------------------------------------------------------------------
{
# [2.1.] Data for marginal costs
  aux <- demand_data %>%
    select(year, month_num, q_demand_fitted_iv3, edemand_iv3)

  data_ymc_0 <- left_join(data, aux, by = c("year", "month_num"))

  data_ymc_0 <- data_ymc_0 %>%
    filter(export_dummy == 1) %>%
    mutate(
      # Cournot
      mr_ymc = price_ym + coeff_iv3["qe_ym"] * qe_ymc,
      # Weighted (Cournot)
      mr_ymc2 = price_ym * (1 + share_ymce/mean_elast_iv3),
    # Stackelberg
      mr_ymc3 = (price_ym + coeff_iv3["qe_ym"] * qe_ymc * (1 / (1 + n_follower_ym))) * dummy_leader + (price_ym + coeff_iv3["qe_ym"] * qe_ymc) * dummy_follower)
}
# ------------------------------------------------------------------------------
# [3] Marginal cost estimation
# ------------------------------------------------------------------------------
{
  data_ymc <- data_ymc_0 %>%
    filter(!is.na(mr_ymc3),
      !is.na(farm_prices_ymc),
      !is.na(fert_ym),
      !is.na(gdd_ymc),
      !is.na(importGDP_ym),
      !is.na(country),
      !is.na(year))

  data_ymc <- pdata.frame(
    data_ymc,
    index = c("country", "date")
  )

  data_ymc <- data_ymc %>%
    arrange(country, date) %>%
    group_by(country) %>%
    mutate(
      lag_gdd_ym_12 = lag(gdd_ym, 36),
      lag_hdd_ym_12 = lag(hdd_ym, 36),
      lag_fdd_ym_12 = lag(fdd_ym, 36)
    ) %>%
    ungroup()

  model_mc_1 <- feols(
    mr_ymc3 ~
      farm_prices_ymc + fert_ym +
      gdd_ym + hdd_ym + fdd_ym | year + country,
      #lag_gdd_ym_12 + lag_hdd_ym_12 + lag_fdd_ym_12| year + country,
      #hdd_ymc + fdd_ymc 
      #gdd_ymc_id | year + country,
      data = data_ymc,
      cluster = ~country
    )

  summary(model_mc_1)

  model_mc_1 <- summary(model_mc_1, se = "cluster")

  modelsummary(model_mc_1,
      title = "Estimación de Costos Marginales",
      output = "latex",
      coef_rename = c(
        "farm_prices_ymc" = "Precio a productor", 
        "fert_ym" = "Precio fertilizantes",
        #"frosts" = "Eventos de helada",
        "hdd_ymc" = "Grados-día calor (HDD)",
        "fdd_ymc" = "Grados-día frío (FDD)"
        ),
      stars = TRUE,
      notes = c("Errores estándar clusterizados por país.",
                "Efectos fijos de año y país incluidos."))


  # Store coefficients
    coeff_mc_1 <- coef(model_mc_1)
  # Predict
    data_ymc$mc_ymc_hat <- predict(model_mc_1)
}
# ------------------------------------------------------------------------------
# [4] Counterfactual Analysis (Stackelberg Model)
# ------------------------------------------------------------------------------
{
# Define key functions for counterfactual analysis
  # 1. Price from demand function (inverse demand)
    price_from_demand <- function(Q_total, coef_demand, tea_price, import_gdp) {
      price <- coef_demand["(Intercept)"] + 
      coef_demand["qe_ym"] * Q_total + 
      coef_demand["importGDP_ym"] * import_gdp + 
      coef_demand["teaPrices_ym"] * tea_price
      return(price)
      }
  # 2. Follower reaction function (Cournot)
    follower_reaction <- function(data_market, Q_leaders_total, coef_demand) {
      followers <- data_market %>% filter(dummy_follower == 1)
      n_followers <- nrow(followers)
      b <- -coef_demand["qe_ym"]  # Slope of demand curve
    # Market price given leader's quantity
      price <- price_from_demand(Q_leaders_total, coef_demand, 
                              data_market$teaPrices_ym[1], 
                              data_market$importGDP_ym[1])#,
                              #data_market$ica_lapse[1])
    # Reaction
      q_follower <- (price - followers$mc_ymc_hat) / ((n_followers + 1) * b)
      Q_followers_total <- sum(q_follower)
      return(data.frame(
        id = followers$id, 
        q_follower = q_follower, 
        Q_followers_total = Q_followers_total, 
        mc_hat = followers$mc_ymc_hat))
        }
  # 3. Leader optimization function
    leader_optim <- function(Q_leaders, data_market, coef_demand) {
      leaders <- data_market %>% filter(dummy_leader == 1)
      n_leaders <- nrow(leaders)
      n_followers <- sum(data_market$dummy_follower == 1)
      b <- -coef_demand["qe_ym"]
      Q_leaders_total <- sum(Q_leaders)
    # Reacción de seguidores
      followers <- follower_reaction(data_market, Q_leaders_total, coef_demand)
      Q_followers_total <- sum(followers$q_follower)
    # Precio de mercado
      Q_total <- Q_leaders_total + Q_followers_total
      price <- price_from_demand(
        Q_total, coef_demand, 
        data_market$teaPrices_ym[1], 
        data_market$importGDP_ym[1])
    # Calcula FOC
      mr_leaders <- price - b * Q_leaders + b * Q_leaders * (n_followers / (n_followers + 1)) - leaders$mc_ymc_hat
    return(data.frame(
      q_leader = Q_leaders,
      id = leaders$id,
      mc_hat = leaders$mc_ymc_hat,
      price = price,
      q_total = Q_total,
      foc_leader = mr_leaders
      ))}
  # 4. Find Stackelberg
    find_stackelberg <- function(data_market, coef_demand, initial_guess = NULL) {
    # Filtrar líderes
      leaders <- data_market %>% filter(dummy_leader == 1)
      if (nrow(leaders) == 0) {
        stop("No hay líderes en el mercado")
      }
    # Guess inicial si no se proporciona
      if (is.null(initial_guess)) {
        initial_guess <- leaders$qe_ymc  # Usar cantidades observadas como guess
      }
    # Verificar dimensiones
      if (length(initial_guess) != nrow(leaders)) {
        stop("El guess inicial debe tener la misma longitud que el número de líderes")
      }
    # Función objetivo para el solver: FOC = 0
      objective_function <- function(Q_leaders_vec) {
        result <- leader_optim(Q_leaders_vec, data_market, coef_demand)
        return(result$foc_leader)  # Queremos FOC = 0
        }
    # Resolver el sistema de ecuaciones FOC = 0
      solution <- nleqslv::nleqslv(
        x = initial_guess,
        fn = objective_function,
        control = list(
          ftol = 1e-10,       # Tolerancia de función
          xtol = 1e-10,       # Tolerancia de variable
          maxit = 1000,       # Máximo de iteraciones
          trace = 1,          # Mostrar progreso
          allowSingular = TRUE))
    # Verificar convergencia
      if (solution$termcd != 1) {
        warning(paste(
          "El solver no convergió. Código de terminación:", solution$termcd,
          "\nMensaje:", solution$message,
          "\nNorma final:", solution$fvec))
        return(NULL)}
    # Obtener resultados de la solución
      equilibrium_quantities <- solution$x
      names(equilibrium_quantities) <- leaders$id
    # Calcular todas las variables en el equilibrio
      equilibrium_results <- leader_optim(equilibrium_quantities, data_market, coef_demand)
    # Añadir información de convergencia
      equilibrium_results$convergence <- solution$termcd == 1
      equilibrium_results$iterations <- solution$iter
      equilibrium_results$fnorm <- sqrt(sum(solution$fvec^2))
      equilibrium_results$message <- solution$message
      return(equilibrium_results)}
  # 5. Market simulation function
    simulate_market <- function(data_market, coef_demand) {
    # Verificar que hay líderes
      leaders <- data_market %>% filter(dummy_leader == 1)
      if (nrow(leaders) == 0) return(NULL)
    # Calcular cantidad inicial de líderes
      Q_leaders_00 <- leaders$qe_ymc
    # Optimiza
      leaders_opt <- find_stackelberg(data_market, coef_demand, leaders$qe_ymc)
      if (is.null(leaders_opt)) {
        warning("No se pudo encontrar equilibrio Stackelberg")
      return(NULL)}
      Q_leaders_total <- sum(leaders_opt$q_leader)
      followers_opt <- follower_reaction(data_market, Q_leaders_total, coef_demand)
      Q_followers_total <- sum(followers_opt$q_follower)
      Q_total <- Q_leaders_total + Q_followers_total
      price <- price_from_demand(
        Q_total, coef_demand, 
        unique(data_market$teaPrices_ym), 
        unique(data_market$importGDP_ym))
    # Beneficio por país
      profits <- data_market %>%
        left_join(
          bind_rows(
            leaders_opt %>% select(id, opt_quantity = q_leader),
            followers_opt %>% select(id, opt_quantity = q_follower)
            ),
          by = "id"
          ) %>%
        mutate(
          price = price,
          quantity = ifelse(is.na(opt_quantity), 0, opt_quantity),
          revenue = (quantity * price * 60 * 2.20462) / 100, # Ajuste por unidad de medida (MM USD)
          profit = (quantity * (price - mc_ymc_hat) * (60 * 2.20462)) / 100, # Ajuste por unidad de medida (MM USD)
          type = case_when(
            dummy_leader == 1 ~ "leader",
            dummy_follower == 1 ~ "follower", 
            TRUE ~ "other")) %>%
        select(date, country, id, dummy_leader, type, quantity, revenue, profit, mc_ymc_hat, price)
    # Beneficio TOTAL de líderes (suma individual)
      profit_leaders <- (sum(leaders_opt$q_leader * (price - leaders_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
      profit_followers <- (sum(followers_opt$q_follower * (price - followers_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
    # Consumer surplus
      price_intercept <- price_from_demand(
        0, coef_demand, 
        unique(data_market$teaPrices_ym), 
        unique(data_market$importGDP_ym))
      consumer_surplus <- ((0.5 * (price_intercept - price) * Q_total) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
    # Results
      results <- list(
        market_summary = tibble(
          date = unique(data_market$date),
          year = unique(data_market$year),
          month_num = unique(data_market$month_num),
          Q_leaders_total = Q_leaders_total,
          Q_followers_total = Q_followers_total,
          Q_total = Q_total,
          price = price,
          profit_leaders = profit_leaders,
          profit_followers = profit_followers,
          consumer_surplus = consumer_surplus,
          total_surplus = profit_leaders + profit_followers + consumer_surplus),
        country_profits = profits)
    return(results)
    }
}
# ------------------------------------------------------------------------------
# [5] Run Counterfactual Analysis
# ------------------------------------------------------------------------------
{
# Prepare data for counterfactuals
data_ymc <- data_ymc %>%
  group_by(date) %>%
  filter(sum(dummy_leader) > 0) %>%  # Keep only markets with leaders
  ungroup()

# Split data by market (time period)
market_list <- data_ymc %>%
  group_by(date) %>%
  group_split()

# Run baseline scenario
baseline_results <- map(market_list, ~simulate_market(.x, coeff_iv3))
baseline_results_ym <- map_dfr(baseline_results, ~.x$market_summary, .id = "market_id") %>% 
  mutate(scenario = "Baseline")
baseline_results_ymc <- map_dfr(baseline_results, ~ .x$country_profits, .id = "market_id") %>% 
  mutate(scenario = "Baseline")

# Run counterfactual scenarios
# Scenario 1
cf1_processed <- map(market_list, function(market) {
  market$mc_ymc_hat <- predict(
    model_mc_1, 
    newdata = market %>% mutate(
      #disease1 = 0,
      #frosts = 0,
      hdd_ym = 0,
      fdd_ym = 0
      ),
    fixef = FALSE)
  simulate_market(market, coeff_iv3)
}) 

cf1_results_ym <- map_dfr(cf1_processed, ~.x$market_summary, .id = "market_id") %>% 
  mutate(scenario = "CF1")

cf1_results_ymc <- map_dfr(cf1_processed, ~ .x$country_profits, .id = "market_id") %>% 
  mutate(scenario = "CF1")

# # Scenario 2
# cf2_results <- map_dfr(market_list, function(market) {
#   market$mc_ymc_hat <- predict(model_mc_1,
#                                newdata = market %>% mutate(
#                                  disease1 = 0,
#                                  frost1 = 0,
#                                  hdd_ym = 0.75,
#                                  fdd_ym = 0.75,
#                                  ),
#                                fixef = FALSE)
#   res <- simulate_market(market, coeff_iv3)
#   res
# }) %>%
#   mutate(scenario = "CF2")
# 
# # Scenario 3
# cf3_results <- map_dfr(market_list, function(market) {
#   market$mc_ymc_hat <- predict(model_mc_1,
#                                newdata = market %>% mutate(
#                                  disease1 = 0,
#                                  frost1 = 0,
#                                  hdd_ym = 0.5,
#                                  fdd_ym = 0.5
#                                ),
#                                fixef = FALSE)
#   simulate_market(market, coeff_iv3)
# }) %>%
#   mutate(scenario = "CF3")

# Combine all results
all_results_ym <- bind_rows(
  baseline_results_ym,
  cf1_results_ym
  ) %>%
  arrange(date, scenario)

all_results_ymc <- bind_rows(
  baseline_results_ymc,
  cf1_results_ymc
  ) %>%
  arrange(date, country, scenario)

saveRDS(all_results_ym, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_ym.rds")
saveRDS(all_results_ymc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_ymc.rds")
}
# ------------------------------------------------------------------------------
# [6] Analyze and Visualize Results
# ------------------------------------------------------------------------------
{
# Summary statistics by scenario
summary_ym <- all_results_ym %>%
  group_by(scenario) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    quantity = sum(Q_total, na.rm = TRUE),
    profit_leader = sum(profit_leaders, na.rm = TRUE),
    profit_followers = sum(profit_followers, na.rm = TRUE),
    consumer_surplus = sum(consumer_surplus, na.rm = TRUE),
    total_surplus = sum(total_surplus, na.rm = TRUE),
    .groups = "drop"
    )

saveRDS(summary_ym, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary_ym.rds")

# Profit comparison by country
summary_ymc <- all_results_ymc %>%
  group_by(country, dummy_leader, scenario) %>%
  summarise(
    profit = sum(profit, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),
    mc = mean(mc_ymc_hat, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
  pivot_wider(
    id_cols = c(country, dummy_leader),
    names_from = scenario,
    values_from = c(profit, quantity, mc)
    )

saveRDS(summary_ymc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary_ymc.rds")
}
