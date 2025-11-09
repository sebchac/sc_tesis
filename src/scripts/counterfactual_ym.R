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
  data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data_cf_ym.rds")

# ------------------------------------------------------------------------------
# [4] Counterfactual Analysis (Stackelberg Model)
# ------------------------------------------------------------------------------
{
# Define key functions for counterfactual analysis
  # 1. Price from demand function (inverse demand)
    price_from_demand <- function(Q_total, coef_demand, tea_price, import_gdp, ica_lapse) {
      price <- coef_demand["(Intercept)"] + 
      coef_demand["qe_ym"] * Q_total + 
      coef_demand["importGDP_ym"] * import_gdp + 
      coef_demand["teaPrices_ym"] * tea_price +
      coef_demand["ica_lapse"] * ica_lapse
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
                            data_market$importGDP_ym[1],
                            data_market$ica_lapse[1])
    # Reaction
      q_follower <- (price - followers$mc_ymc_hat_1) / ((n_followers + 1) * b)
      Q_followers_total <- sum(q_follower)
      return(data.frame(
        id = followers$id, 
        q_follower = q_follower, 
        Q_followers_total = Q_followers_total, 
        mc_hat = followers$mc_ymc_hat_1))
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
        data_market$importGDP_ym[1],
        data_market$ica_lapse[1])
    # Calcula FOC
      mr_leaders <- price - b * Q_leaders + b * Q_leaders * (n_followers / (n_followers + 1)) - leaders$mc_ymc_hat_1
    return(data.frame(
      q_leader = Q_leaders,
      id = leaders$id,
      mc_hat = leaders$mc_ymc_hat_1,
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
        unique(data_market$importGDP_ym),
        unique(data_market$ica_lapse))
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
          profit = (quantity * (price - mc_ymc_hat_1) * (60 * 2.20462)) / 100, # Ajuste por unidad de medida (MM USD)
          type = case_when(
            dummy_leader == 1 ~ "leader",
            dummy_follower == 1 ~ "follower", 
            TRUE ~ "other")) %>%
        select(date, country, id, dummy_leader, type, quantity, revenue, profit, mc_ymc_hat_1, price)
    # Beneficio TOTAL de líderes (suma individual)
      profit_leaders <- (sum(leaders_opt$q_leader * (price - leaders_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
      profit_followers <- (sum(followers_opt$q_follower * (price - followers_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
    # Consumer surplus
      price_intercept <- price_from_demand(
        0, coef_demand, 
        unique(data_market$teaPrices_ym), 
        unique(data_market$importGDP_ym),
        unique(data_market$ica_lapse[1]))
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
  market$mc_ymc_hat_1 <- predict(
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
#   market$mc_ymc_hat_1 <- predict(model_mc_1,
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
#   market$mc_ymc_hat_1 <- predict(model_mc_1,
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
    mc = mean(mc_ymc_hat_1, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
  pivot_wider(
    id_cols = c(country, dummy_leader),
    names_from = scenario,
    values_from = c(profit, quantity, mc)
    )

saveRDS(summary_ymc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary_ymc.rds")
}
