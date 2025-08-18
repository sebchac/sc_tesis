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

# Data
data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/data.rds")

# ------------------------------------------------------------------------------
# [1] Demand estimation
# ------------------------------------------------------------------------------

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

ols1 <- lm(price_ym ~ qe_ym + ica_lapse,
                 data = demand_data)
# Store coefficients
coeff_ols1 <- coef(ols1)
# Predict and elasticity
demand_data$q_demand_fitted_ols1 <- predict(ols1)
demand_data$edemand_ols1 <- (1/coeff_ols1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
mean_elast_ols1 <- mean(demand_data$edemand_ols1, na.rm = TRUE)

ols2 <- lm(price_ym ~ qe_ym + ica_lapse + importGDP_ym,
              data = demand_data)
# Store coefficients
coeff_ols2 <- coef(ols2)
# Predict and elasticity
demand_data$q_demand_fitted_ols2 <- predict(ols2)
demand_data$edemand_ols2 <- (1/coeff_ols2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
mean_elast_ols2 <- mean(demand_data$edemand_ols2, na.rm = TRUE)

ols3 <- lm(price_ym ~ qe_ym + ica_lapse + importGDP_ym + teaPrices_ym,
                 data = demand_data)
# Store coefficients
coeff_ols3 <- coef(ols3)
# Predict and elasticity
demand_data$q_demand_fitted_ols3 <- predict(ols3)
demand_data$edemand_ols3 <- (1/coeff_ols3["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
mean_elast_ols3 <- mean(demand_data$edemand_ols3, na.rm = TRUE)


# [1.2.2.] IV

iv1 <- ivreg(price_ym ~ qe_ym + ica_lapse|
               ica_lapse + farm_prices_ym + gdd_ym,
                   data = demand_data)
# Store coefficients
coeff_iv1 <- coef(iv1)
# Predict and elasticity
demand_data$q_demand_fitted_iv1 <- predict(iv1)
demand_data$edemand_iv1 <- (1/coeff_iv1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
mean_elast_iv1 <- mean(demand_data$edemand_iv1, na.rm = TRUE)

iv2 <- ivreg(price_ym ~ qe_ym + ica_lapse + importGDP_ym |
               ica_lapse + farm_prices_ym + gdd_ym  +
                     importGDP_ym,
                   data = demand_data)
# Store coefficients
coeff_iv2 <- coef(iv2)
# Predict and elasticity
demand_data$q_demand_fitted_iv2 <- predict(iv2)
demand_data$edemand_iv2 <- (1/coeff_iv2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
mean_elast_iv2 <- mean(demand_data$edemand_iv2, na.rm = TRUE)

iv3 <- ivreg(price_ym ~ qe_ym + ica_lapse + importGDP_ym + teaPrices_ym |
               ica_lapse + farm_prices_ym + gdd_ym +
                     importGDP_ym + teaPrices_ym,
                   data = demand_data)
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
          title = "Estimaciones de la Demanda Mundial de Café",
          column.labels = c("OLS", "OLS", "OLS", "IV", "IV", "IV"),
          column.separate = c(3, 3),
          covariate.labels = c("Exportaciones ($Q_t$)", "PIB Importadores ($X_t$)", "Precio Té ($Z_t$)"),
          se = list(sqrt(diag(cov_ols1)), sqrt(diag(cov_ols2)), sqrt(diag(cov_ols3)),
                    sqrt(diag(cov_iv1)), sqrt(diag(cov_iv2)), sqrt(diag(cov_iv3))),
          notes = c("Errores estándar robustos entre paréntesis (HAC + cluster por año)."),
          type = "latex")

# ------------------------------------------------------------------------------
# [2] Implicit marginal costs
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# [3] Marginal cost estimation
# ------------------------------------------------------------------------------

data_ymc_0 <- data_ymc_0 %>%
  filter(!is.na(mr_ymc3),
         !is.na(farm_prices_ymc),
         !is.na(fert_ym),
         !is.na(gdd_ymc),
         !is.na(importGDP_ym),
         !is.na(country),
         !is.na(year))

data_ymc <- data_ymc_0 %>%
  group_by(country) %>%
  mutate(
    umbral_gdd = quantile(gdd_ymc, 0.1, na.rm = TRUE),
    umbral_hdd = quantile(hdd_ymc, 0.9, na.rm = TRUE),
    umbral_fdd = quantile(fdd_ymc, 0.9, na.rm = TRUE),
    
    # Identificar eventos que superan el umbral
    hdd_event = if_else(hdd_ymc > umbral_hdd, 1L, 0L),
    fdd_event = if_else(fdd_ymc > umbral_fdd, 1L, 0L),
    gdd_event = if_else(gdd_ymc < umbral_gdd, 1L, 0L),
    
    # Usar slide_index_dbl para obtener valores numéricos directamente
    hdd_ymc_id = slide_index_dbl(hdd_event, date, ~any(.x == 1), .after = 23, .complete = TRUE),
    fdd_ymc_id = slide_index_dbl(fdd_event, date, ~any(.x == 1), .after = 23, .complete = TRUE),
    gdd_ymc_id = slide_index_dbl(gdd_event, date, ~any(.x == 1), .after = 23, .complete = TRUE)
  ) %>%
  ungroup()

data_ymc <- data_ymc %>%
  filter(!is.na(hdd_ymc_id),
         !is.na(fdd_ymc_id),
         !is.na(gdd_ymc_id))

data_ymc <- pdata.frame(
  data_ymc,
  index = c("country", "date")
)

model_mc_1 <- feols(
  mr_ymc3 ~ ica_lapse + farm_prices_ymc + fert_ym +
    disease1 + frost1 +
    hdd_ym + fdd_ym | year + country,
    #hdd_ymc_id | year + country,
  data = data_ymc,
  cluster = ~country
)

summary(model_mc_1)

# Store coefficients
coeff_mc_1 <- coef(model_mc_1)
# # Predict
 data_ymc$mc_ymc_hat <- predict(model_mc_1)
# 
# # [3.1.] Counterfactuals marginal costs
# 
# # Counterfactual data
# data_ymc_cf1 <- data_ymc %>%
#   mutate(
#     # Case 1: HDD reduction
#     hdd_ymc_id = 0
#   )
# data_ymc_cf2 <- data_ymc %>%
#   mutate(
#     # Case 2: HDD + FDD reduction
#     hdd_ymc_id = 0,
#     fdd_ymc_id = 0
#   )
# 
# data_ymc$mc_ymc_cf1 <- predict(model_mc, newdata = data_ymc_cf1, fixef = FALSE)
# data_ymc$mc_ymc_cf2 <- predict(model_mc, newdata = data_ymc_cf2, fixef = FALSE)

# ------------------------------------------------------------------------------
# [4] Counterfactual Analysis (Stackelberg Model)
# ------------------------------------------------------------------------------
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
follower_reaction <- function(data_market, Q_leader, coef_demand) {
  followers <- data_market %>% filter(dummy_follower == 1)
  b <- -coef_demand["qe_ym"]  # Slope of demand curve
  
  # Market price given leader's quantity
  price <- price_from_demand(Q_leader, coef_demand, 
                             unique(data_market$teaPrices_ym), 
                             unique(data_market$importGDP_ym))
  
  # Number of followers
  n_followers <- nrow(followers)
  
  # Total quantity for followers (Cournot equilibrium)
  sum_mc <- sum(followers$mc_ymc_hat)
  Q_followers_total <- (n_followers * price - sum_mc) / ((n_followers + 1) * b)
  
  # Individual follower quantities
  Q_followers <- pmax(0, (price - b * Q_followers_total - followers$mc_ymc_hat) / b)
  
  return(data.frame(id = followers$id, 
                    q_follower = Q_followers, 
                    mc_hat = followers$mc_ymc_hat))
}

# 3. Leader optimization function
leader_optim <- function(Q_leader, data_market, coef_demand, total_mc = NULL) {
  leaders <- data_market %>% filter(dummy_leader == 1)
  n_followers <- sum(data_market$dummy_follower == 1)
  b <- -coef_demand["qe_ym"]
  
  # Si no se proporciona total_mc, calcular como promedio de MC de líderes
  if(is.null(total_mc)) {
    total_mc <- mean(leaders$mc_ymc_hat, na.rm = TRUE)
  }
  
  # Get follower reactions
  followers <- follower_reaction(data_market, Q_leader, coef_demand)
  Q_followers_total <- sum(followers$q_follower)
  
  # Total quantity and price
  Q_total <- Q_leader + Q_followers_total
  price <- price_from_demand(Q_total, coef_demand, 
                             unique(data_market$teaPrices_ym), 
                             unique(data_market$importGDP_ym))
  
  # Marginal revenue for leader
  marginal_revenue <- price + coef_demand["qe_ym"] * Q_leader * (n_followers / (n_followers + 1))
  
  # FOC: MR - MC = 0
  foc_value <- marginal_revenue - total_mc
  
  return(foc_value)
}

# 4. Market simulation function
simulate_market <- function(data_market, coef_demand) {
  # Verificar que hay líderes
  leaders <- data_market %>% filter(dummy_leader == 1)
  if (nrow(leaders) == 0) return(NULL)
  
  # Calcular cantidad inicial TOTAL de líderes
  Q_leader_init <- sum(leaders$qe_ymc, na.rm = TRUE)
  
  # Calcular costo marginal promedio ponderado
  total_mc <- sum(leaders$mc_ymc_hat * (leaders$qe_ymc / sum(leaders$qe_ymc, na.rm = TRUE)))
  
  # Resolver para la cantidad óptima
  solution <- nleqslv(
    Q_leader_init, 
    function(x) leader_optim(x, data_market, coef_demand, total_mc),
    control = list(ftol = 1e-10, maxit = 1000)
  )
  
  if (solution$termcd != 1) return(NULL)
  
  Q_leader_total <- solution$x
  followers <- follower_reaction(data_market, Q_leader_total, coef_demand)
  Q_followers_total <- sum(followers$q_follower)
  Q_total <- Q_leader_total + Q_followers_total
  
  # Precio de mercado (único para todos)
  price <- price_from_demand(Q_total, coef_demand, 
                             unique(data_market$teaPrices_ym), 
                             unique(data_market$importGDP_ym))
  
  # Beneficio TOTAL de líderes (suma individual)
  profit_leaders <- sum(Q_leader_total * (price - leaders$mc_ymc_hat) * 
                          (leaders$qe_ymc / sum(leaders$qe_ymc, na.rm = TRUE)))
  
  profit_followers <- sum(followers$q_follower * (price - followers$mc_hat))
  
  # Consumer surplus
  price_intercept <- price_from_demand(0, coef_demand, 
                                       unique(data_market$teaPrices_ym), 
                                       unique(data_market$importGDP_ym))
  consumer_surplus <- 0.5 * (price_intercept - price) * Q_total
  
  # Results
  results <- tibble(
    date = unique(data_market$date),
    year = unique(data_market$year),
    month_num = unique(data_market$month_num),
    Q_leader_total = Q_leader_total,
    Q_followers_total = Q_followers_total,
    Q_total = Q_total,
    price = price,
    profit_leaders = profit_leaders,
    profit_followers = profit_followers,
    consumer_surplus = consumer_surplus,
    total_surplus = profit_leaders + profit_followers + consumer_surplus,
    solver_converged = solution$termcd == 1,
    solver_iterations = solution$iter
  )
  
  return(results)
}

# ------------------------------------------------------------------------------
# [5] Run Counterfactual Analysis
# ------------------------------------------------------------------------------

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
baseline_results <- map_dfr(market_list, ~simulate_market(.x, coeff_iv3))

# Run counterfactual scenarios
# Scenario 1
cf1_results <- map_dfr(market_list, function(market) {
  market$mc_ymc_hat <- predict(model_mc_1, 
                               newdata = market %>% mutate(
                                 disease1 = 0,
                                 frost1 = 0,
                                 hdd_ym = 0,
                                 fdd_ym = 0
                                 ),
                               fixef = FALSE)
  simulate_market(market, coeff_iv3)
}) %>%
  mutate(scenario = "First reduction")

# Scenario 2
cf2_results <- map_dfr(market_list, function(market) {
  market$mc_ymc_hat <- predict(model_mc_2, 
                               newdata = market %>% mutate(
                                 #disease1 = 0,
                                 #frost1 = 0,
                                 hdd_followers_id = 0,
                                 fdd_followers_id = 0
                                 ),
                               fixef = FALSE)
  simulate_market(market, coeff_iv3)
}) %>%
  mutate(scenario = "Followers reduction")

# Scenario 3
cf3_results <- map_dfr(market_list, function(market) {
  market$mc_ymc_hat <- predict(model_mc_3, 
                               newdata = market %>% mutate(
                                 #disease1 = 0,
                                 #frost1 = 0,
                                 hdd_id = 0,
                                 fdd_id = 0
                               ),
                               fixef = FALSE)
  simulate_market(market, coeff_iv3)
}) %>%
  mutate(scenario = "Total reduction")

# Combine all results
all_results <- bind_rows(
  baseline_results %>% mutate(scenario = "Baseline"),
  cf1_results
) 

all_results <- all_results %>%
  arrange(date, scenario)

# ------------------------------------------------------------------------------
# [6] Analyze and Visualize Results
# ------------------------------------------------------------------------------

# Summary statistics by scenario
results_summary <- all_results %>%
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

ggplot(all_results, aes(x = date, y = price, color = scenario, group = scenario)) +
  geom_line() +
  theme_classic()

# Plot comparison
ggplot(all_results, aes(x = scenario, y = profit_leaders, fill = scenario)) +
  geom_boxplot() +
  theme_minimal()

# ------------------------------------------------------------------------------
# [7] Export Results
# ------------------------------------------------------------------------------

# Save results to CSV
write.csv(all_results, "counterfactual_results.csv", row.names = FALSE)
write.csv(results_summary, "counterfactual_summary.csv", row.names = FALSE)

# Save plots
ggsave("price_dates.png", width = 10, height = 6)
ggsave("quantity_comparison.png", width = 8, height = 6)

