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
library(moments)
library(kableExtra)
library(splines)
library(lme4)

ruta_proyecto <- "/Users/sebastianchacon/Desktop/sc_tesis/"
ruta_paper <- paste0(ruta_proyecto, "paper/")
ruta_tables <- paste0(ruta_paper, "tables/")
ruta_figures <- paste0(ruta_paper, "figures/")

# Data
  data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data.rds")

# ------------------------------------------------------------------------------
# [1] Demand estimation
# ------------------------------------------------------------------------------

# [1.1.] Data for demand estimation
  # Filters
  demand_data <- data %>%
    filter(dummy_ym == 1) %>%
    arrange(year, month_num) %>%
    select(date, year, month_num, price_ym, qe_ym, importGDP_ym, teaPrices_ym, farm_prices_ym,
      tas_yme, tas_yme_mean, tas_ymi_mean, tmin_yme, tmin_yme_mean, tmin_ymi_mean, tmax_yme, 
      tmax_yme_mean, tmax_ymi_mean, oni_value, fert_ym,
      pr_yme, pr_yme_mean, pr_ymi_mean,
      gdd_ym, hdd_ym, fdd_ym, gdd_ym_mean, hdd_ym_mean, fdd_ym_mean,
      gdd_brazil_ym, gdd_colombia_ym, gdd_vietnam_ym, gdd_leaders_ym, gdd_followers_ym,
      hdd_brazil_ym, hdd_colombia_ym, hdd_vietnam_ym, hdd_leaders_ym, hdd_followers_ym,
      fdd_brazil_ym, fdd_colombia_ym, fdd_vietnam_ym, fdd_leaders_ym, fdd_followers_ym,
      optExp_ym_mean, optExp_ym,
      heatExp_ym_mean, heatExp_ym,
      frostExp_ym_mean, frostExp_ym,
      disease1, disease2, frost1, frost2, droughts, ica_lapse,
      heat_lag1_ym_mean, frost_lag1_ym_mean, opt_lag1_ym_mean,
      heat_diff1_ym_mean, frost_diff1_ym_mean, opt_diff1_ym_mean,
      heat_ratio_ym_mean, frost_ratio_ym_mean, stress_ratio_ym_mean,
      heat_ratio_ym, frost_ratio_ym, stress_ratio_ym,
      heat_ratio_ym_agg, frost_ratio_ym_agg, stress_ratio_ym_agg) %>%
    mutate(trend = floor(year - min(year)),
           trend2 = trend^2,
           trend_ym = row_number(),
           trend5y = floor((year - min(year)) / 5),
           trend3y = floor((year - min(year)) / 3),
           lag3_tmax_yme = dplyr::lag(tmax_yme, 3),
           lag3_tmin_yme = dplyr::lag(tmin_yme, 3)
           ) %>%
    drop_na()
  
  summary(demand_data$teaPrices_ym)
  sd(demand_data$teaPrices_ym)
  
  aux <- demand_data %>%
    distinct(year, importGDP_ym)
  
  summary(aux$importGDP_ym)
  sd(aux$importGDP_ym)
  
  cor(demand_data %>% select(qe_ym, tas_yme, pr_yme, gdd_ym, hdd_ym, fdd_ym))
  cor(demand_data %>% select(qe_ym, tas_yme, pr_yme, optExp_ym_mean, heatExp_ym_mean, frostExp_ym_mean))
  
# [1.2.] Demand estimation models
# [1.2.1.] OLS
  ols1 <- lm(price_ym ~ qe_ym , 
          data = demand_data)
  summary(ols1)
  # Store coefficients
    coeff_ols1 <- coef(ols1)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols1 <- predict(ols1)
    demand_data$edemand_ols1 <- (1/coeff_ols1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols1 <- mean(demand_data$edemand_ols1, na.rm = TRUE)

  ols2 <- lm(price_ym ~ qe_ym  + importGDP_ym,
          data = demand_data)
  summary(ols2)
  # Store coefficients
    coeff_ols2 <- coef(ols2)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols2 <- predict(ols2)
    demand_data$edemand_ols2 <- (1/coeff_ols2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols2 <- mean(demand_data$edemand_ols2, na.rm = TRUE)

  ols3 <- lm(price_ym ~ qe_ym  + importGDP_ym + teaPrices_ym,
          data = demand_data)
  summary(ols3)
  # Store coefficients
    coeff_ols3 <- coef(ols3)
  # Predict and elasticity
    demand_data$q_demand_fitted_ols3 <- predict(ols3)
    demand_data$edemand_ols3 <- (1/coeff_ols3["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_ols3 <- mean(demand_data$edemand_ols3, na.rm = TRUE)

# [1.2.2.] IV
  iv1 <- ivreg(price_ym ~ qe_ym |
                 lag3_tmax_yme + lag3_tmin_yme,
          data = demand_data)
  # Store coefficients
    coeff_iv1 <- coef(iv1)
  # Predict and elasticity
    demand_data$q_demand_fitted_iv1 <- predict(iv1)
    demand_data$edemand_iv1 <- (1/coeff_iv1["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_iv1 <- mean(demand_data$edemand_iv1, na.rm = TRUE)
    summary(iv1)

  iv2 <- ivreg(price_ym ~ qe_ym  + importGDP_ym |
                 lag3_tmax_yme + lag3_tmin_yme +
          importGDP_ym,
          data = demand_data)
  summary(iv2)
  # Store coefficients
    coeff_iv2 <- coef(iv2)
  # Predict and elasticity
    demand_data$q_demand_fitted_iv2 <- predict(iv2)
    demand_data$edemand_iv2 <- (1/coeff_iv2["qe_ym"])*(demand_data$price_ym/demand_data$qe_ym)
    mean_elast_iv2 <- mean(demand_data$edemand_iv2, na.rm = TRUE)

  iv3 <- ivreg(price_ym ~ qe_ym  + importGDP_ym + teaPrices_ym |
          #tas_yme + gdd_ym +
            lag3_tmax_yme + lag3_tmin_yme +
          importGDP_ym + teaPrices_ym,
          data = demand_data)
  summary(iv3)
  
  # Store coefficients
    coeff_iv3 <- coef(iv3)
    beta <- coeff_iv3["qe_ym"] / 12
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
  
  saveRDS(demand_data, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/demand_ym.rds")
  
  
# ------------------------------------------------------------------------------
# [2] Implicit marginal costs
# ------------------------------------------------------------------------------

# [2.1.] Data for marginal costs
  aux <- demand_data %>%
    select(year, month_num, q_demand_fitted_iv3, edemand_iv3, trend, trend5y, trend3y) %>%
    mutate(year = as.double(year))

  data_ymc_0 <- left_join(data %>% select(-trend) %>% mutate(year = as.double(year)), aux, by = c("year", "month_num"))

  data_yc <- data_ymc_0 %>%
    filter(dummy_yc == 1, net_export_dummy == 1, year >= 1990, year <= 2020) %>%
    mutate(
      # Cournot
      mr_yc = price_y + beta * qe_yc,
    # Stackelberg
      mr_yc3 = (price_y + beta * qe_yc * (1 / (1 + n_follower_y))) * dummy_leader + (price_y + beta * qe_yc) * dummy_follower,
    ) %>%
    group_by(country, year) %>%
    mutate(mr_yc = ifelse(mr_yc < 0, 
                          quantile(mr_yc[mr_yc >= 0], 0.25, na.rm = TRUE), 
                          mr_yc),
           mr_yc3 = ifelse(mr_yc3 < 0, 
                           quantile(mr_yc3[mr_yc3 >= 0], 0.25, na.rm = TRUE), 
                           mr_yc3)) %>%
    ungroup() %>%
    mutate(
      ln_mr_yc = log(mr_yc + 1), 
      ln_mr_yc3 = log(mr_yc3 + 1)
    )
  
  aux1 <- data_yc %>%
    group_by(country) %>%
    summarise(
      n_mr = sum(!is.na(mr_yc3)),
      .groups = 'drop'
    ) %>%
    ungroup()
  
  data_yc <- data_yc %>%
    left_join(aux1, by = c("country")) %>%
    filter(n_mr >= 31)
  
# ------------------------------------------------------------------------------
# [3] Marginal cost estimation
# ------------------------------------------------------------------------------

  data_yc <- data_yc %>%
    filter(!is.na(mr_yc3),
           !is.na(mr_yc),
      !is.na(fert_y),
      !is.na(importGDP_y),
      !is.na(country),
      !is.na(year))
  
  data_yc <- pdata.frame(
    data_yc,
    index = c("country", "year")
  )
  
# [3.1.] Only one stage and Stackelberg
  mc_fe <- feols(
    mr_yc3 ~
      trend5y +
      fert_y +
      heatExp_yc +
      heat_lag1_yc
    | country,
    data = data_yc,
    cluster = ~country
  )
  
  summary(mc_fe)
  
  # Only one stage and Stackelberg for leaders
  data_leader <- data_yc %>% filter(dummy_leader == 1)
  
  mc_fe_l <- feols(
    mr_yc3 ~
      #farm_prices_yc +
      trend5y + 
      fert_y +
      heatExp_yc +
      heat_lag1_yc | country,
    data = data_leader,
    cluster = ~country
  )
  
  summary(mc_fe_l)
  
  # Only one stage and Stackelberg for followers
  data_follower <- data_yc %>% filter(dummy_leader == 0)
  
  mc_fe_f <- feols(
    mr_yc3 ~
      #farm_prices_yc +
      trend5y + 
      fert_y +
      heatExp_yc +
      heat_lag1_yc| country,
    data = data_follower,
    cluster = ~country
  )
  
  summary(mc_fe_f)
  
  # [3.2.] Only one stage and Cournot
  mc_fe_cournot <- feols(
    mr_yc ~
      trend5y +
      fert_y +
      heatExp_yc +
      heat_lag1_yc
    | country,
    data = data_yc,
    cluster = ~country
  )
  
  summary(mc_fe_cournot)
  
# Contrafactuales
  # ============================================================================
  # PASO 1: ESTIMAR COMPONENTES EN PERÍODO BASE (1965-1989)
  # ============================================================================
  
  # Preparar datos
  data_cf <- data %>%
    distinct(country, year, heatExp_yc) %>%
    filter(year >= 1965, year <= 1989) %>%
    drop_na()
  
  # 1. Estimar tendencias país-específicas (1965-1989)
  
  coefficients <- data_cf %>%
    group_by(country) %>%
    nest() %>%
    mutate(
      model = map(data, ~lm(heatExp_yc ~ year, data = .)),
      tidied = map(model, tidy)
    ) %>%
    unnest(tidied) %>%
    select(country, term, estimate) %>%
    tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
    rename(intercept = `(Intercept)`, slope = year)
  
  # 2. Construir contrafactual para 1990-2019
  years_counterfactual <- 1990:2019
  
  data_counterfactual <- expand.grid(
    country = unique(data_cf$country),
    year = years_counterfactual,
    stringsAsFactors = FALSE
  ) %>%
    left_join(coefficients, by = "country") %>%
    mutate(heatExp_yc_cf = intercept + slope * year) %>%
    select(country, year, heatExp_yc_cf) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      heat_lag1_yc_cf = dplyr::lag(heatExp_yc_cf, 1),
      year = as.factor(year)
    ) %>%
    ungroup() %>%
    drop_na()
  
  data_yc <- data_yc %>% left_join(data_counterfactual, by = c("year", "country"))
  
  # Store coefficients FE
    coeff_mc_s <- coef(mc_fe)
  # Predict FE
    data_yc$mc_yc_hat_s <- predict(mc_fe)

  # Predict CF1: Stackelberg with new heatExp
    data_yc$mc_yc_hat_s_cf1 <- predict(
      mc_fe, 
      newdata = data_yc %>% mutate(
        heatExp_yc = heatExp_yc_cf,
        heat_lag1_yc = heat_lag1_yc_cf
      ))
  # Predict CF2: Cournot with new heatExp
     data_yc$mc_yc_hat_c_cf2 <- predict(
       mc_fe_cournot, 
       newdata = data_yc %>% mutate(
         heatExp_yc = heatExp_yc_cf,
         heat_lag1_yc = heat_lag1_yc_cf
       ))
  # Predict CF3: Stackelberg with 25% heat reduction
     data_yc$mc_yc_hat_s_cf3 <- predict(
       mc_fe, 
       newdata = data_yc %>% mutate(
         heatExp_yc = 0.75 * heatExp_yc,
         heat_lag1_yc = 0.75 * heat_lag1_yc
       ))
     
  # Predict CF4: Cournot with 25% heat reduction
     data_yc$mc_yc_hat_c_cf4 <- predict(
       mc_fe_cournot, 
       newdata = data_yc %>% mutate(
         heatExp_yc = 0.75 * heatExp_yc,
         heat_lag1_yc = 0.75 * heat_lag1_yc
       ))
    
    # Elimina NAs generados por lag
    data_yc <- data_yc %>%
      #filter(!is.na(mc_yc_hat_s_cf1) & !is.na(mc_yc_hat_s_cf2) & !is.na(mc_yc_hat_s_cf3))
      filter(!is.na(mc_yc_hat_s_cf1))
    
    
  # # Store coefficients IV 
  #   coeff_mc_2 <- coef(mc_iv)
  # # Predict IV
  #   data_ymc$mc_ymc_hat_s <- predict(mc_iv)  


saveRDS(data_yc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data_cf_y.rds")

# ------------------------------------------------------------------------------
# [4] Counterfactual Analysis (Stackelberg Model)
# ------------------------------------------------------------------------------

  # Define key functions for counterfactual analysis
  # 1. Price from demand function (inverse demand)
  price_from_demand <- function(Q_total, coef_demand, tea_price, import_gdp) {
    price <- coef_demand["(Intercept)"] + 
      (coef_demand["qe_ym"] / 12) * Q_total + 
      coef_demand["importGDP_ym"] * import_gdp + 
      coef_demand["teaPrices_ym"] * tea_price
    return(price)
  }
  # 2. Follower reaction function (Cournot)
  follower_reaction <- function(data_market, Q_leaders_total, coef_demand) {
    followers <- data_market %>% filter(dummy_follower == 1)
    n_followers <- nrow(followers)
    b <- -beta  # Slope of demand curve (year)
    # Market price given leader's quantity
    price <- price_from_demand(Q_leaders_total, coef_demand, 
                               data_market$teaPrices_y[1], 
                               data_market$importGDP_y[1])
    # Reaction
    q_follower <- (price - followers$mc_yc_hat_s) / ((n_followers + 1) * b)
    Q_followers_total <- sum(q_follower)
    return(data.frame(
      id = followers$id, 
      q_follower = q_follower, 
      Q_followers_total = Q_followers_total, 
      mc_hat = followers$mc_yc_hat_s))
  }
  # 3. Leader optimization function
  leader_optim <- function(Q_leaders, data_market, coef_demand) {
    leaders <- data_market %>% filter(dummy_leader == 1)
    n_leaders <- nrow(leaders)
    n_followers <- sum(data_market$dummy_follower == 1)
    b <- -beta
    Q_leaders_total <- sum(Q_leaders)
    # Reacción de seguidores
    followers <- follower_reaction(data_market, Q_leaders_total, coef_demand)
    Q_followers_total <- sum(followers$q_follower)
    # Precio de mercado
    Q_total <- Q_leaders_total + Q_followers_total
    price <- price_from_demand(
      Q_total, coef_demand, 
      data_market$teaPrices_y[1], 
      data_market$importGDP_y[1])
    # Calcula FOC
    mr_leaders <- price - b * Q_leaders + b * Q_leaders * (n_followers / (n_followers + 1)) - leaders$mc_yc_hat_s
    return(data.frame(
      q_leader = Q_leaders,
      id = leaders$id,
      mc_hat = leaders$mc_yc_hat_s,
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
      initial_guess <- leaders$qe_yc  # Usar cantidades observadas como guess
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
    Q_leaders_00 <- leaders$qe_yc
    # Optimiza
    leaders_opt <- find_stackelberg(data_market, coef_demand, leaders$qe_yc)
    if (is.null(leaders_opt)) {
      warning("No se pudo encontrar equilibrio Stackelberg")
      return(NULL)}
    Q_leaders_total <- sum(leaders_opt$q_leader)
    followers_opt <- follower_reaction(data_market, Q_leaders_total, coef_demand)
    Q_followers_total <- sum(followers_opt$q_follower)
    Q_total <- Q_leaders_total + Q_followers_total
    price <- price_from_demand(
      Q_total, coef_demand, 
      unique(data_market$teaPrices_y), 
      unique(data_market$importGDP_y))
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
        profit = (quantity * (price - mc_yc_hat_s) * (60 * 2.20462)) / 100, # Ajuste por unidad de medida (MM USD)
        type = case_when(
          dummy_leader == 1 ~ "leader",
          dummy_follower == 1 ~ "follower", 
          TRUE ~ "other")) %>%
      select(date, country, id, dummy_leader, type, quantity, revenue, profit, mc_yc_hat_s, price)
    # Beneficio TOTAL de líderes (suma individual)
    profit_leaders <- (sum(leaders_opt$q_leader * (price - leaders_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
    profit_followers <- (sum(followers_opt$q_follower * (price - followers_opt$mc_hat)) * (60 * 2.20462)) / 100 # Ajuste por unidad de medida
    # Consumer surplus
    price_intercept <- price_from_demand(
      0, coef_demand, 
      unique(data_market$teaPrices_y), 
      unique(data_market$importGDP_y))
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

# ------------------------------------------------------------------------------
# [5] Run Counterfactual Analysis
# ------------------------------------------------------------------------------

  # Prepare data for counterfactuals
  data_yc <- data_yc %>%
    group_by(year) %>%
    filter(sum(dummy_leader) > 0) %>%  # Keep only markets with leaders
    ungroup()
  
  # Split data by market (time period)
  market_list <- data_yc %>%
    group_by(year) %>%
    group_split()
  
  # Baseline
  cf0_processed <- map(market_list, function(market) {
    market$mc_yc_hat_s <- predict(
      mc_fe, 
      newdata = market,
      fixef = FALSE)
    simulate_market(market, coeff_iv3)
  }) 

  # Scenario 1
  cf1_processed <- map(market_list, function(market) {
    market$mc_yc_hat_s <- market$mc_yc_hat_s_cf1 
    simulate_market(market, coeff_iv3)
  })
  
  # Scenario 2
  cf2_processed <- map(market_list, function(market) {
    market$mc_yc_hat_s <- market$mc_yc_hat_c_cf2
    simulate_market(market, coeff_iv3)
  })
  
  # Scenario 3
  cf3_processed <- map(market_list, function(market) {
    market$mc_yc_hat_s <- market$mc_yc_hat_s_cf3
    simulate_market(market, coeff_iv3)
  }) 
  
  # Scenario 4
  cf4_processed <- map(market_list, function(market) {
    market$mc_yc_hat_s <- market$mc_yc_hat_c_cf4
    simulate_market(market, coeff_iv3)
  }) 
  
  
  cf0_results_y <- map_dfr(cf0_processed, ~.x$market_summary, .id = "market_id") %>% 
    mutate(scenario = "CF0")
  
  cf0_results_yc <- map_dfr(cf0_processed, ~ .x$country_profits, .id = "market_id") %>% 
    mutate(scenario = "CF0")
  
  cf1_results_y <- map_dfr(cf1_processed, ~.x$market_summary, .id = "market_id") %>% 
    mutate(scenario = "CF1")
  
  cf1_results_yc <- map_dfr(cf1_processed, ~ .x$country_profits, .id = "market_id") %>% 
    mutate(scenario = "CF1")
  
  cf2_results_y <- map_dfr(cf2_processed, ~.x$market_summary, .id = "market_id") %>% 
    mutate(scenario = "CF2")
  
  cf2_results_yc <- map_dfr(cf2_processed, ~ .x$country_profits, .id = "market_id") %>% 
    mutate(scenario = "CF2")
  
  cf3_results_y <- map_dfr(cf3_processed, ~.x$market_summary, .id = "market_id") %>% 
    mutate(scenario = "CF3")
  
  cf3_results_yc <- map_dfr(cf3_processed, ~ .x$country_profits, .id = "market_id") %>% 
    mutate(scenario = "CF3")
  
  cf4_results_y <- map_dfr(cf4_processed, ~.x$market_summary, .id = "market_id") %>% 
    mutate(scenario = "CF4")
  
  cf4_results_yc <- map_dfr(cf4_processed, ~ .x$country_profits, .id = "market_id") %>% 
    mutate(scenario = "CF4")
  
  
  # Combine all results
  all_results_y <- bind_rows(
    cf0_results_y,
    cf1_results_y,
    cf2_results_y,
    cf3_results_y,
    cf4_results_y
  ) %>%
    arrange(date, scenario)
  
  all_results_yc <- bind_rows(
    cf0_results_yc,
    cf1_results_yc,
    cf2_results_yc,
    cf3_results_yc,
    cf4_results_yc
  ) %>%
    arrange(date, country, scenario)
  
  saveRDS(all_results_y, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_y.rds")
  saveRDS(all_results_yc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_yc.rds")

# ------------------------------------------------------------------------------
# [6] Analyze and Visualize Results
# ------------------------------------------------------------------------------

  # Summary statistics by scenario
  summary <- all_results_y %>%
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

  saveRDS(summary, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary.rds")
  
  # Guardar en ruta_figures
  #writeLines(tabla_tex, paste0(ruta_figures, "tabla.tex"))
  
  # Profit comparison by country
  summary_c <- all_results_yc %>%
    group_by(country, dummy_leader, scenario) %>%
    summarise(
      profit = sum(profit, na.rm = TRUE),
      quantity = sum(quantity, na.rm = TRUE),
      mc = mean(mc_yc_hat_s, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = c(country, dummy_leader),
      names_from = scenario,
      values_from = c(profit, quantity, mc)
    )
  
  saveRDS(summary_c, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary_c.rds")

