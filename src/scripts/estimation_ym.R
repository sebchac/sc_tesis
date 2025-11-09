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


saveRDS(data_ymc, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data_cf_ym.rds")