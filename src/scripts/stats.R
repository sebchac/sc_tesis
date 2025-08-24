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

# Data
data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/data.rds")
results_ym <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/results_ym.rds")
results_ymc <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/results_ymc.rds")
summary_ym <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/summary_ym.rds")
summary_ymc <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/summary_ymc.rds")

# Resumen por país desde data
data_yc <- data %>%
  filter(dummy_yc == 1, export_dummy == 1) %>%
  group_by(country) %>%
  summarise(
    qe_c = sum(qe_yc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()
  

# Efecto de gdd y fdd en farm prices
data_y <- data %>%
  filter(dummy_y == 1, year >= 1965, year <= 2019)

ggplot(data_y, aes(x = gdd_y_mean, y = farm_prices_y_mean)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = fdd_y_mean, y = farm_prices_y_mean)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Efecto de gdd y fdd en precios
ggplot(data_y, aes(x = gdd_y_mean, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = fdd_y_mean, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Efecto de gdd y fdd en cantidades
ggplot(data_y, aes(x = gdd_y_mean, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = fdd_y_mean, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Boxplot GDP
data_yc <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  mutate(
    type = case_when(
      export_dummy == 1 ~ "Exporting",
      export_dummy == 0 ~ "Importing"
    ))

ggplot(data_yc, aes(factor(year), y = rgdpna_yc, fill = type)) +
  geom_boxplot(position = position_dodge(0.8),
               width = 0.7)

# Cambios en profits y costos marginales
data_ymc_cf <- results_ymc %>%
  select(date, country, profit, quantity, mc_ymc_hat, scenario, dummy_leader) %>%
  pivot_longer(
    cols = c(profit, mc_ymc_hat, quantity),
    names_to = "variable",
    values_to = "value"
  ) %>%
  unite("variable_scenario", variable, scenario, sep = "_") %>%
  pivot_wider(
    names_from = variable_scenario,
    values_from = value
  )

data_ymc_cf <- data_ymc_cf %>%
  group_by(country, date) %>%
  mutate(
    delta_quantity = quantity_CF1 - quantity_Baseline,
    delta_profit = profit_CF1 - profit_Baseline,
    delta_mc = mc_ymc_hat_CF1 - mc_ymc_hat_Baseline,
    d_log_profit = log(profit_CF1) - log(profit_Baseline),
    d_log_mc = log(mc_ymc_hat_CF1) - log(mc_ymc_hat_Baseline),
    d_log_quantity = log(quantity_CF1) - log(quantity_Baseline)
  ) %>%
  ungroup() %>%
  group_by(date, dummy_leader) %>%
  mutate(
    qe_leaders_cf1_ym = sum()
  )

resumen_grupos <- data_ymc_cf %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(
    qe_cf1_leaders_y  = sum(quantity_CF1[dummy_leader == 1], na.rm = TRUE),
    qe_base_leaders_y = sum(quantity_Baseline[dummy_leader == 1], na.rm = TRUE),
    qe_cf1_followers_y  = sum(quantity_CF1[dummy_leader == 0], na.rm = TRUE),
    qe_base_followers_y = sum(quantity_Baseline[dummy_leader == 0], na.rm = TRUE),
    
    profit_cf1_leaders_y  = sum(profit_CF1[dummy_leader == 1], na.rm = TRUE),
    profit_base_leaders_y = sum(profit_Baseline[dummy_leader == 1], na.rm = TRUE),
    profit_cf1_followers_y  = sum(profit_CF1[dummy_leader == 0], na.rm = TRUE),
    profit_base_followers_y = sum(profit_Baseline[dummy_leader == 0], na.rm = TRUE),
    
    mc_cf1_leaders_y  = mean(mc_ymc_hat_CF1[dummy_leader == 1], na.rm = TRUE),
    mc_base_leaders_y = mean(mc_ymc_hat_Baseline[dummy_leader == 1], na.rm = TRUE),
    mc_cf1_followers_y  = mean(mc_ymc_hat_CF1[dummy_leader == 0], na.rm = TRUE),
    mc_base_followers_y = mean(mc_ymc_hat_Baseline[dummy_leader == 0], na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    # Brechas absolutas y relativas
    d_cf1_groups_y = qe_cf1_leaders_y - qe_cf1_followers_y,
    d_base_groups_y = qe_base_leaders_y - qe_base_followers_y,
    d2_cf1_groups_y = qe_cf1_leaders_y / qe_cf1_followers_y,
    d2_base_groups_y = qe_base_leaders_y / qe_base_followers_y,
    # Perjuicio por grupo
    d_leaders_y = qe_cf1_leaders_y - qe_base_leaders_y,
    d_followers_y = qe_cf1_followers_y - qe_base_followers_y,
    d2_leaders_y = (qe_cf1_leaders_y / qe_base_leaders_y - 1) * 100,
    d2_followers_y = (qe_cf1_followers_y / qe_base_followers_y - 1) * 100,
    # Comparación de cambios por grupo
    diff_groups_y = d2_leaders_y - d2_followers_y,
      
    cc_qe_groups_y = (qe_cf1_leaders_y - qe_cf1_followers_y) / (qe_base_leaders_y - qe_base_followers_y),
    cc_profit_groups_y = (profit_cf1_leaders_y - profit_cf1_followers_y) / (profit_base_leaders_y - profit_base_followers_y),
    cc_mc_groups_y = (mc_cf1_leaders_y - mc_cf1_followers_y) / (mc_base_leaders_y - mc_base_followers_y)
  )

mean(resumen_grupos$cc_qe_groups_y)  
mean(resumen_grupos$cc_profit_groups_y)  
mean(resumen_grupos$cc_mc_groups_y)

data_y <- data_y %>%
  left_join(resumen_grupos, by = c("year"))

ggplot(resumen_grupos, aes(x = year, y = cc_qe_groups_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = gdd_y_mean, y = cc_qe_groups_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

m2 <- lm(
  delta_profit ~ delta_mc, data = data_ymc_cf
)

summary(m2)

m3 <- lm(
  d_log_profit ~ d_log_mc, data = data_ymc_cf
)

summary(m3)



