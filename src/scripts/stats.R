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
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Colours
green  <- "#13F59A"
blue <- "#136DF5"
red <- "#F5136D"
beige <- "#F59B13"

# Data
data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/data.rds")
results_ym <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/results_ym.rds")
results_ymc <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/results_ymc.rds")
summary_ym <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/summary_ym.rds")
summary_ymc <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/summary_ymc.rds")

#-------------------------------------------------------------------------------
# [1] Fit de precios
#-------------------------------------------------------------------------------
aux1 <- data %>%
  filter(dummy_ym == 1) %>%
  select(date, price_ym) %>%
  mutate(date = as.Date(date)) %>%
  left_join(results_ym %>% 
              filter(scenario == "Baseline") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  distinct(date, price, price_ym) %>%
  rename(price_ym_estimated = price) %>%
  drop_na()

ggplot(aux1) +
  geom_line(aes(x = date, y = price_ym, color = "Precio Observado"), 
            linewidth = 0.8, alpha = 0.8) +
  geom_line(aes(x = date, y = price_ym_estimated, color = "Precio Estimado"), 
            linewidth = 0.8, alpha = 0.8, linetype = "dashed") +
  scale_color_manual(
    name = NULL,
    values = c("Precio Observado" = "red3", "Precio Estimado" = "blue3")
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "¢")
  ) +
  labs(
    title = "Comparación: Precio Observado vs. Precio Estimado",
    x = NULL,
    y = "US cents/lb"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-------------------------------------------------------------------------------
# [2] Fit de cantidades
#-------------------------------------------------------------------------------
aux2 <- data %>%
  filter(dummy_y == 1) %>%
  select(date, qe_y) %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  left_join(results_ym %>% 
              filter(scenario == "Baseline") %>%
              mutate(date = as.Date(date),
                     year = year(date)) %>%
              group_by(year) %>%
              mutate(qe_y_estimated = sum(Q_total, na.rm = TRUE)) %>%
              ungroup() %>%
              select(year, qe_y_estimated), by = c("year")) %>%
  distinct(year, qe_y, qe_y_estimated) %>%
  drop_na()

ggplot(aux2) +
  geom_line(aes(x = year, y = qe_y, color = "X"), 
            linewidth = 0.8, alpha = 0.8) +
  geom_line(aes(x = year, y = qe_y_estimated, color = "X est"), 
            linewidth = 0.8, alpha = 0.8, linetype = "dashed") +
  scale_color_manual(
    name = NULL,
    values = c("X" = "red3", "X est" = "blue3")
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mil. 60 kg bags"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-------------------------------------------------------------------------------
# [3] Precios mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------

results_ym <- results_ym %>%
  mutate(
    scenario = if_else(
      scenario == "Baseline", "Base", "Contrafactual"
    ),
    date = as.Date(date)
  )

ggplot(results_ym, aes(x = date, y = price, color = scenario, group = scenario)) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.8) +
  scale_color_manual(
    values = c("Base" = blue, "Contrafactual" = red) 
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = c(0.02, 0.02)
  ) +
  labs(
    title = "Precio Mensual (I-CIP)",
    x = NULL,
    y = "US cents/lb",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-------------------------------------------------------------------------------
# [4] Costos marginales mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------
aux1 <- results_ymc %>%
  mutate(
    scenario = if_else(
      scenario == "Baseline", "Base", "Contrafactual"
    ),
    type = if_else(
      type == "leader", "Líderes", "Seguidores"
    ),
    date = as.Date(date)
  ) %>%
  group_by(date, type, scenario) %>%
  summarise(
    mc_ym = mean(mc_ymc_hat, na.rm = TRUE),
    mc_se_ym = sd(mc_ymc_hat, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    group_scenario = paste(type, scenario, sep = "-")
  ) %>%
  ungroup()

ggplot(aux1, aes(x = date, y = mc_ym, color = scenario,
                 linetype = scenario, group = scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_ribbon(aes(ymin = mc_ym - 1.96 * mc_se_ym,
                  ymax = mc_ym + 1.96 * mc_se_ym,
                  fill = scenario),
              alpha = 0.2, color = NA) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c("Base" = "#1f77b4", "Contrafactual" = "#ff7f0e")
  ) +
  scale_fill_manual(
    values = c("Base" = "#1f77b4", "Contrafactual" = "#ff7f0e"),
    guide = "none"  # Ocultar leyenda de fill para evitar duplicados
  ) +
  scale_linetype_manual(
    values = c("Base" = "solid", "Contrafactual" = "dashed")
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = c(0.02, 0.02)
  ) +
  labs(
    title = "Costo Marginal Promedio Mensual",
    x = NULL,
    y = "US cents/lb",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.key.width = unit(2, "cm")
  )

#-------------------------------------------------------------------------------
# [5] Mapamundi
#-------------------------------------------------------------------------------
aux3 <- summary_ymc %>%
  mutate(
    d_profit = profit_CF1 - profit_Baseline,
    d_quantity = quantity_CF1 - quantity_Baseline,
    d_mc = mc_Baseline - mc_CF1,
    group_profit = case_when(
      d_profit <= quantile(d_profit, 0.25, na.rm = TRUE) ~ "≤ P25",
      d_profit > quantile(d_profit, 0.25, na.rm = TRUE) & d_profit <= quantile(d_profit, 0.75, na.rm = TRUE) ~ "P25-P75",
      d_profit > quantile(d_profit, 0.75, na.rm = TRUE) ~ "> P75",
      TRUE ~ NA_character_
    ),
    group_quantity = case_when(
      d_quantity <= quantile(d_quantity, 0.25, na.rm = TRUE) ~ "≤ P25",
      d_quantity > quantile(d_quantity, 0.25, na.rm = TRUE) & d_quantity <= quantile(d_quantity, 0.75, na.rm = TRUE) ~ "P25-P75",
      d_quantity > quantile(d_quantity, 0.75, na.rm = TRUE) ~ "> P75",
      TRUE ~ NA_character_
    ),
    group_mc = case_when(
      d_mc <= quantile(d_mc, 0.25, na.rm = TRUE) ~ "≤ P25",
      d_mc > quantile(d_mc, 0.25, na.rm = TRUE) & d_mc <= quantile(d_mc, 0.75, na.rm = TRUE) ~ "P25-P75",
      d_mc > quantile(d_mc, 0.75, na.rm = TRUE) ~ "> P75",
      TRUE ~ NA_character_
    ),
    name = case_when(
      country == "Central African Republic" ~ "Central African Rep.",
      country == "Dominican Republic" ~ "Dominican Rep.",
      country == "Viet Nam" ~ "Vietnam",
      TRUE ~ country
    )
  ) %>%
  select(name, group_profit, group_quantity, group_mc)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  left_join(aux3, by = c("name"))
  
ggplot(world) +
  geom_sf(aes(fill = group_profit), color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Cambio en Profit",
    values = c(
      "≤ P25" = "green3",
      "P25-P75" = "yellow2",
      "> P75" = "red3"
      ),
    na.value = "gray90"
  ) 

ggplot(world) +
  geom_sf(aes(fill = group_quantity), color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Cambio en Q",
    values = c(
      "≤ P25" = "green3",
      "P25-P75" = "yellow2",
      "> P75" = "red3"
    ),
    na.value = "gray90"
  ) 

ggplot(world) +
  geom_sf(aes(fill = group_mc), color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Cambio en mc",
    values = c(
      "≤ P25" = "green3",
      "P25-P75" = "yellow2",
      "> P75" = "red3"
    ),
    na.value = "gray90"
  ) 



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
  filter(mc_ymc_hat_CF1 >= quantile(mc_ymc_hat_CF1, 0.025),
         mc_ymc_hat_CF1 <= quantile(mc_ymc_hat_CF1, 0.975),
         profit_CF1 >= quantile(profit_CF1, 0.025),
         profit_CF1 <= quantile(profit_CF1, 0.975))

data_ymc_cf <- data_ymc_cf %>%
  mutate(
    d_profit = profit_CF1 - profit_Baseline,
    d2_profit = (profit_CF1 / profit_Baseline - 1) * 100,
    d_mc = mc_ymc_hat_Baseline - mc_ymc_hat_CF1,
    d_mc2 = d_mc^(2),
    dlog_profit = log(profit_CF1 / profit_Baseline),
    dlog_mc = log(mc_ymc_hat_Baseline / mc_ymc_hat_CF1),
    year = year(date)
  )

# Cuanto mayor es la disminución en cm, mayor es la ganancia en profit, pero el efecto es cóncavo
m1 <- feols(
  d_profit ~ d_mc + d_mc2 | country + year, 
  data = data_ymc_cf
)

summary(m1)

# Cuanto mayor es la disminución en cm, mayor es la ganancia en profit. El efecto es mayor para líderes
m2 <- feols(
  d_profit ~ d_mc * dummy_leader | country + year, 
  data = data_ymc_cf
)

summary(m2)


m2 <- feols(
  dlog_profit ~ dlog_mc | country + year, 
  data = data_ymc_cf
  )

summary(m2)

m3 <- feols(
  dlog_profit ~ dlog_mc * dummy_leader | country + year, 
  data = data_ymc_cf
)

summary(m3)

m3 <- feols(
  dlog_profit ~ log(mc_ymc_hat_Baseline) | country + year, 
  data = data_ymc_cf
)

summary(m3)

m3 <- lm(
  dlog_profit ~ mc_ymc_hat_Baseline, 
  data = data_ymc_cf
)

summary(m3)

# Diferencias entre grupos

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



