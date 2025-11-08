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
library(stringr)

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
# Exportaciones desde 1990 hasta 2019
#-------------------------------------------------------------------------------
aux1 <- data %>%
  filter(dummy_y == 1) %>%
  select(date, qe_y) %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  distinct(year, qe_y) %>%
  drop_na()

plot1 <- ggplot(aux1 %>%
                  filter(year >= 1990)) +
  geom_line(aes(x = year, y = qe_y), 
            linewidth = 0.7, alpha = 1) +
  labs(
    title = NULL,
    x = NULL,
    y = "Exp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/informe/plot1.png", 
       plot = plot1, 
       width = 7,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# Precio ICIP desde 1990 hasta 2019
#-------------------------------------------------------------------------------
aux2 <- data %>%
  filter(dummy_ym == 1) %>%
  select(date, price_ym) %>%
  mutate(date = as.Date(date)) %>%
  distinct(date, price_ym) %>%
  drop_na()

plot2 <- ggplot(aux2 %>%
                  filter(year(date) >= 1990)) +
  geom_line(aes(x = date, y = price_ym), 
            linewidth = 0.7, alpha = 1) +
  labs(
    title = NULL,
    x = NULL,
    y = "ICIP (¢/lb)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/informe/plot2.png", 
       plot = plot2, 
       width = 7,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# Boxplot GDP Exportadores e importadores
#-------------------------------------------------------------------------------

data_yc <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  mutate(
    type = case_when(
      export_dummy == 1 ~ "Exportadores",
      export_dummy == 0 ~ "Importadores"
    ))

plot3 <- ggplot(data_yc, aes(factor(year), y = rgdpna_yc, fill = type)) +
  geom_boxplot(
    position = position_dodge(0.8),
    width = 0.7,
    alpha = 0.8,           # Similar alpha para consistencia
    size = 0.5,            # Tamaño de línea similar
    color = "black"        # Borde negro para contraste en grises
  ) +
  labs(
    title = NULL,
    x = NULL,              # Coincide con tu primer gráfico
    y = "PIB real ajustado",       # Ajusta según tu variable
    fill = NULL            # Coincide con tu primer gráfico
  ) +
  theme_classic() +        # Mismo tema base
  theme(
    legend.position = "bottom",
    text = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  ) +
  # Escala de grises profesional para papers
  scale_fill_grey(
    start = 0.2,    # Gris oscuro
    end = 0.8,      # Gris claro  
    na.value = "red"
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/informe/plot3.png", 
       plot = plot3, 
       width = 18,
       height = 8,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# Evolución de importaciones
#-------------------------------------------------------------------------------
data_y <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  group_by(year) %>%
  summarise(
    qi_y = sum(qi_yc, na.rm = T),
    .groups = "drop"
  )
  
plot1 <- ggplot(data_y) +
  geom_line(aes(x = year, y = qi_y), 
            linewidth = 0.7, alpha = 1) +
  labs(
    title = NULL,
    x = NULL,
    y = "Imp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/informe/plot1.png", 
       plot = plot1, 
       width = 7,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Líderes y seguidores - Factual
#-------------------------------------------------------------------------------

# Cantidades

aux1 <- data %>%
  filter(dummy_yc == 1,
         export_dummy == 1
  ) %>%
  mutate(
    dummy_leader = as.factor(dummy_leader)
  ) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    qe_y = sum(qe_yc, na.rm = TRUE),
    .groups = "drop"
  )

plot1 <- ggplot(aux1, aes(x = year, y = qe_y, 
                          color = factor(dummy_leader,
                                         levels = c(0, 1),
                                         labels = c("Seguidores", "Líderes")), 
                          group = dummy_leader)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  labs(
    title = NULL,
    x = NULL,
    y = "Exp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot1.png", 
       plot = plot1, 
       width = 7,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

# Farm prices

aux2 <- data %>%
  filter(dummy_yc == 1,
         export_dummy == 1,
         year >= 1990) %>%
  filter(farm_prices_ymc < quantile(farm_prices_ymc, 0.95, na.rm = TRUE) &
           farm_prices_ymc > quantile(farm_prices_ymc, 0.05, na.rm = TRUE)) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    farm_prices_y = mean(farm_prices_ymc, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(aux2, aes(x = year, y = farm_prices_y, 
                 color = as.factor(dummy_leader), group = as.factor(dummy_leader))) +
  geom_line(size = 1.2) +
  geom_point(size = 3)

aux3 <- data %>%
  filter(year >= 1990) %>%
  filter(farm_prices_ymc < quantile(farm_prices_ymc, 0.95, na.rm = TRUE) &
           farm_prices_ymc > quantile(farm_prices_ymc, 0.05, na.rm = TRUE)) 

ggplot(aux3, aes(x = as.factor(year), y = farm_prices_ymc, fill = as.factor(dummy_leader))) +
  geom_boxplot() 

mean(aux2$farm_prices_y[aux2$dummy_leader == 1])
mean(aux2$farm_prices_y[aux2$dummy_leader == 0])

# Profits
aux1 <- results_ymc %>%
  filter(scenario == "Baseline") %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    profits_y = sum(profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    profits_total_y = sum(profits_y, na.rm = TRUE),
    profits_group_r = round(profits_y/profits_total_y, 2)
  ) %>%
  ungroup()


ggplot(aux1, aes(x = year, y = profits_group_r, 
                 color = as.factor(dummy_leader), group = as.factor(dummy_leader))) +
  geom_line(size = 1.2) +
  geom_point(size = 3)
#-------------------------------------------------------------------------------
# [] Líderes y seguidores - contrafactual
#-------------------------------------------------------------------------------

# Cantidades

aux1 <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    qe_y = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

plot7 <- ggplot(aux1, aes(x = year, y = qe_y, 
                          color = factor(dummy_leader,
                                         levels = c(0, 1),
                                         labels = c("Seguidores", "Líderes")), 
                          group = dummy_leader)) +
  geom_line(size = 0.8) +
  geom_point(size = 1) +
  labs(
    title = NULL,
    x = NULL,
    y = "mill. 60 kg.",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot7.png", 
       plot = plot7, 
       width = 7,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

# Revenues
aux1 <- results_ymc %>%
  filter(scenario == "CF1") %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    revenue_y = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()


ggplot(aux1, aes(x = year, y = revenue_y, 
                 color = as.factor(dummy_leader), group = as.factor(dummy_leader))) +
  geom_line(size = 1.2) +
  geom_point(size = 3)

#-------------------------------------------------------------------------------
# [] Fit de precios
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
# [] Fit de precios y mc estimados
#-------------------------------------------------------------------------------
aux1 <- data %>%
  filter(dummy_ym == 1) %>%
  select(date, price_ym) %>%
  mutate(date = as.Date(date)) %>%
  left_join(results_ym %>% 
              filter(scenario == "Baseline") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  left_join(results_ymc %>%
              filter(scenario == "Baseline") %>%
              mutate(date = as.Date(date)) %>%
              group_by(date) %>%
              summarise(
                mc_ym = mean(mc_ymc_hat, na.rm = TRUE),
                .groups = "drop"
              ), by = c("date")) %>%
  distinct(date, price, price_ym, mc_ym) %>%
  rename(price_ym_estimated = price) %>%
  drop_na()

plot5 <- ggplot(aux1 %>%
                  filter(year(date) >= 1990)) +
  geom_line(aes(x = date, y = price_ym, color = "Precio obs."), 
            linewidth = 0.3, alpha = 1) +
  geom_line(aes(x = date, y = price_ym_estimated, color = "Precio est."), 
            linewidth = 0.3, alpha = 1, linetype = "dashed") +
  geom_line(aes(x = date, y = mc_ym, color = "Costo est."), 
            linewidth = 0.3, alpha = 1, linetype = "dashed") +
  #scale_color_manual(
  #  name = NULL,
  #  values = c("Precio obs." = "black", "Precio est." = "blue2", 
  #             "Costo est." = "red2")
  #) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "cents/lb",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot5.png", 
       plot = plot5, 
       width = 9,
       height = 5,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Fit de cantidades
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

plot6 <- ggplot(aux2 %>%
                  filter(year >= 1990)) +
  geom_line(aes(x = year, y = qe_y, color = "Obs."), 
            linewidth = 0.5, alpha = 1) +
  geom_line(aes(x = year, y = qe_y_estimated, color = "Est."), 
            linewidth = 0.5, alpha = 1, linetype = "dashed") +
  #scale_color_manual(
  #  name = NULL,
  #  values = c("Obs." = "black", "Est." = "blue2")
  #) +
  labs(
    title = NULL,
    x = NULL,
    y = "Exp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot6.png", 
       plot = plot6, 
       width = 9,
       height = 6,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Precios mensuales estimados (contrafactual)
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
# [] Costos marginales mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  group_by(year, scenario) %>%
  summarise(
    mc_y = mean(mc_ymc_hat, na.rm = TRUE),
    .groups = "drop"
  )

plot8 <- ggplot(aux %>%
                  filter(year >= 1990 ), aes(x = year, y = mc_y, 
                                             color = factor(scenario,
                                                            levels = c("Baseline", "CF1"),
                                                            labels = c("Factual", "Contrafactual")), 
                                             group = scenario)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  labs(
    title = NULL,
    x = NULL,
    y = "Rem. (cents/lb)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot8.png", 
       plot = plot8, 
       width = 9,
       height = 6,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Cantidades mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  group_by(year, scenario) %>%
  summarise(
    qe_y = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

plot9 <- ggplot(aux %>%
                  filter(year >= 1990 ), aes(x = year, y = qe_y, 
                                             color = factor(scenario,
                                                            levels = c("Baseline", "CF1"),
                                                            labels = c("Factual", "Contrafactual")), 
                                             group = scenario)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  labs(
    title = NULL,
    x = NULL,
    y = "Exp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot9.png", 
       plot = plot9, 
       width = 9,
       height = 6,
       units = "cm",
       dpi = 600,
       bg = "white")


#-------------------------------------------------------------------------------
# [] Mapamundi
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


#-------------------------------------------------------------------------------
# [] Resumen de exportaciones
#-------------------------------------------------------------------------------

data_c <- data %>%
  filter(dummy_yc == 1, 
         export_dummy == 1) %>%
  group_by(country) %>%
  summarise(
    qe_c = sum(qe_yc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(desc(qe_c)) %>%
  mutate(
    r = round(qe_c/sum(qe_c), 2)
  )

#-------------------------------------------------------------------------------
# [] Efecto de GDD y FDD
#-------------------------------------------------------------------------------
# Efecto de gdd y fdd en farm prices
data_y <- data %>%
  filter(dummy_y == 1, year >= 1965, year <= 2019)

plot2 <- ggplot(data_y, aes(x = gdd_y_mean, y = farm_prices_y_mean)) +
  geom_point(size = 0.3, color = "#2E86AB", alpha = 1) +  # Puntos más pequeños
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed", linewidth = 0.3) +
  labs(
    title = NULL,
    x = "Grados-día crecimiento (18-26°C)",
    y = "Rem. (cents/lb)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),           # Texto general más pequeño
    axis.title = element_text(size = 9),      # Títulos de ejes más pequeños
    axis.title.y = element_text(margin = margin(r = 3)),  # Menos espacio
    axis.text = element_text(size = 8),       # Texto de ejes más pequeño
    plot.margin = margin(5, 5, 5, 5)          # Márgenes mínimos
  )

# Guardar con dimensiones optimizadas para columna estrecha
ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot2.png", 
       plot = plot2, 
       width = 7,        # cm (aprox 0.18 del ancho del A0 vertical)
       height = 5,       # cm - relación 7:5
       units = "cm",
       dpi = 600,        # Mayor resolución para impresión nítida
       bg = "white")

plot3 <- ggplot(data_y, aes(x = fdd_y_mean, y = farm_prices_y_mean)) +
  geom_point(size = 0.3, color = "#2E86AB", alpha = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed", linewidth = 0.3) +
  labs(
    title = NULL,
    x = "Grados-día helada (<10°C)",
    y = "Rem. (cents/lb)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot3.png", 
       plot = plot3, 
       width = 7,        # Mismo tamaño que plot2 para consistencia
       height = 5,       # Misma altura
       units = "cm",
       dpi = 600,
       bg = "white")

# Efecto de gdd, hdd y fdd en precios
ggplot(data_y, aes(x = gdd_y_mean, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = hdd_y_mean, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = fdd_y_mean, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Efecto de gdd, hdd y fdd en cantidades
ggplot(data_y, aes(x = gdd_y, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = hdd_y, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggplot(data_y, aes(x = fdd_y, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

plot4 <- ggplot(data_y, aes(x = fdd_y_mean, y = qe_y)) +
  geom_point(size = 1, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = NULL,
    x = "Grados-día helada (<10°C)",
    y = "Exp. (mill. 60kg)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/figures/plot4.png", 
       plot = plot4, 
       width = 7,        # Mismo tamaño que plot4 para consistencia
       height = 5,       # Misma altura
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Boxplot GDP
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# [] Regresiones. Efecto de cambio climático
#-------------------------------------------------------------------------------
# El CC acentúa las diferencias
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  group_by(year, dummy_leader, scenario) %>%
  summarise(
    qe_y = mean(quantity, na.rm = TRUE),
    re_y = mean(revenue, na.rm = TRUE),
    pr_y= mean(profit, na.rm = TRUE),
    mc_y = mean(mc_ymc_hat, na.rm = TRUE),
    .groups = "drop"
  )

aux1 <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  mutate(
    log_qe_ymc = log(quantity),
    log_pr_ymc = log(profit),
    log_mc_ymc = log(mc_ymc_hat)
  )

aux2 <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    scenario = case_when(
      scenario == "Baseline" ~ 0,
      TRUE ~ 1
    )
  ) %>%
  filter(year >= 1990) %>%
  group_by(year, country, scenario) %>%
  mutate(
    qe_yc = sum(quantity, na.rm = TRUE),
    pr_yc = sum(profit, na.rm = TRUE),
    mc_yc = mean(mc_ymc_hat, na.rm = TRUE),
    log_qe_yc = log(qe_yc),
    log_pr_yc = log(pr_yc),
    log_mc_yc = log(mc_yc)
  ) %>%
  ungroup() %>%
  distinct(year, country, dummy_leader, 
           qe_yc, log_qe_yc, pr_yc, log_pr_yc, 
           mc_yc, log_mc_yc, scenario)

feols(profit ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)
feols(quantity ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)
feols(mc_ymc_hat ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)
feols(log_pr_ymc ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)
feols(log_qe_ymc ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)
feols(log_mc_ymc ~ scenario * dummy_leader | year, data = aux1, cluster = ~year)

feols(pr_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)
feols(qe_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)
feols(mc_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)
feols(log_pr_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)
feols(log_qe_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)
feols(log_mc_yc ~ scenario * dummy_leader | year, data = aux2, cluster = ~year)

# Cambios en profits y costos marginales
data_ymc_cf <- results_ymc %>%
  mutate(
    quantity = quantity * 60000 # To tons
  ) %>%
  select(date, country, revenue, profit, quantity, mc_ymc_hat, scenario, dummy_leader) %>%
  pivot_longer(
    cols = c(profit, mc_ymc_hat, quantity, revenue),
    names_to = "variable",
    values_to = "value"
  ) %>%
  unite("variable_scenario", variable, scenario, sep = "_") %>%
  pivot_wider(
    names_from = variable_scenario,
    values_from = value
  )

data_ymc_cf <- data_ymc_cf %>%
  mutate(
    dummy_follower = 1 - dummy_leader,
    d_profit = (profit_CF1 - profit_Baseline) * 1000000, # To USD
    d2_profit = profit_CF1 / profit_Baseline - 1,
    d2_mc = mc_ymc_hat_Baseline / mc_ymc_hat_CF1 - 1,
    d_mc = mc_ymc_hat_Baseline - mc_ymc_hat_CF1, # Cents per lb
    d_mc2 = d_mc^(2),
    dlog_profit = log(profit_CF1 / profit_Baseline),
    dlog_mc = log(mc_ymc_hat_Baseline / mc_ymc_hat_CF1),
    d_qe = quantity_CF1 - quantity_Baseline,
    d2_qe = quantity_CF1 / quantity_Baseline - 1,
    dlog_qe = log(quantity_CF1 / quantity_Baseline),
    year = year(date),
    d_re = revenue_CF1 - revenue_Baseline,
    dlog_re = log(revenue_CF1/revenue_Baseline)
  )

aux <- data_ymc_cf %>%
  group_by(country) %>%
  summarise(
    d_profit = round(sum(d_profit, na.rm = TRUE), 2),
    d_mc = round(sum(d_mc, na.rm = TRUE), 2),
    d_qe = round(sum(d_qe, na.rm = TRUE), 2),
    .groups = "drop"
  )

aux1 <- data_ymc_cf %>%
  group_by(country) %>%
  summarise(
    d2_profit = round(mean(d2_profit, na.rm = TRUE), 4),
    d2_mc = round(mean(d2_mc, na.rm = TRUE), 4),
    d2_qe = round(mean(d2_qe, na.rm = TRUE), 4),
    .groups = "drop"
  )

# Cuanto mayor es la disminución en cm, mayor es la ganancia en profit quantity, pero el efecto es cóncavo
m1 <- feols(
  d_profit ~ d_mc 
  #+ d_mc2 
  | country + year, 
  data = data_ymc_cf
)

summary(m1)

m1 <- feols(
  d_qe ~ d_mc 
  #+ d_mc2 
  | country + year, 
  data = data_ymc_cf
)

summary(m1)

m1 <- feols(
  dlog_qe ~ dlog_mc 
  | country + year, 
  data = data_ymc_cf
)

summary(m1)

m1 <- feols(
  d_re ~ d_mc 
  | country + year, 
  data = data_ymc_cf
)

summary(m1)

m1 <- feols(
  dlog_re ~ dlog_mc 
  | country + year, 
  data = data_ymc_cf
)

summary(m1)

# Cuanto mayor es la disminución en cm, mayor es la ganancia en profit. El efecto es mayor para líderes
m2 <- feols(
  d_profit ~ d_mc * dummy_leader | year, 
  data = data_ymc_cf
)

summary(m2)

m2 <- feols(
  d_qe ~ d_mc * dummy_leader | year, 
  data = data_ymc_cf
)

summary(m2)

m3 <- feols(
  dlog_qe ~ dlog_mc * dummy_leader | year, 
  data = data_ymc_cf
)

summary(m3)

#-------------------------------------------------------------------------------
# [] Líderes y seguidores - Costos implícitos y remuneraciones
#-------------------------------------------------------------------------------
# Efecto desigual en remuneraciones
data_yc <- data %>%
  filter(dummy_yc == 1,
         year >= 1990) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_farm_yc = log(farm_prices_yc)
  )

feols(qe_yc ~ farm_prices_yc * dummy_leader | year, data = data_yc, cluster = ~year)
feols(log_qe_yc ~ log_farm_yc * dummy_leader | year, data = data_yc, cluster = ~year)

# Efecto desigual en costos estimados
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990,
         scenario == "Baseline") %>%
  group_by(
    country, year
  ) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    mc_yc = mean(mc_ymc_hat, na.rm = TRUE),
    dummy_leader = mean(dummy_leader, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_mc_yc = log(mc_yc)
  )

feols(qe_yc ~ mc_yc * dummy_leader | year, data = aux, cluster = ~year)
feols(log_qe_yc ~ log_mc_yc * dummy_leader | year, data = aux, cluster = ~year)


aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990,
         scenario == "Baseline") %>%
  group_by(
    country, year
  ) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    mc_yc = mean(mc_ymc_hat, na.rm = TRUE),
    dummy_leader = mean(dummy_leader, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_mc_yc = log(mc_yc)
  )

feols(qe_yc ~ mc_yc * dummy_leader | year, data = aux, cluster = ~year)
feols(log_qe_yc ~ log_mc_yc * dummy_leader | year, data = aux, cluster = ~year)

# Efecto desigual en costos estimados + cambio climático
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  group_by(
    country, year, scenario
  ) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    mc_yc = mean(mc_ymc_hat, na.rm = TRUE),
    dummy_leader = mean(dummy_leader, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_mc_yc = log(mc_yc)
  )

feols(qe_yc ~ mc_yc * dummy_leader * scenario| year, data = aux, cluster = ~year)
feols(log_qe_yc ~ log_mc_yc * dummy_leader * scenario| year, data = aux, cluster = ~year)

#-------------------------------------------------------------------------------
# [] Líderes y seguidores - Expr y costos
#-------------------------------------------------------------------------------
# Efecto desigual en remuneraciones
data_yc <- data %>%
  filter(dummy_yc == 1,
         year >= 1990) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_farm_yc = log(farm_prices_yc)
  )

feols(farm_prices_yc ~  dummy_leader | year, data = data_yc, cluster = ~year)
feols(log_farm_yc ~ dummy_leader | year, data = data_yc, cluster = ~year)

# Efecto desigual en costos estimados
aux <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990,
         scenario == "Baseline") %>%
  group_by(
    country, year
  ) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    mc_yc = mean(mc_ymc_hat, na.rm = TRUE),
    dummy_leader = mean(dummy_leader, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_qe_yc = log(qe_yc),
    log_mc_yc = log(mc_yc)
  )

feols(mc_yc ~ dummy_leader | year, data = aux, cluster = ~year)
feols(log_mc_yc ~ dummy_leader | year, data = aux, cluster = ~year)



#-------------------------------------------------------------------------------
# [] HHI
#-------------------------------------------------------------------------------

data_yc <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  group_by(year, country, scenario) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(scenario == "Baseline") %>%
  select(-scenario) %>%
  group_by(year) %>%
  mutate(
    qe_y = sum(qe_yc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, country) %>%
  mutate(
    sh_yc = round(qe_yc / qe_y, 2)
  ) %>%
  group_by(year) %>%
  mutate(
    hhi_y = 10000 * (sum(sh_yc^2, na.rm = TRUE))
  ) %>%
  ungroup()

mean(data_yc$hhi_y)

data_yc <- results_ymc %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year >= 1990) %>%
  group_by(year, country, scenario) %>%
  summarise(
    qe_yc = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(scenario == "CF1") %>%
  select(-scenario) %>%
  group_by(year) %>%
  mutate(
    qe_y = sum(qe_yc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, country) %>%
  mutate(
    sh_yc = round(qe_yc / qe_y, 2)
  ) %>%
  group_by(year) %>%
  mutate(
    hhi_y = 10000 * (sum(sh_yc^2, na.rm = TRUE))
  ) %>%
  ungroup()

mean(data_yc$hhi_y)
