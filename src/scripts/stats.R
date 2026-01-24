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
library(viridis)
library(kableExtra)
library(readr)
library(readxl)

library(tidyverse)
library(scales)
library(countrycode) 

library(ggrepel)

# Rutas
fig_path <- "/Users/sebastianchacon/Desktop/sc_tesis/paper/figures/"
tab_path <- "/Users/sebastianchacon/Desktop/sc_tesis/paper/tables/"
filename_psdcoffee <- "psd_coffee.csv" 
main_dir <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/"
filepath_psdcoffee <- paste(main_dir, filename_psdcoffee,sep="") 

# Data
data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data.rds")
results_y <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_y.rds")
results_yc <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/results_yc.rds")
summary <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary.rds")
summary_c <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/summary_c.rds")

psd_coffee <- read_csv("/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/psd_coffee.csv")

data_cf <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data_cf.rds")

#-------------------------------------------------------------------------------
# [VF] Evolución de exposición al óptimo, al calor y al frío
#-------------------------------------------------------------------------------
aux <- data %>%
  filter(dummy_y == 1, year >= 1965, year <= 2019) %>%
  distinct(year, optExp_y_mean, heatExp_y_mean, frostExp_y_mean,
           gdd_y_mean, gdd_y, hdd_y_mean, hdd_y, fdd_y_mean, fdd_y) %>%
  drop_na()

plot02_01 <- ggplot(
  aux) +
  geom_line(
    aes(
      x = year, 
      y = optExp_y_mean
        ),
    linewidth = 0.7, alpha = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Días",
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

plot02_02 <- ggplot(
  aux) +
  geom_line(
    aes(x = year, y = heatExp_y_mean),
    linewidth = 0.7, alpha = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Días",
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

plot02_03 <- ggplot(
  aux) +
  geom_line(
    aes(x = year, y = frostExp_y_mean),
    linewidth = 0.7, alpha = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Días",
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

ggsave(
  filename = paste0(fig_path, "plot02_01.png"),
  plot = plot02_01,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "plot02_02.png"),
  plot = plot02_02,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "plot02_03.png"),
  plot = plot02_03,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

#-------------------------------------------------------------------------------
# [VF] Exportaciones desde 1960 hasta 2019
#-------------------------------------------------------------------------------
aux1 <- data %>%
  filter(dummy_y == 1) %>%
  select(date, qe_y) %>%
  mutate(
    date = as.Date(date),
    year = year(date)) %>%
  distinct(year, qe_y) %>%
  drop_na()

summary(aux1$qe_y)
sd(aux1$qe_y)

plot1 <- ggplot(
  aux1) +
  geom_line(
    aes(x = year, y = qe_y),
    linewidth = 0.7, alpha = 1
  ) +
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

ggsave(
      filename = paste0(fig_path, "export1960.png"),
      plot = plot1,
      width = 7,
      height = 5,
      units = "cm",
      dpi = 600,
      bg = "white")

#-------------------------------------------------------------------------------
# [VF] Precio ICIP desde 1960 hasta 2019
#-------------------------------------------------------------------------------
aux2 <- data %>%
  filter(dummy_ym == 1) %>%
  select(date, price_ym) %>%
  mutate(date = as.Date(date)) %>%
  distinct(date, price_ym) %>%
  drop_na()

summary(aux2$price_ym)
sd(aux2$price_ym)

aux2_y <- data %>%
  filter(dummy_y == 1) %>%
  select(year, price_y) %>%
  distinct(year, price_y) %>%
  drop_na()

plot2 <- ggplot(
  aux2) +
  geom_line(aes(x = date, y = price_ym),
            linewidth = 0.7, alpha = 1
  ) +
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/paper/figures/icip1960.png",
      plot = plot2,
      width = 7,
      height = 5,
      units = "cm",
      dpi = 600,
      bg = "white")

#-------------------------------------------------------------------------------
# [VF] Boxplot GDP Exportadores e importadores
#-------------------------------------------------------------------------------

data_yc <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  mutate(
    type = case_when(
      export_dummy == 1 ~ "Exportadores",
      export_dummy == 0 ~ "Importadores"
    ))

plot3 <- ggplot(data_yc, aes(factor(year), y = rgdpe_yc, fill = type)) +
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
    y = "PIB real ajustado (log. trill. USD)",       # Ajusta según tu variable
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/paper/figures/plot3.png", 
      plot = plot3, 
      width = 18,
      height = 8,
      units = "cm",
      dpi = 600,
      bg = "white")

#-------------------------------------------------------------------------------
# [VF] Boxplot de heatExp_yc y frostExp_yc
#-------------------------------------------------------------------------------

data_yc <- data %>%
  filter(dummy_yc == 1, year <= 2019, net_export_dummy == 1)

ggplot(data_yc, aes(x = year, y = optExp_yc)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(aes(group = country), method = "lm", se = FALSE, 
              color = "gray", size = 0.5, alpha = 0.5) +
  theme_minimal() +
  labs(x = "Año", y = "Días óptimos",
       title = "Tendencias individuales (gris) y global (azul)")

ggplot(data_yc, aes(x = factor(year), y = heatExp_yc)) +
  geom_boxplot(
    position = position_dodge(0.8),
    width = 0.7,
    alpha = 0.8,
    size = 0.5,
    color = "black"
  ) +
  # Línea del promedio usando stat_summary
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    color = "darkred",
    size = 1,
    linetype = "solid"
  ) +
  # Puntos del promedio
  stat_summary(
    fun = mean,
    geom = "point",
    aes(group = 1),
    color = "darkred",
    size = 2,
    shape = 18
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Dias",
    fill = NULL
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
  ) +
  scale_fill_grey(
    start = 0.2,
    end = 0.8,
    na.value = "red"
  ) +
  scale_x_discrete(
    breaks = function(x) {
      years <- as.numeric(as.character(x))
      seq_years <- seq(min(years), max(years), by = 5)
      as.character(seq_years)
    }
  )
  

plot3 <- ggplot(data_yc, aes(factor(year), y = heatExp, fill = type)) +
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/paper/figures/plot3.png", 
       plot = plot3, 
       width = 18,
       height = 8,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [VF] Boxplot de ND-GAIN
#-------------------------------------------------------------------------------
data_yc <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  mutate(
    type = case_when(
      export_dummy == 1 ~ "Exportadores",
      export_dummy == 0 ~ "Importadores"
    ),
    group = case_when(
      dummy_leader == 1 ~ "Líderes",
      dummy_leader == 0 ~ "Seguidores"
    ))

plot03_09 <- ggplot(data_yc, aes(factor(year), y = ndgain_yc, fill = type)) +
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
    y = "ND-GAIN",       # Ajusta según tu variable
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

ggsave(paste0(fig_path, "plot03_09.png"), 
       plot = plot03_09, 
       width = 18,
       height = 8,
       units = "cm",
       dpi = 600,
       bg = "white")


#-------------------------------------------------------------------------------
# [] Evolución de importaciones 2002 hasta 2019
#-------------------------------------------------------------------------------
data_y <- data %>%
  filter(dummy_yc == 1, year >= 2002, year <= 2019) %>%
  group_by(year) %>%
  summarise(
    qi_y = sum(qi_yc, na.rm = TRUE)/1000,
    .groups = "drop"
  )

plot4 <- ggplot(data_y) +
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/paper/figures/import2002.png", 
      plot = plot4, 
      width = 7,
      height = 5,
      units = "cm",
      dpi = 600,
      bg = "white")

#-------------------------------------------------------------------------------
# Países más afectados por GDD, HDD, FDD
#-------------------------------------------------------------------------------
clima_dataset <- readRDS("~/Desktop/sc_tesis/bld/data/df_gdd_complete_1961_2020.rds")

clima_yc <- clima_dataset %>%
  group_by(country, year) %>%
  summarise(
    hdd_yc = sum(hdd_ymc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  filter(year %in% c(1980, 2019)) %>%
  pivot_wider(
    names_from = year, 
    values_from = hdd_yc,
    names_prefix = "hdd_"
  ) %>%
  mutate(
    delta_hdd_yc = hdd_2019 - hdd_1980
  ) %>%
  filter(!is.na(delta_hdd_yc))

world <- ne_countries(scale = "medium", returnclass = "sf")

world_data <- world %>%
  left_join(clima_yc, by = c("name" = "country")) 

ggplot(world_data) +
  geom_sf(aes(fill = delta_hdd_yc), color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Cambio en HDD\n(1991-2019)",
    option = "plasma",
    na.value = "grey90",
    n.breaks = 8,  # Más intervalos en la leyenda
    breaks = scales::breaks_extended(n = 8)  # 8 breaks equidistantes
  ) +
  labs(
    title = "Cambio en Grados Día de Calor (HDD) entre 1991-2019",
    subtitle = "Países productores de café",
    caption = "Fuente: Tus datos"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


#-------------------------------------------------------------------------------
# [VF] Líderes y seguidores - Factual
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

aux2 <- aux1 %>%
  filter(year == 2019) %>%
  group_by(dummy_leader) %>%
  summarise(
    qe = sum(qe_y, na.rm = TRUE)
  ) %>%
  ungroup()

plot02_04 <- ggplot(aux1, aes(x = year, y = qe_y, 
                              color = factor(dummy_leader,
                                             levels = c(0, 1),
                                             labels = c("Seguidores", "Líderes")), 
                              group = dummy_leader)) +
  geom_line(linewidth = 0.7, alpha = 1) +
  # Especificar colores manualmente
  scale_color_manual(values = c("Seguidores" = "gray60", 
                                "Líderes" = "black")) +
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

ggsave(
  filename = paste0(fig_path, "plot02_04.png"),
  plot = plot02_04,
  width = 8,
  height = 6,
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
aux1 <- results_yc %>%
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
# [VF] Fit de precios
#-------------------------------------------------------------------------------
aux1 <- data %>%
  filter(dummy_y == 1) %>%
  select(date, price_y) %>%
  mutate(date = as.Date(date)) %>%
  left_join(results_y %>% 
              #filter(scenario == "CF0") %>%
              filter(scenario == "CF0") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  distinct(date, price, price_y) %>%
  rename(price_y_estimated = price) %>%
  drop_na()

plot_0601 <- ggplot(aux1) +
  geom_line(aes(x = date, y = price_y, color = "Obs."), 
            linewidth = 0.8, alpha = 0.8) +
  geom_line(aes(x = date, y = price_y_estimated, color = "Est."), 
            linewidth = 0.8, alpha = 0.8, linetype = "dashed") +
  scale_color_manual(
    name = NULL,
    values = c("Obs." = "gray40", "Est." = "black")
  ) +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y",
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "¢")
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "¢/lb"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = paste0(fig_path, "plot_0601.png"),
  plot = plot_0601,
  width = 12,
  height = 7,
  units = "cm",
  dpi = 600,
  bg = "white")

#-------------------------------------------------------------------------------
# [VF] Fit de cantidades
#-------------------------------------------------------------------------------
aux2 <- data %>%
  filter(dummy_y == 1) %>%
  select(date, qe_y) %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  left_join(results_y %>% 
              #filter(scenario == "CF0") %>%
              filter(scenario == "CF0") %>%
              mutate(date = as.Date(date),
                     year = year(date)) %>%
              group_by(year) %>%
              mutate(qe_y_estimated = sum(Q_total, na.rm = TRUE)) %>%
              ungroup() %>%
              select(year, qe_y_estimated), by = c("year")) %>%
  distinct(year, qe_y, qe_y_estimated) %>%
  drop_na()

plot_0602 <- ggplot(aux2) +
  geom_line(aes(x = year, y = qe_y, color = "Obs."), 
            linewidth = 0.5, alpha = 1) +
  geom_line(aes(x = year, y = qe_y_estimated, color = "Est."), 
            linewidth = 0.5, alpha = 1, linetype = "dashed") +
  scale_color_manual(
    name = NULL,
    values = c("Obs." = "gray40", "Est." = "black")
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Exp. (mill. 60 kg)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 3)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5, b = 0)
  )

ggsave(paste0(fig_path, "plot_0602.png"), 
       plot = plot_0602, 
       width = 12,
       height = 7,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Precios mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------

ggplot(results_y, aes(x = date, y = price, color = scenario, group = scenario)) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.8) +
  scale_color_manual(
    values = c("Base" = "blue", "Contrafactual" = "red") 
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
    mc_y = mean(mc_ymc_hat_1, na.rm = TRUE),
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/figures/plot8.png", 
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

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/figures/plot9.png", 
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
data_yc <- data %>%
  filter(dummy_yc == 1, 
         export_dummy == 1,
         year >= 1965, year <= 2019)

data_y <- data %>%
  filter(dummy_y == 1,
         net_export_dummy == 1, tas_ye > 0)

# Efecto de gdd, hdd y fdd en precios

ggplot(data_y, aes(x = tas_ye, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Efecto de gdd, hdd y fdd en cantidades
ggplot(data_yc, aes(x = frostExp_y_mean, y = qe_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

ggsave("/Users/sebastianchacon/Desktop/sc_tesis/bld/figures/plot4.png", 
       plot = plot4, 
       width = 7,        # Mismo tamaño que plot4 para consistencia
       height = 5,       # Misma altura
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Regresiones. Efecto de cambio climático diferenciado por grupo
#-------------------------------------------------------------------------------
# Por año-país

aux <- results_yc %>%
  filter(scenario == "CF0" | scenario == "CF1") %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    trend5y = floor((year - min(year)) / 5),
    scenario = case_when(
      scenario == "CF0" ~ 0,
      scenario == "CF1" ~ 1,
      TRUE ~ NA
    )
  )

m1 <- lm(profit ~ scenario * dummy_leader, data = aux)
m2 <- lm(quantity ~ scenario * dummy_leader, data = aux)
m3 <- lm(mc_yc_hat_s ~ scenario * dummy_leader, data = aux)

summary(m3)


#-------------------------------------------------------------------------------
# [VF] Evolución de HHI, C3, Lerner 
#-------------------------------------------------------------------------------
# HHI
aux <- data %>%
  filter(
    dummy_yc == 1, 
    net_export_dummy == 1,
    year %in% 1990:2020) %>%
  select(year, country, qe_yc, qe_y) %>%
  arrange(year, country) %>%
  mutate(
    sh_yc = round(qe_yc / qe_y, 4),
    sh2_yc = sh_yc^2
    ) %>%
  group_by(year) %>%
  summarise(
    hhi_y = round(sum(sh2_yc, na.rm = TRUE) * 10000, 0),
    .groups = 'drop'
  ) %>%
  ungroup()
  
plot_030401 <- ggplot(
  aux) +
  geom_line(
    aes(x = year, y = hhi_y),
    linewidth = 0.7, alpha = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
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

ggsave(
  filename = paste0(fig_path, "hhi.png"),
  plot = plot_030401,
  width = 7.5,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

# C3
aux <- data %>%
  filter(
    dummy_yc == 1, 
    country %in% c("Brazil", "Colombia", "Viet Nam"),
    year %in% 1990:2020) %>%
  select(year, country, qe_yc, qe_y) %>%
  arrange(year, country) %>%
  group_by(year) %>%
  mutate(
    qe_leaders_y = sum(qe_yc, na.rm = TRUE),
    sh_leaders_y = round(qe_leaders_y / qe_y, 2)
  ) %>%
  ungroup() %>%
  distinct(year, sh_leaders_y)

plot_030402 <- ggplot(
  aux) +
  geom_line(
    aes(x = year, y = sh_leaders_y),
    linewidth = 0.7, alpha = 1
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.25, 0.7)) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
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

ggsave(
  filename = paste0(fig_path, "c3.png"),
  plot = plot_030402,
  width = 7.5,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

#-------------------------------------------------------------------------------
# [] Estadística de variables para demanda y costos
#-------------------------------------------------------------------------------

aux_y <- data %>%
  filter(dummy_y == 1) %>%
  select(year, qe_y, price_y, importGDP_y, teaPrices_y)

summary(aux_y$qe_y)
summary(aux_y$price_y)
summary(aux_y$importGDP_y)
summary(aux_y$teaPrices_y)

aux_ym <- data %>%
  filter(dummy_ym == 1) %>%
  select(qe_ym, price_ym, importGDP_ym, teaPrices_ym, fert_ym)

summary(aux_ym$qe_ym)
summary(aux_ym$price_ym)
summary(aux_ym$importGDP_ym)
summary(aux_ym$teaPrices_ym)

aux_y <- data %>%
  filter(dummy_y == 1, year %in% 1990:2020) %>%
  select(year, fert_y)

aux_ym <- data %>%
  filter(dummy_ym == 1, year %in% 1990:2020) %>%
  select(year, fert_ym)

summary(aux_y$fert_y)
summary(aux_ym$fert_ym)

#-------------------------------------------------------------------------------
# [VF] Estadística de variables climáticas
#-------------------------------------------------------------------------------
# Frecuencia mensual
data_ym <- data %>%
  filter(dummy_ym == 1,
         year <= 2019,
         year >= 1965) %>%
  select(date, 
         gdd_ym, gdd_ym_mean, 
         hdd_ym, hdd_ym_mean, 
         fdd_ym, fdd_ym_mean,
         optExp_ym, optExp_ym_mean,
         heatExp_ym, heatExp_ym_mean,
         frostExp_ym, frostExp_ym_mean)

summary(data_ym$gdd_ym)
summary(data_ym$gdd_ym_mean)
summary(data_ym$hdd_ym)
summary(data_ym$hdd_ym_mean)
summary(data_ym$fdd_ym)
summary(data_ym$fdd_ym_mean)

sd(data_ym$gdd_ym_mean)
sd(data_ym$gdd_ym)
sd(data_ym$hdd_ym_mean)
sd(data_ym$hdd_ym)
sd(data_ym$fdd_ym_mean)
sd(data_ym$fdd_ym)

summary(data_ym$optExp_ym_mean)
summary(data_ym$optExp_ym)
summary(data_ym$heatExp_ym_mean)
summary(data_ym$heatExp_ym)
summary(data_ym$frostExp_ym_mean)
summary(data_ym$frostExp_ym)

sd(data_ym$optExp_ym_mean)
sd(data_ym$optExp_ym)
sd(data_ym$heatExp_ym_mean)
sd(data_ym$heatExp_ym)
sd(data_ym$frostExp_ym_mean)
sd(data_ym$frostExp_ym)

# Frecuencia anual
data_y <- data %>%
  filter(dummy_y == 1,
         year <= 2019,
         year >= 1965) %>%
  select(date, 
         gdd_y, gdd_y_mean, 
         hdd_y, hdd_y_mean, 
         fdd_y, fdd_y_mean,
         optExp_y, optExp_y_mean,
         heatExp_y, heatExp_y_mean,
         frostExp_y, frostExp_y_mean)

summary(data_y$gdd_y_mean)
summary(data_y$gdd_y)
summary(data_y$hdd_y_mean)
summary(data_y$hdd_y)
summary(data_y$fdd_y_mean)
summary(data_y$fdd_y)

sd(data_y$gdd_y_mean)
sd(data_y$gdd_y)
sd(data_y$hdd_y_mean)
sd(data_y$hdd_y)
sd(data_y$fdd_y_mean)
sd(data_y$fdd_y)

summary(data_y$optExp_y_mean)
summary(data_y$optExp_y)
summary(data_y$heatExp_y_mean)
summary(data_y$heatExp_y)
summary(data_y$frostExp_y_mean)
summary(data_y$frostExp_y)

sd(data_y$optExp_y_mean)
sd(data_y$optExp_y)
sd(data_y$heatExp_y_mean)
sd(data_y$heatExp_y)
sd(data_y$frostExp_y_mean)
sd(data_y$frostExp_y)

#-------------------------------------------------------------------------------
# [VF] Exportaciones por país y por especie
#-------------------------------------------------------------------------------
aux <- psd_coffee %>%
  select(Country_Name, Market_Year, Attribute_Description, Attribute_ID, Value) %>%
  rename(country = Country_Name, year = Market_Year, operation = Attribute_Description,
         id = Attribute_ID, value = Value) %>%
  mutate(country = if_else(country == "Congo (Brazzaville)","Congo",
                           if_else(country == "Congo (Kinshasa)","Congo",
                                   if_else(country == "Yemen (Sanaa)", "Yemen",
                                           if_else(country == "Vietnam","Viet Nam",
                                                   if_else(country == "Cote d'Ivoire","Côte d'Ivoire", country)))))) %>%
  filter(value > 0 & (id == "029" | id == "053")) # id: 29 (arabica production) 53 (robusta production)

# We sum both Congos
aux <- aux %>%
  group_by(year, country, operation, id) %>%
  summarise(
    value = sum(value),
    .groups = 'drop') %>%
  ungroup()

# Decades
aux <- aux %>%
  mutate(
    decade = floor(year / 10) * 10
  )

# Lustrum
aux <- aux %>%
  mutate(
    lustrum = floor(year / 5) * 5
  )

# Ranking for Arabica and Robusta
aux_c <- aux %>%
  group_by(country, operation) %>%
  summarise(
    value_c = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(operation) %>%
  mutate(
    value = sum(value_c, na.rm = TRUE),
    share_c = round(value_c / value, 2)
  ) %>%
  ungroup() %>%
  arrange(operation, desc(share_c))

# Ranking for Arabica and Robusta by decade
aux_dc <- aux %>%
  group_by(country, operation, decade) %>%
  summarise(
    value_c = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(operation, decade) %>%
  mutate(
    value = sum(value_c, na.rm = TRUE),
    share_c = round(value_c / value, 2)
  ) %>%
  ungroup() %>%
  arrange(decade, operation, desc(share_c)) %>%
  group_by(decade, operation) %>%
  slice_head(n = 2) %>%
  ungroup()

# Ranking for Arabica and Robusta by lustrum
aux_lc <- aux %>%
  group_by(country, operation, lustrum) %>%
  summarise(
    value_c = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(operation, lustrum) %>%
  mutate(
    value = sum(value_c, na.rm = TRUE),
    share_c = round(value_c / value, 2)
  ) %>%
  ungroup() %>%
  arrange(lustrum, operation, desc(share_c)) %>%
  group_by(lustrum, operation) %>%
  slice_head(n = 2) %>%
  ungroup()

# Composición de la producción de cada país y por grupo

aux1 <- aux %>%
  group_by(country, year) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

aux2 <- aux %>% # Arabica proportion by country-year
  filter(operation == "Arabica Production") %>%
  group_by(country, year) %>%
  summarise(
    arabica_value = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  left_join(aux1, by = c("country", "year")) %>%
  arrange(country, year) %>%
  mutate(
    arabica_prop = round(arabica_value / total_value, 2)
  )

ggplot(aux2 %>% left_join(data %>% 
         distinct(country, net_export_dummy), by = "country") %>%
         filter(net_export_dummy == 1), 
       aes(year, country, fill = arabica_prop)) +
  geom_tile()

arab_prop_c <- aux2 %>% # Arabica proportion by country (net Export)
  group_by(country) %>%
  summarise(
    arab_prop = mean(arabica_prop, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  left_join(data %>% distinct(country, net_export_dummy), by = "country") %>%
  filter(net_export_dummy == 1)


aux3 <- aux2 %>% # Arabica proportion by group
  mutate(
    dummy_leader = case_when(
      (country == "Brazil" | country == "Colombia" | country == "Viet Nam") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    arabica_value = sum(arabica_value, na.rm = TRUE),
    total_value = sum(total_value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    arabica_prop = round(arabica_value / total_value, 2)
  )

plot_arab_total <- aux3 %>% # Evolución de la composición por grupo
  filter(year %in% 1960:2020) %>%
  group_by(year) %>%
  summarise(arabica_value = sum(arabica_value, na.rm = TRUE),
            total_value = sum(total_value, na.rm = TRUE),
            arabica_prop = round(arabica_value / total_value, 2),
            .groups = 'drop') %>%
  ggplot(aes(x = year, y = arabica_prop)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.8) +
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

ggsave(paste0(fig_path, "arab_total.png"), 
       plot = plot_arab_total,
       width = 7, 
       height = 5,
       units = "cm",
       dpi = 300,
       bg = "white")


plot_arab_grupos <- aux3 %>% # Evolución de la composición por grupo
  filter(year %in% 1990:2020) %>%
  group_by(year, dummy_leader) %>%
  summarise(arabica_prop_mean = mean(arabica_prop, na.rm = TRUE)) %>%
  mutate(grupo = ifelse(dummy_leader == 1, "Líderes", "Seguidores")) %>%
  ggplot(aes(x = year, y = arabica_prop_mean, color = grupo, linetype = grupo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.8) +
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_color_manual(values = c("Líderes" = "black", "Seguidores" = "gray70")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

ggsave(paste0(fig_path, "arab_grupos.png"), 
       plot = plot_arab_grupos,
       width = 7, 
       height = 7,
       units = "cm",
       dpi = 300,
       bg = "white")

plot_arab_vietnam <- aux2 %>% # Evolución de la composición para Viet Nam
  filter(country == "Viet Nam", year %in% 1990:2020) %>%
  mutate(robusta_prop = 1 - arabica_prop) %>%
  ggplot(aes(x = year, y = arabica_prop)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 0.8, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(paste0(fig_path, "arab_vietnam.png"), 
       plot = plot_arab_vietnam,
       width = 7, 
       height = 5, 
       units = "cm",
       dpi = 300,
       bg = "white")

plot_arab_col <- aux2 %>% # Evolución de la composición para Colombia
  filter(country == "Colombia", year %in% 1990:2020) %>%
  mutate(robusta_prop = 1 - arabica_prop) %>%
  ggplot(aes(x = year, y = arabica_prop)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 0.8, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(paste0(fig_path, "arab_col.png"), 
       plot = plot_arab_col,
       width = 7, 
       height = 5, 
       units = "cm",
       dpi = 300,
       bg = "white")

plot_arab_br <- aux2 %>% # Evolución de la composición para Brazil
  filter(country == "Brazil", year %in% 1990:2020) %>%
  mutate(robusta_prop = 1 - arabica_prop) %>%
  ggplot(aes(x = year, y = arabica_prop)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 0.8, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(paste0(fig_path, "arab_br.png"), 
       plot = plot_arab_br,
       width = 7, 
       height = 5, 
       units = "cm",
       dpi = 300,
       bg = "white")

  

#-------------------------------------------------------------------------------
# [VF] Modelo triangular de temperatura
#-------------------------------------------------------------------------------

library(ggplot2)

crear_grafico_simple <- function(t_min, t_max, k_i, tipo) {
  # tipo = 0: sombrea área bajo el umbral
  # tipo = 1: sombrea área sobre el umbral
  
  x <- seq(0, 1, length.out = 100)
  y <- ifelse(x <= 0.5,
              t_min + (t_max - t_min) * (x / 0.5),
              t_max - (t_max - t_min) * ((x - 0.5) / 0.5))
  
  # Calcular fracción según el tipo
  if (tipo == 0) {
    # Fracción bajo el umbral
    f <- if (t_max <= k_i) 1.00 else if (t_min >= k_i) 0.00 else 
      round(0.5 * (k_i - t_min) / (t_max - t_min), 2)
  } else {
    # Fracción sobre el umbral
    f <- if (t_min >= k_i) 1.00 else if (t_max <= k_i) 0.00 else 
      round(1 - (0.5 * (k_i - t_min) / (t_max - t_min)), 2)
  }
  
  # Crear el gráfico base
  p <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_hline(yintercept = k_i, linetype = "dashed", color = "black") +
    geom_line(color = "black") +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "½", "1")) +
    labs(x = "Fracción del día", y = "°C") +
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
  
  # Agregar el sombreado según el tipo
  if (tipo == 0) {
    # Sombreado bajo el umbral
    p <- p + geom_ribbon(aes(ymin = ifelse(y <= k_i, min(y), NA), 
                             ymax = ifelse(y <= k_i, y, NA)), 
                         fill = "#F0F0F0")
  } else {
    # Sombreado sobre el umbral
    p <- p + geom_ribbon(aes(ymin = ifelse(y >= k_i, k_i, NA), 
                             ymax = ifelse(y >= k_i, y, NA)), 
                         fill = "#F0F0F0")
  }
  
  return(p)
}

# Crear y mostrar
p1 <- crear_grafico_simple(2, 8, 10, 0)
p2 <- crear_grafico_simple(7, 13, 10, 0)
p3 <- crear_grafico_simple(12, 18, 10, 0)
p4 <- crear_grafico_simple(22, 28, 30, 1)
p5 <- crear_grafico_simple(27, 33, 30, 1)
p6 <- crear_grafico_simple(32, 38, 30, 1)
print(p1); print(p2); print(p3); print(p4); print(p5); print(p6)

ggsave(
  filename = paste0(fig_path, "exp_p1.png"),
  plot = p1,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "exp_p2.png"),
  plot = p2,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "exp_p3.png"),
  plot = p3,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "exp_p4.png"),
  plot = p4,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "exp_p5.png"),
  plot = p5,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

ggsave(
  filename = paste0(fig_path, "exp_p6.png"),
  plot = p6,
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  bg = "white")

#-----
# [VF] Test T
#-----
aux_yc <- results_yc %>%
  filter(scenario %in% c("CF0", "Stackelberg CF2")) %>%
  select(market_id, country, scenario, quantity) %>%
  pivot_wider(
    names_from = scenario,
    values_from = quantity,
    names_prefix = "quantity_"
  ) %>%
  mutate(
    delta_quantity = `quantity_Stackelberg CF2` - `quantity_CF0`
  )

t_test_delta <- t.test(aux_yc$delta_quantity, mu = 0)
print(t_test_delta)


#-------------------------------------------------------------------------------
# [VF] Cambio en heatExp por país
#-------------------------------------------------------------------------------

aux <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1, year %in% 1970:1989) %>%
  select(country, year, heatExp_yc) %>%
  group_by(country) %>%
  summarise(
    mean_heat = mean(heatExp_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

aux1 <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1, year %in% 1990:2019) %>%
  select(country, year, heatExp_yc) %>%
  group_by(country) %>%
  summarise(
    heat_factual = mean(heatExp_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

aux <- aux %>%
  left_join(aux1, by = c("country")) %>%
  mutate(
    diff_heat =  heat_factual / mean_heat - 1
  )

x <- aux %>% pull(country)

country_interest <- c("Angola", "Benin", "Bolivia", "Brazil", "Burundi", 
                      "Cameroon", "Colombia", "Congo", "Costa Rica", "Cuba",
                      "Dominican Republic", "Ecuador", "El Salvador", "Ethiopia",
                      "Gabon", "Ghana", "Guatemala", "Guinea", "Guyana", "Haiti",
                      "Honduras", "India", "Indonesia", "Jamaica", "Kenya",
                      "Liberia", "Madagascar", "Malawi", "Mexico", "Nicaragua",
                      "Nigeria", "Panama", "Papua New Guinea", "Paraguay", "Peru",
                      "Rwanda", "Sierra Leone", "Sri Lanka", "Togo", 
                      "Trinidad and Tobago", "Uganda", "Venezuela", "Yemen",
                      "Zambia", "Zimbabwe") 

country_final <- intersect(country_interest, country_plots)

datos_mapa <- aux %>%
  mutate(
    country_clean = case_when(
      # Países con nombres idénticos o muy similares
      country %in% country_final ~ country,
      
      # Países con nombres diferentes en y
      country == "Vietnam" ~ "Viet Nam",
      country == "Côte d'Ivoire" ~ "Côte d'Ivoire",  # Ya es igual
      country == "Eq. Guinea" ~ "Equatorial Guinea",

      # Para cualquier otro país no en la lista, mantener NA o el original
      TRUE ~ NA_character_
    ),
    iso_a3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  ) %>%
  filter(!is.na(iso_a3))

# 2. OBTENER MAPA DEL MUNDO
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry)

y <- world %>% pull(name)

# 3. UNIR DATOS CON MAPA
world_data <- world %>%
  left_join(datos_mapa, by = "iso_a3")

world_data <- world_data %>%
  mutate(
    diff_heat_percentil = case_when(
      diff_heat <= quantile(diff_heat, 0.25, na.rm = TRUE) ~ "0-25%",
      diff_heat <= quantile(diff_heat, 0.50, na.rm = TRUE) ~ "25-50%",
      diff_heat <= quantile(diff_heat, 0.75, na.rm = TRUE) ~ "50-75%",
      diff_heat <= quantile(diff_heat, 1.00, na.rm = TRUE) ~ "75-100%",
      TRUE ~ NA_character_
    ),
    # Convertir a factor ordenado
    diff_heat_percentil = factor(diff_heat_percentil,
                                 levels = c("0-25%", "25-50%", "50-75%", "75-100%"))
  )

# Verificar los cortes
quantiles <- quantile(world_data$diff_heat, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
cat("Percentiles 0%, 25%, 50%, 75%, 100%:\n")
print(quantiles)

# Gráfico con categorías de percentiles
mapa <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70", size = 0.1) +
  # Líneas geográficas
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 23.5, color = "blue", linetype = "dotted", linewidth = 0.4, alpha = 0.6) +
  geom_hline(yintercept = -23.5, color = "blue", linetype = "dotted", linewidth = 0.4, alpha = 0.6) +
  # Datos
  geom_sf(data = world_data %>% filter(!is.na(diff_heat_percentil)), 
          aes(fill = diff_heat_percentil), color = "gray40", size = 0.2) +
  scale_fill_viridis_d(
    name = NULL,
    option = "plasma",
    direction = -1,
    na.value = NA,
    drop = FALSE,
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.4, "cm")
    )
  ) +
  coord_sf(ylim = c(-60, 60)) +  # Asegura que se vean las líneas
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )
# 5. MOSTRAR Y GUARDAR
print(mapa)


# Guardar en alta resolución
ggsave(paste0(fig_path, "mapa_estres_termico_cafe.png"), 
       plot = mapa,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# 6. TABLA COMPLEMENTARIA: TOP 10 PAÍSES MÁS AFECTADOS
top_10 <- datos_mapa %>%
  arrange(desc(diff_heat)) %>%
  head(20) %>%
  select(País = country, 
         `Días adicionales` = diff_heat) %>%
  mutate(`Días adicionales` = round(`Días adicionales`, 1))

cat("\nTOP 10 PAÍSES CON MAYOR AUMENTO DE ESTRÉS TÉRMICO:\n")
print(top_10)

# También puedes guardar esta tabla
write_csv(top_10, "top10_estres_termico.csv")


#-------------------------------------------------------------------------------
# [VF] Cambios en summary_c
#-------------------------------------------------------------------------------

aux <- summary_c %>%
  select(country, dummy_leader, `profit_CF0`, `profit_CF1`, 
         `quantity_CF0`, `quantity_CF1`,
         `mc_CF0`, `mc_CF1`) %>%
  mutate(
    diff_profit = round(`profit_CF0` / `profit_CF1` - 1, 4),
    diff_quantity = round(`quantity_CF0` / `quantity_CF1` - 1, 4),
    diff_cost = round(`mc_CF0` / `mc_CF1` - 1, 4),
    
    country = factor(country, levels = country),
    
    grupo_profit = case_when(
      diff_profit > 0 ~ "Positivo",
      diff_profit < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    ),
    grupo_quantity = case_when(
      diff_quantity > 0 ~ "Positivo",
      diff_quantity < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    ),
    grupo_cost = case_when(
      diff_cost > 0 ~ "Positivo",
      diff_cost < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    )
  )

# Se omiten casos con menos de 25 años de información entre 1990 y 2019
aux1 <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1, year %in% 1990:2019) %>%
  group_by(country) %>%
  summarise(
    n_obs = sum(!is.na(qe_yc)),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  filter(n_obs > 25) %>%
  pull(country)

aux <- aux %>%
  filter(country %in% aux1, diff_profit < 4, diff_profit > -4) # Se omiten casos extremos (diff_profit 1000%)

country_plots <- aux %>%
  mutate(country = as.character(country)) %>%
  pull(country)

# Plot diferencias

# Profit
plot_profit <- ggplot(aux %>%
                        filter(country %in% country_plots), aes(x = country, y = diff_profit)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_profit), size = 3, stroke = 1) +
  # Líneas de conexión al cero (opcional, ayuda a la lectura)
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_profit), 
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),  # Más margen superior
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)  # Más espacio abajo
  )

print(plot_profit)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_profit.png"), 
       plot = plot_profit,
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Quantity
plot_quantity_ld <- ggplot(aux %>% filter(country %in% c("Brazil", "Colombia", "Viet Nam")), aes(x = country, y = diff_quantity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_quantity), size = 3, stroke = 1) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_quantity),
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  # AGREGAR ETIQUETAS DE VALOR
  geom_text(aes(label = scales::percent(diff_quantity, accuracy = 0.1)), 
            vjust = -1.5, size = 3) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.4,1.3)) +
  labs(
    x = NULL,
    y = NULL,  # Eliminada etiqueta del eje Y
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),  # ELIMINADO
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)
  )

print(plot_quantity_ld)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_quantity_leaders.png"), 
       plot = plot_quantity_ld,
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Quantity only followers
plot_quantity_fl <- ggplot(aux %>% filter(!country %in% c("Brazil", "Colombia", "Viet Nam")), aes(x = country, y = diff_quantity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_quantity), size = 3, stroke = 1) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_quantity),
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.1,0.1)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),  # Más margen superior
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)  # Más espacio abajo
  )

print(plot_quantity_fl)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_quantity_followers.png"), 
       plot = plot_quantity_fl,
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Costs
plot_costs <- ggplot(aux, aes(x = country, y = diff_cost)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_cost), size = 3, stroke = 1) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_cost),
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.3,0.6)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),  # Más margen superior
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)  # Más espacio abajo
  )

print(plot_costs)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_cost.png"), 
       plot = plot_costs,
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

#-------------------------------------------------------------------------------
# [VF] Cambios en market share
#-------------------------------------------------------------------------------

aux_cf0 <- results_yc %>%
  filter(scenario == "CF0") %>%
  mutate(
    qe = sum(quantity, na.rm = TRUE)
  ) %>%
  group_by(country) %>%
  mutate(
    qe_c = sum(quantity, na.rm = TRUE)
    ) %>%
  ungroup() %>%
  distinct(country, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarise(
    share_c_cf0 = round(qe_c / qe, 4),
    .groups = 'drop'
  ) %>%
  ungroup()

aux_cf1 <- results_yc %>%
  filter(scenario == "CF1") %>%
  mutate(
    qe = sum(quantity, na.rm = TRUE)
  ) %>%
  group_by(country) %>%
  mutate(
    qe_c = sum(quantity, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(country, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarise(
    share_c_cf1 = round(qe_c / qe, 4),
    .groups = 'drop'
  ) %>%
  ungroup()

aux_share <- aux_cf0 %>% left_join(aux_cf1, by = "country") %>%
  mutate(
    diff = round(share_c_cf0 - share_c_cf1, 4)
  ) %>%
  mutate(
    grupo_share = case_when(
      diff > 0 ~ "Positivo",
      diff < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    )
  )
  

ggplot(aux_share %>% filter(!country %in% c("Brazil", "Colombia", "Viet Nam")), aes(x = country, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_share), size = 3, stroke = 1) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff),
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.01,0.01)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),  # Más margen superior
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)  # Más espacio abajo
  )

# aux1 <- data %>%
#   filter(dummy_yc == 1, net_export_dummy == 1) %>%
#   mutate(
#     qe = sum(qe_yc, na.rm = TRUE)
#   ) %>%
#   group_by(country) %>%
#   mutate(
#     qe_c = sum(qe_yc, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   distinct(country, .keep_all = TRUE) %>%
#   group_by(country) %>%
#   summarise(
#     share_c_cf0 = round(qe_c / qe, 2),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
  
#-------------------------------------------------------------------------------
# [VF] Cambio en costos marginales
#-------------------------------------------------------------------------------

aux_cf0 <- results_yc %>%
  filter(scenario == "CF0") %>%
  group_by(country) %>%
  summarise(
    costs_cf0 = mean(mc_yc_hat_s, na.rm = TRUE),
    dummy_leader = first(dummy_leader),
    .groups = 'drop'
  ) %>%
  ungroup()

aux_cf1 <- results_yc %>%
  filter(scenario == "CF1") %>%
  group_by(country) %>%
  summarise(
    costs_cf1 = mean(mc_yc_hat_s, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

aux_costs <- aux_cf0 %>% left_join(aux_cf1, by = "country") %>%
  mutate(
    diff = round(costs_cf1 / costs_cf0 - 1, 4)
  )

# Plot

# Filtrar los datos para los países seleccionados
datos_filtrados <- results_yc %>%
  filter(scenario == "CF1", country %in% c("Brazil", "Colombia", "Viet Nam"))

# Crear el gráfico
ggplot(datos_filtrados, aes(x = date, y = mc_yc_hat_s, color = country)) +
  geom_line(size = 1) +  # Líneas para mostrar la evolución
  geom_point(size = 2) + # Puntos en cada año
  labs(
    title = "Evolución de mc_yc_hat_s",
    x = "Año",
    y = "mc_yc_hat_s",
    color = "País"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("Brazil" = "blue", "Colombia" = "green", "Viet Nam" = "red")
  )
  
#-------------------------------------------------------------------------------
# [VF] Cambio en heatExp
#-------------------------------------------------------------------------------
aux_heat <- data_cf %>%
  select(year, country, dummy_leader, heatExp_yc, heatExp_yc_cf) %>%
  group_by(country) %>%
  summarise(
    dummy_leader = first(dummy_leader),
    heatExp_c = mean(heatExp_yc, na.rm = TRUE),
    heatExp_cf_c = mean(heatExp_yc_cf, na.rm = TRUE),
    diff = heatExp_c / heatExp_cf_c - 1,
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    country = factor(country, levels = country),
    
    grupo_heat = case_when(
      diff > 0 ~ "Positivo",
      diff < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    )
  ) %>%
  filter(country %in% country_plots)

# Heat
plot_heat <- ggplot(aux_heat, aes(x = country, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_point(aes(shape = grupo_heat), size = 3, stroke = 1) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff),
               color = "gray70", linewidth = 0.2, alpha = 0.5) +
  scale_shape_manual(values = c("Negativo" = 1, "Positivo" = 16, "Neutro" = 4)) +
  scale_y_continuous(labels = scales::percent) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # TEXTO VERTICAL (90 grados)
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),  # Más margen superior
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "none",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    # Ajustar márgenes para dar espacio al texto vertical
    plot.margin = margin(10, 10, 20, 10)  # Más espacio abajo
  )

print(plot_heat)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_heat.png"), 
       plot = plot_heat,
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

#-------------------------------------------------------------------------------
# [VF] Descomposición del efecto
#-------------------------------------------------------------------------------

descomp_yc <- results_yc %>%
  filter(scenario == "CF0" | scenario == "CF1") %>%
  select(date, country, quantity, costs = mc_yc_hat_s, price, profit, scenario) %>%
  pivot_wider(
    id_cols = c(date, country),
    names_from = scenario,
    values_from = c(quantity, price, costs, profit),
    names_sep = "_"
  ) %>%
  mutate(
    conversion = (60 * 2.20462) / 100,
    
    total_effect = profit_CF1 - profit_CF0,
    
    direct_effect = (costs_CF0 - costs_CF1) * quantity_CF0 * conversion,

    strategic_effect = total_effect - direct_effect,

    abs_total = abs(direct_effect) + abs(strategic_effect),
    
    direct_prop = ifelse(abs_total > 0, abs(direct_effect) / abs_total, NA),
    
    strategic_prop = ifelse(abs_total > 0, abs(strategic_effect) / abs_total, NA),
  )

descomp_c <- descomp_yc %>%
  group_by(country) %>%
  summarise(
    total_effect = sum(total_effect, na.rm = TRUE),
    direct_effect = sum(direct_effect, na.rm = TRUE),
    strategic_effect = sum(strategic_effect, na.rm = TRUE),
    abs_effect = abs(direct_effect) + abs(strategic_effect),
    efficiency_prop = round(abs(direct_effect) / abs_effect, 4),
    strategic_prop = round(abs(strategic_effect) / abs_effect, 4),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  filter(country %in% country_plots)

descomp_g <- descomp_c %>%
  left_join(data %>% distinct(country, dummy_leader), by = "country") %>%
  mutate(dummy_leader = if_else(dummy_leader == 1, "Líderes", "Seguidores")) %>%
  group_by(dummy_leader) %>%
  summarise(
    total_effect = sum(total_effect, na.rm = TRUE),
    direct_effect = sum(direct_effect, na.rm = TRUE),
    strategic_effect = sum(strategic_effect, na.rm = TRUE),
    abs_effect = abs(direct_effect) + abs(strategic_effect),
    efficiency_prop = round(abs(direct_effect) / abs_effect, 4),
    strategic_prop = round(abs(strategic_effect) / abs_effect, 4),
    .groups = 'drop'
  ) %>%
  ungroup()

descomp_c <- descomp_c %>%
  left_join(data %>% distinct(country, dummy_leader), by = "country")

# Cuadrantes Líderes
plot_descomp_ld <- ggplot(descomp_c %>%
                           filter(country %in% c("Viet Nam", "Brazil", "Colombia")), 
                         aes(x = direct_effect, 
                             y = strategic_effect,
                             shape = factor(dummy_leader,
                                            levels = c(1, 0),
                                            labels = c("Líder", "Seguidor")))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_abline(slope = -1, intercept = 0, linetype = "dotted", color = "black") +
  
  #Etiquetar solo los países mencionados en el texto
  geom_text_repel(
    data = descomp_c %>%
      mutate(country = if_else(country == "Viet Nam", "Vietnam", country)) %>%
      filter(country %in% c("Vietnam", "Brazil", "Colombia")),
    aes(label = country),
    size = 3.5,
    box.padding = 0.5,
    segment.color = "gray50"
  ) +

  labs(
    x = "Efecto Eficiencia (mill. USD)",
    y = "Efecto Estratégico (mill. USD)",
    shape = ""
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = paste0(fig_path, "plot_descomp_ld.png"),
  plot = plot_descomp_ld,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white")

# Cuadrantes Seguidores
plot_descomp_fl <- ggplot(descomp_c %>%
                           filter(!country %in% c("Viet Nam", "Brazil", "Colombia", "Cuba", "Côte d'Ivoire")), 
                         aes(x = direct_effect, 
                             y = strategic_effect,
                             shape = factor(dummy_leader,
                                            levels = c(1, 0),
                                            labels = c("Líder", "Seguidor")))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_abline(slope = -1, intercept = 0, linetype = "dotted", color = "black") +
  
  #Etiquetar solo los países mencionados en el texto
  # geom_text_repel(
  #   data = descomp_c %>%
  #     filter(country %in% c("Côte d'Ivoire", "Cuba")),
  #   aes(label = country),
  #   size = 3.5,
  #   box.padding = 0.5,
  #   segment.color = "gray50"
  # ) +
  
  labs(
    x = "Efecto Eficiencia (mill. USD)",
    y = "Efecto Estratégico (mill. USD)",
    shape = ""
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = paste0(fig_path, "plot_descomp_fl.png"),
  plot = plot_descomp_fl,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white")

# Ranking Top Pérdida
plot_ranking_losers <- descomp_c %>%
  arrange(desc(total_effect)) %>%
  slice(1:10) %>%
  select(country, dummy_leader, direct_effect, strategic_effect, efficiency_prop, strategic_prop) %>%
  mutate(
    country = forcats::fct_reorder(country, direct_effect + strategic_effect),
    Type = ifelse(dummy_leader == 1, "Líder", "Seguidor"),
    pct_direct = efficiency_prop,
    pct_strategic = strategic_prop
  ) %>%
  pivot_longer(cols = c(direct_effect, strategic_effect),
               names_to = "Efecto", values_to = "Valor") %>%
  mutate(
    Efecto = recode(Efecto,
                    "direct_effect" = "Efecto directo",
                    "strategic_effect" = "Efecto estratégico"),
    # Crear etiqueta de porcentaje
    pct = ifelse(Efecto == "Efecto directo", pct_direct, pct_strategic),
    pct_label = paste0(round(pct * 100, 0), "%")
  ) %>%
  ggplot(aes(x = country, y = Valor, fill = Efecto)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Efecto directo" = "#BE988C", "Efecto estratégico" = "#C9D6C3")
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Pérdida total (MM USD)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot_ranking_losers)

ggsave(
  filename = paste0(fig_path, "plot_ranking_losers.png"),
  plot = plot_ranking_losers,
  width = 9,
  height = 6,
  dpi = 600,
  bg = "white")

# Ranking Top Win
plot_ranking_winners <- descomp_c %>%
  arrange(desc(total_effect)) %>%
  slice_tail(n=5) %>%
  select(country, dummy_leader, direct_effect, strategic_effect, efficiency_prop, strategic_prop) %>%
  mutate(
    #strategic_effect = -strategic_effect,
    
    #efficiency_prop = -efficiency_prop,
    
    country = forcats::fct_reorder(country, direct_effect + strategic_effect),
    Type = ifelse(dummy_leader == 1, "Líder", "Seguidor"),
    pct_direct = efficiency_prop,
    pct_strategic = strategic_prop
  ) %>%
  pivot_longer(cols = c(direct_effect, strategic_effect),
               names_to = "Efecto", values_to = "Valor") %>%
  mutate(
    Efecto = recode(Efecto,
                    "direct_effect" = "Efecto directo",
                    "strategic_effect" = "Efecto estratégico"),
    # Crear etiqueta de porcentaje
    pct = ifelse(Efecto == "Efecto directo", pct_direct, pct_strategic),
    pct_label = paste0(round(pct * 100, 0), "%")
  ) %>%
  ggplot(aes(x = country, y = Valor, fill = Efecto)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes( label = pct_label),  # Extremo derecho
    hjust = -0.1,  # Ajusta para separar del borde
    color = "black",
    size = 3
  )+
  coord_flip() +
  scale_fill_manual(
    values = c("Efecto eficiencia" = "#BE988C", "Efecto estratégico" = "#C9D6C3")
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Ganancia total (MM USD)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot_ranking_winners)

ggsave(
  filename = paste0(fig_path, "plot_ranking_winners.png"),
  plot = plot_ranking_winners,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white")

# Distribución de proporciones

plot_violin <- descomp_c %>%
  select(country, dummy_leader, efficiency_prop, strategic_prop) %>%
  pivot_longer(cols = c(efficiency_prop, strategic_prop),
               names_to = "Componente", values_to = "Proporción") %>%
  mutate(
    Type = ifelse(dummy_leader == 1, "Líderes", "Seguidores"),
    Componente = recode(Componente,
                        "efficiency_prop" = "Proporción: Efecto directo",
                        "strategic_prop" = "Proporción: Efecto estratégico")
  ) %>%
  ggplot(aes(x = Type, y = Proporción, fill = Type)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  facet_wrap(~Componente) +
  scale_fill_manual(values = c("Líderes" = "#d73027", "Seguidores" = "#4575b4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribución de la composición de pérdidas",
    subtitle = "¿Qué tan importante es cada efecto? Comparación líderes vs seguidores",
    x = NULL,
    y = "Proporción del efecto total (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

# Ganadores estratégicos

library(kableExtra)

tabla_winners <- descomp_c %>%
  filter(strategic_effect < 0) %>%  # Ganancia estratégica
  arrange(strategic_effect) %>%
  mutate(
    Type = ifelse(dummy_leader == 1, "Líder", "Seguidor"),
    net_effect = total_effect,
    gain_ratio = abs(strategic_effect / direct_effect)
  ) %>%
  select(Country = country, Type, 
         `Efecto directo` = direct_effect,
         `Efecto estratégico` = strategic_effect,
         `Efecto neto` = net_effect,
         `Ratio ganancia` = gain_ratio) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kbl(
    caption = "Países con ganancia estratégica por cambio climático",
    booktabs = TRUE,
    format = "latex",
    align = c("l", "l", rep("r", 4))
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 10
  ) %>%
  footnote(
    general = "Ratio ganancia = |Efecto estratégico| / Efecto directo. Un ratio > 1 indica que la ganancia estratégica supera la pérdida directa.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(tabla_winners), paste0(tab_path, "tabla_winners.tex"))

# Concentración de pérdidas
library(ineq)

# Curva de Lorenz de pérdidas
descomp_c_sorted <- descomp_c %>%
  arrange(total_effect) %>%
  mutate(
    cumulative_countries = row_number() / n(),
    cumulative_loss = cumsum(total_effect) / sum(total_effect, na.rm = TRUE)
  )

gini_loss <- ineq(descomp_c$total_effect, type = "Gini")

plot_lorenz <- ggplot(descomp_c_sorted, 
                      aes(x = cumulative_countries, y = cumulative_loss)) +
  geom_line(linewidth = 1.2, color = "#d73027") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.3, y = 0.8, 
           label = paste0("Gini = ", round(gini_loss, 3)),
           size = 5, fontface = "bold") +
  labs(
    title = "Concentración de pérdidas por cambio climático",
    subtitle = "Curva de Lorenz: ¿Las pérdidas están concentradas en pocos países?",
    x = "Proporción acumulada de países",
    y = "Proporción acumulada de pérdidas"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(paste0(ruta_figures, "lorenz_losses.png"), 
       plot_lorenz, width = 8, height = 8)


# Preparar datos en formato largo para ggplot
decomp_long <- descomp_g %>%
  mutate(
    dummy_leader = if_else(dummy_leader == 1, "Líderes", "Seguidores")
  ) %>%
  pivot_longer(
    cols = c(direct_effect, strategic_effect),
    names_to = "effect_type",
    values_to = "effect_value"
  )

# Gráfico para LÍDERES vs SEGUIDORES (si tienes columna 'group')
ggplot(decomp_long %>% filter(!is.na(dummy_leader)), 
       aes(x = dummy_leader, y = effect_value, fill = effect_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("direct_effect" = "black", 
               "strategic_effect" = "gray70"),
    labels = c("Eficiencia", "Efecto Estratégico")
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "mill. USD",
    fill = NULL
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

# hEATMAP

descomp_c <- descomp_c %>%
  mutate(
    vulnerability_class = case_when(
      total_effect > quantile(total_effect, 0.75, na.rm = TRUE) ~ "Muy vulnerable",
      total_effect > quantile(total_effect, 0.5, na.rm = TRUE) ~ "Vulnerable",
      total_effect > quantile(total_effect, 0.25, na.rm = TRUE) ~ "Moderado",
      TRUE ~ "Poco vulnerable"
    ),
    strategic_class = case_when(
      strategic_effect > 0 & efficiency_prop > 0.7 ~ "Pérdida por costos",
      strategic_effect > 0 & efficiency_prop <= 0.7 ~ "Pérdida mixta",
      strategic_effect < 0 ~ "Ganancia estratégica",
      TRUE ~ "Neutro"
    )
  )

# Top 15 países más afectados
plot_top15 <- descomp_c %>%
  arrange(desc(total_effect)) %>%
  slice(1:15) %>%
  select(country, direct_effect, strategic_effect) %>%
  pivot_longer(cols = c(direct_effect, strategic_effect),
               names_to = "Efecto", values_to = "Valor") %>%
  mutate(
    Efecto = recode(Efecto,
                    "direct_effect" = "Efecto directo",
                    "strategic_effect" = "Efecto estratégico"),
    country = forcats::fct_reorder(country, Valor, .fun = sum)
  ) %>%
  ggplot(aes(x = Efecto, y = country, fill = Valor)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Valor, 1)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b",
    midpoint = 0,
    name = "Pérdida\n(MM USD)"
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  filename = paste0(fig_path, "plot_top15.png"),
  plot = plot_top15,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white")


#-------------------------------------------------------------------------------
# [] Motivación
#-------------------------------------------------------------------------------
data_yc <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1) %>%
  select(year, country, price_y, qe_yc, qe_y, 
         heatExp_yc, optExp_yc, frostExp_yc, tas_yce) %>%
  arrange(country, year) %>%
  mutate(
    log_qe_yc = log(qe_yc + 1),
    lag1_qe_yc = dplyr::lag(qe_yc, 1),
    revenue_yc = price_y * qe_yc,
    lag1_rev_yc = dplyr::lag(revenue_yc, 1),
    log_rev_yc = log(revenue_yc + 1),
    share_yc = 100 * (qe_yc / qe_y),
    lag1_sh_yc = dplyr::lag(share_yc, 1),
    
    log_heat_yc = log(heatExp_yc),
    lag1_heat_yc = dplyr::lag(heatExp_yc, 1),
    log_lag1_heat_yc = dplyr::lag(lag1_heat_yc, 1),
    log_frost_yc = log(frostExp_yc),
    lag1_frost_yc = dplyr::lag(frostExp_yc, 1),
    log_lag1_frost_yc = dplyr::lag(lag1_frost_yc, 1),
    log_opt_yc = log(optExp_yc),
    lag1_opt_yc = dplyr::lag(optExp_yc, 1),
    log_lag1_opt_yc = dplyr::lag(lag1_opt_yc, 1),
    log_tas_yc = log(tas_yce),
    lag1_tas_yc = dplyr::lag(tas_yce, 1),
    log_lag1_tas_yc = dplyr::lag(lag1_tas_yc, 1)
  ) %>%
  group_by(country) %>%
  mutate(
    diff_qe_yc = qe_yc - lag1_qe_yc,
    diff_rev_yc = revenue_yc - lag1_rev_yc,
    diff_sh_yc = share_yc - lag1_sh_yc,
    
    diff_heat_yc = heatExp_yc - lag1_heat_yc,
    diff_opt_yc = optExp_yc - lag1_opt_yc,
    diff_tas_yc = tas_yce - lag1_tas_yc
  ) %>%
  ungroup() %>%
  drop_na()

m1 <- lm(
  log_qe_yc ~
    tas_yce + I(tas_yce^2),
  data = data_yc
)

summary(m1)

m2 <- lm(
  log_rev_yc ~
    tas_yce + I(tas_yce^2),
  data = data_yc
)

summary(m2)

m3 <- lm(
  share_yc ~
    tas_yce + I(tas_yce^2),
  data = data_yc
)

summary(m3)

m1_fe <- feols(
  log_qe_yc ~
    tas_yce + I(tas_yce^2)
  | country + year,
  data = data_yc,
  cluster = ~country
)

summary(m1_fe)

m2_fe <- feols(
  log_rev_yc ~
    tas_yce + I(tas_yce^2)
    | country + year,
  data = data_yc,
  cluster = ~country
)

summary(m2_fe)

m3_fe <- feols(
  share_yc ~
    tas_yce + I(tas_yce^2)
    | country + year,
  data = data_yc,
  cluster = ~country
)

summary(m3_fe)

#-------------------------------------------------------------------------------
# Heatmap heat
#-------------------------------------------------------------------------------
data_yc <- data %>%
  filter(dummy_yc == 1) %>%
  mutate(
    share_yc = qe_yc / qe_y,
    rev_yc = qe_yc * price_y,
    grupo = case_when(
      country %in% c("Brazil", "Colombia", "Vietnam") ~ "Líder",
      TRUE ~ "Seguidor"
    )
  ) %>%
  select(year, country, qe_yc, qe_y, rev_yc, grupo, tas_yce, heatExp_yc, share_yc) %>%
  drop_na() %>%
  mutate(
    country_clean = case_when(
      # Países con nombres idénticos o muy similares
      country %in% c("Angola", "Benin", "Bolivia", "Brazil", "Burundi", 
                     "Cameroon", "Colombia", "Congo", "Costa Rica", "Cuba",
                     "Dominican Republic", "Ecuador", "El Salvador", "Ethiopia",
                     "Gabon", "Ghana", "Guatemala", "Guinea", "Guyana", "Haiti",
                     "Honduras", "India", "Indonesia", "Jamaica", "Kenya",
                     "Liberia", "Madagascar", "Malawi", "Mexico", "Nicaragua",
                     "Nigeria", "Panama", "Papua New Guinea", "Paraguay", "Peru",
                     "Rwanda", "Sierra Leone", "Sri Lanka", "Togo", 
                     "Trinidad and Tobago", "Uganda", "Venezuela", "Yemen",
                     "Zambia", "Zimbabwe") ~ country,
      
      # Países con nombres diferentes en y
      country == "Vietnam" ~ "Viet Nam",
      country == "Côte d'Ivoire" ~ "Côte d'Ivoire",  # Ya es igual
      country == "Eq. Guinea" ~ "Equatorial Guinea",
      country == "Central African Rep." ~ "Central African Republic",
      country == "Laos" ~ "Laos",  # Ya es igual
      country == "Tanzania" ~ "Tanzania",  # Ya es igual
      country == "New Caledonia" ~ "New Caledonia",  # Ya es igual
      
      # Para cualquier otro país no en la lista, mantener NA o el original
      TRUE ~ NA_character_
    ),
    iso_a3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  ) %>%
  filter(!is.na(iso_a3))

ggplot(data_yc, aes(year, country, fill= tas_yce)) + 
  geom_tile()

# Curva térmica qe_yc

motiv1 <- ggplot(data_yc %>% filter(country != "Bolivia"), aes(x = tas_yce, y = log(qe_yc + 1), color = grupo)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(group = 1)) +
  labs(
    x = "Temperatura promedio anual (°C)",
    y = "Log(Cantidad exportada)",
    title = NULL,
    subtitle = NULL,
    color = NULL
  ) +
  theme_classic()

ggsave(
  filename = paste0(fig_path, "motiv1.png"),
  plot = motiv1,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white")

# Curva térmica rev_yc

motiv2 <- ggplot(data_yc %>% filter(country != "Bolivia"), aes(x = tas_yce, y = log(rev_yc + 1), color = grupo)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(group = 1)) +
  labs(
    x = "Temperatura promedio anual (°C)",
    y = "Log(Ingresos)",
    title = NULL,
    subtitle = NULL,
    color = NULL
  ) +
  theme_classic()

ggsave(
  filename = paste0(fig_path, "motiv2.png"),
  plot = motiv2,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white")

# Curva térmica share_yc

motiv3 <- ggplot(data_yc %>% filter(country != "Bolivia"), aes(x = tas_yce, y = share_yc, color = grupo)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(group = 1)) +
  labs(
    x = "Temperatura promedio anual (°C)",
    y = "Participación de mercado",
    title = NULL,
    subtitle = NULL,
    color = NULL
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.4)) +
  theme_classic()

ggsave(
  filename = paste0(fig_path, "motiv3.png"),
  plot = motiv3,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white")

map_data <- data_yc %>%
  filter(year >= 1990) %>%
  group_by(iso_a3) %>%
  summarise(
    country = first(country_clean),
    temp_mean = mean(tas_yce, na.rm = TRUE),
    share_mean = sum(qe_yc, na.rm = TRUE) / sum(qe_y, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    temp_group = if_else(temp_mean >= 20 & temp_mean <= 25, 
                         "20-25°C", 
                         "Otro")
  )

# Obtener datos de mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% 
  left_join(map_data, by = "iso_a3")

# Gráfico principal
ggplot(world) +
  geom_sf(aes(fill = temp_group), color = "gray40", size = 0.2) +
  geom_point(data = world %>% filter(!is.na(share_mean)),
             aes(geometry = geometry, size = share_mean),
             stat = "sf_coordinates", alpha = 0.7) +
  scale_fill_manual(
    values = c("20-25°C" = "#2E8B57",  # Verde
               "Otro" = "#B0C4DE"),       # Gris azulado
    name = "Temperatura promedio",
    na.value = "gray90"
  ) +
  scale_size_continuous(
    name = "Participación en exportaciones",
    range = c(1, 12),
    #breaks = c(0.1, 0.3, 0.5),
    labels = scales::percent
  ) +
  labs(
    title = NULL,
    subtitle = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14)
  )

data_yc %>%
  group_by(year) %>%
  summarise(temp_global = mean(tas_yce, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = temp_global)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 1989, linetype = "dashed", color = "red") +
  geom_smooth(data = . %>% filter(year < 1989), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = . %>% filter(year >= 1989), method = "lm", se = FALSE, color = "red") +
  labs(x = "Año", y = "Temperatura promedio global (°C)",
       title = "Cambio en tendencia térmica alrededor de 1989") +
  theme_minimal()

#-------------------------------------------------------------------------------
# [] Test de Stackelberg
#-------------------------------------------------------------------------------

data_yc <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1) %>%
  arrange(country, year) %>%
  select(year, country, net_export_dummy, dummy_leader, price_y, qe_yc,
         heatExp_yc) %>%
  drop_na()

# Variables de líderes
aux_br <- data_yc %>%
  filter(country == "Brazil") %>%
  mutate(
    qe_br_lag1 = dplyr::lag(qe_yc, 1),
    diff_qe_br = qe_yc - qe_br_lag1,
    heat_br_lag1 = dplyr::lag(heatExp_yc, 1),
    diff_heat_br = heatExp_yc - heat_br_lag1
  ) %>%
  select(year, qe_br_lag1, diff_qe_br, heat_br_lag1, diff_heat_br)

aux_co <- data_yc %>%
  filter(country == "Colombia") %>%
  mutate(
    qe_co_lag1 = dplyr::lag(qe_yc, 1),
    diff_qe_co = qe_yc - qe_co_lag1,
    heat_co_lag1 = dplyr::lag(heatExp_yc, 1),
    diff_heat_co = heatExp_yc - heat_co_lag1
  ) %>%
  select(year, qe_co_lag1, diff_qe_co, heat_co_lag1, diff_heat_co)

aux_vt <- data_yc %>%
  filter(country == "Viet Nam") %>%
  mutate(
    qe_vt_lag1 = dplyr::lag(qe_yc, 1),
    diff_qe_vt = qe_yc - qe_vt_lag1,
    heat_vt_lag1 = dplyr::lag(heatExp_yc, 1),
    diff_heat_vt = heatExp_yc - heat_vt_lag1
  ) %>%
  select(year, qe_vt_lag1, diff_qe_vt, heat_vt_lag1, diff_heat_vt)
  
# Variables para seguidores
aux_fl <- data_yc %>%
  filter(dummy_leader == 0) %>%
  group_by(year) %>%
  summarise(
    qe_fl = mean(qe_yc, na.rm = TRUE),
    heat_fl = mean(heatExp_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    qe_fl_lag1 = dplyr::lag(qe_fl),
    diff_qe_fl = qe_fl - qe_fl_lag1,
    heat_fl_lag1 = dplyr::lag(heat_fl, 1),
    diff_heat_fl = heat_fl - heat_fl_lag1
  )

# Unión con precio
aux <- data %>%
  distinct(year, price_y) %>%
  left_join(aux_br, by = "year")
aux <- aux %>% left_join(aux_co, by = "year")
aux <- aux %>% left_join(aux_vt, by = "year")
aux <- aux %>% left_join(aux_fl, by = "year")
aux <- aux %>% drop_na()

m1 <- ivreg(
  price_y ~ diff_qe_br + diff_qe_co + diff_qe_vt + diff_qe_fl |
    heat_br_lag1 + heat_co_lag1 + heat_vt_lag1 + heat_fl_lag1 +
    diff_qe_br + diff_qe_co + diff_qe_vt + diff_qe_fl,
  data = aux
)

summary(m1)

#-------------------------------------------------------------------------------
# [VF] Forma reducida para precios
#-------------------------------------------------------------------------------
data_br <- data %>%
  filter(dummy_yc == 1, 
         net_export_dummy == 1,
         country %in% c("Brazil")) %>%
  select(country, year, qe_yc, price_y) %>%
  rename(qe_br = qe_yc) %>%
  mutate(
    log_qe_br = log(qe_br)
  )

data_co <- data %>%
  filter(dummy_yc == 1, 
         net_export_dummy == 1,
         country %in% c("Colombia")) %>%
  select(country, year, qe_yc) %>%
  rename(qe_co = qe_yc) %>%
  mutate(
    log_qe_co = log(qe_co)
  )

data_vt <- data %>%
  filter(dummy_yc == 1, 
         net_export_dummy == 1,
         country %in% c("Viet Nam")) %>%
  select(country, year, qe_yc) %>%
  rename(qe_vt = qe_yc) %>%
  mutate(
    log_qe_vt = log(qe_vt)
  )

data_leaders <- data %>%
  filter(dummy_yc == 1, 
         net_export_dummy == 1,
         country %in% c("Cuba", "Peru", "Panama")) %>%
  select(country, year, qe_yc) %>%
  group_by(year) %>%
  summarise(
    qe_leaders_sum = sum(qe_yc, na.rm = TRUE),
    qe_leaders_mean = mean(qe_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    log_qe_leaders = log(qe_leaders_sum)
  )

data_followers <- data %>%
  filter(dummy_yc == 1, 
         net_export_dummy == 1,
         !country %in% c("Cuba", "Peru", "Panama")) %>%
  select(country, year, qe_yc) %>%
  group_by(year) %>%
  summarise(
    qe_followers_sum = sum(qe_yc, na.rm = TRUE),
    qe_followers_mean = mean(qe_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    log_qe_followers = log(qe_followers_sum)
  )

data_motiv <- data_br %>%
  left_join(data_co, by = "year")
data_motiv <- data_motiv %>%
  left_join(data_vt, by = "year")
data_motiv <- data_motiv %>%
  left_join(data_leaders, by = "year")
data_motiv <- data_motiv %>%
  left_join(data_followers, by = "year")

m1 <- lm(
  price_y ~ qe_leaders_sum + qe_followers_sum,
  data = data_motiv
)

summary(m1)


#-------------------------------------------------------------------------------
# [VF] Long Differences
#-------------------------------------------------------------------------------
# Filtrar años, crear periodos y calcular promedios
data_ld <- data %>%
  filter(dummy_yc == 1,
         net_export_dummy == 1,
         year >= 1960 & year <= 2019) %>%
  mutate(period = ifelse(year %in% 1989:1991, "initial",
                         ifelse(year %in% 1997:2019, "final", NA)),
         rev_yc = price_y * qe_yc,
         share_yc = qe_yc / qe_y) %>%
  filter(!is.na(period)) %>%
  group_by(country, period) %>%
  summarise(
    qe = mean(qe_yc, na.rm = TRUE),
    heat = mean(heatExp_yc, na.rm = TRUE),
    frost = mean(frostExp_yc, na.rm = TRUE),
    opt = mean(optExp_yc, na.rm = TRUE),
    price = mean(price_y, na.rm = TRUE),
    tas = mean(tas_yce, na.rm = TRUE),
    rev = mean(rev_yc, na.rm = TRUE),
    share = mean(share_yc, na.rm = TRUE),
    tmax = mean(tmax_yce, na.rm = TRUE),
    tmin = mean(tmin_yce, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

# Crear diferencias (final - inicial)
diff_data <- data_ld %>%
  pivot_wider(names_from = period, values_from = c(qe, tmax, tmin, share, rev, heat, frost, opt, price, tas)) %>%
  mutate(
    d_qe = qe_final - qe_initial,
    d_heat = heat_final - heat_initial,
    d_frost = frost_final - frost_initial,
    d_price = price_final - price_initial,
    d_tas = tas_final - tas_initial,
    d_opt = opt_final - opt_initial,
    d_rev = rev_final - rev_initial,
    d_share = share_final - share_initial,
    d_tmax = tmax_final - tmax_initial,
    d_tmin = tmin_final - tmin_initial
  ) %>%
  select(country, starts_with("d_")) %>%
  drop_na()

# Regresión de diferencias
model <- lm(d_share ~ d_tmax + d_heat + d_price, data = diff_data)

# Resultados
summary(model)
tidy(model)

# Gráfico clave: Calor vs Exportaciones
ggplot(diff_data, aes(x = d_opt, y = d_qe, label = country)) +
  geom_point(color = "#2E8B57", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#8B4513") +
  labs(
    title = "Impacto del Cambio Climático en Exportaciones de Café",
    subtitle = "Cambio 1990-1994 vs 2015-2019 (Long Differences)",
    x = "Cambio en Exposición al Calor (d_heat)",
    y = "Cambio en Exportaciones (d_qe)",
    caption = "Fuente: Base data_yc. Nota: Incluye control por cambios en precio y heladas."
  ) +
  theme_minimal(base_size = 13)


m1 <- feols(qe_yc ~ heatExp_yc + frostExp_yc |
              country + year, data = data_yc %>% filter(year >= 1990))

summary(m1)


#-------------------------------------------------------------------------------
# [VF] Motivación
#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. CALCULAR CAMBIO EN ESTRÉS TÉRMICO (TEMPERATURA PROMEDIO)
# Suponiendo que tas_yce es temperatura promedio anual por país

heat_change <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1) %>%
  select(country, year, qe_y, qe_yc, tas_yce, heatExp_yc) %>%
  mutate(periodo = case_when(
    year %in% 1970:1989 ~ "antes",
    year %in% 1990:2019 ~ "despues",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(periodo)) %>%
  group_by(country, periodo) %>%
  summarise(temp_promedio = mean(tas_yce, na.rm = TRUE), 
            heat_promedio = mean(heatExp_yc, na.rm = TRUE),
            .groups = 'drop') %>%
  ungroup() %>%
  pivot_wider(names_from = periodo, values_from = c(temp_promedio, heat_promedio)) %>%
  mutate(cambio_temp = temp_promedio_despues - temp_promedio_antes,
         cambio_heat = heat_promedio_despues - heat_promedio_antes) %>%
  select(country, cambio_temp, cambio_heat)

# 2. CALCULAR CAMBIO EN PARTICIPACIÓN DE MERCADO
# Unir con datos de países y calcular participación
participation_change <- data %>%
  filter(dummy_yc == 1, net_export_dummy == 1) %>%
  mutate(
    share_yc = qe_yc / qe_y
  ) %>%
  mutate(periodo = case_when(
    year %in% 1970:1989 ~ "antes",
    year %in% 1990:2019 ~ "despues",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(periodo)) %>%
  group_by(country, periodo) %>%
  summarise(participacion_prom = mean(share_yc, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  pivot_wider(names_from = periodo, values_from = participacion_prom) %>%
  mutate(cambio_participacion = despues - antes) %>%
  select(country, cambio_participacion, participacion_antes = antes, participacion_despues = despues)

# 3. UNIR AMBAS VARIABLES Y CLASIFICAR PAÍSES
plot_data <- heat_change %>%
  inner_join(participation_change, by = "country") %>%
  # Crear grupos: Líderes (definir según tu criterio) y Seguidores
  mutate(grupo = case_when(
    country %in% c("Brazil", "Colombia", "Viet Nam") ~ "Líder",
    participacion_antes > 0.02 ~ "Seguidor Grande", # Umbral arbitrario
    TRUE ~ "Seguidor Pequeño"
  ))


# 5. VERSIÓN ALTERNATIVA: FOCUS EN LA NUBE Y EL OUTLIER
# Filtrar para ver mejor la distribución
# Gráfico simple con los tres líderes etiquetados
ggplot(plot_data, aes(x = cambio_temp, y = cambio_participacion)) +
  # Todos los puntos en gris
  geom_point(color = "gray60", size = 2, alpha = 0.7) +
  
  # Destacar los tres líderes
  geom_point(
    data = filter(plot_data, country %in% c("Brazil", "Colombia", "Vietnam", "Viet Nam")),
    aes(color = country),
    size = 4,
    shape = c(16, 17, 15)[as.factor(filter(plot_data, country %in% c("Brazil", "Colombia", "Vietnam", "Viet Nam"))$country)]
  ) +
  
  # Etiquetar los tres líderes
  geom_text_repel(
    data = filter(plot_data, country %in% c("Brazil", "Colombia", "Vietnam", "Viet Nam")),
    aes(label = country, color = country),
    size = 3.5,
    box.padding = 0.5,
    show.legend = FALSE
  ) +
  
  # Escala de colores para líderes
  scale_color_manual(
    values = c(
      "Brazil" = "#0072B2",   # Azul
      "Colombia" = "#009E73", # Verde
      "Vietnam" = "#D55E00",  # Naranja
      "Viet Nam" = "#D55E00"  # Por si acaso
    )
  ) +
  
  labs(
    x = "Δ Temperatura (°C)",
    y = "Δ Participación (%)",
    color = NULL
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

# Gráfico simple sin líderes - versión minimalista pura
# Con línea de tendencia sutil
ggplot(plot_data %>% filter(!(country %in% c("Brazil", "Colombia", "Vietnam", "Viet Nam"))), 
       aes(x = cambio_heat, y = cambio_participacion)) +
  
  geom_point(color = "gray40", size = 2, alpha = 0.7) +
  
  # Línea de tendencia muy sutil
  geom_smooth(method = "lm", se = FALSE, 
              color = "gray30", linewidth = 0.5, linetype = "dashed") +
  
  # Líneas de referencia cero
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3, linetype = "solid") +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3, linetype = "solid") +
  
  labs(x = "Δ Temperatura", y = "Δ Participación") +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray80")
  )
