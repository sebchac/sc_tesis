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

plot3 <- ggplot(data_yc, aes(factor(year), y = heat, fill = type)) +
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
              filter(scenario == "Stackelberg Base") %>%
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
    y = "US cents/lb"
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
              filter(scenario == "Stackelberg Base") %>%
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
  coord_cartesian(ylim = c(0, 300)) +
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
       width = 9,
       height = 7,
       units = "cm",
       dpi = 600,
       bg = "white")

#-------------------------------------------------------------------------------
# [] Precios mensuales estimados (contrafactual)
#-------------------------------------------------------------------------------

aux <- results_y %>%
  mutate(
    scenario = if_else(
      scenario == "CF0", "CF1"
    ),
    date = as.Date(date)
  )

ggplot(aux, aes(x = date, y = price, color = scenario, group = scenario)) +
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
data_y <- data %>%
  filter(dummy_y == 1, 
         export_dummy == 1,
         year >= 1965, year <= 2019)

# Efecto de gdd, hdd y fdd en precios

ggplot(data_y, aes(x = tas_ye, y = price_y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linetype = "dashed") +
  theme_classic()

# Efecto de gdd, hdd y fdd en cantidades
ggplot(data_y, aes(x = fdd_y_mean, y = qe_y)) +
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
  filter(scenario == "Stackelberg Base" | scenario == "Stackelberg CF1") %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    trend5y = floor((year - min(year)) / 5),
    scenario = case_when(
      scenario == "Stackelberg Base" ~ 0,
      scenario == "Stackelberg CF1" ~ 1,
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
    export_dummy == 1,
    year >= 1990,
    year <= 2019) %>%
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
    year >= 1990,
    year <= 2019) %>%
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

#------

#-----
# [] Test T
#-----
aux_yc <- results_yc %>%
  filter(scenario %in% c("Stackelberg Base", "Stackelberg CF2")) %>%
  select(market_id, country, scenario, quantity) %>%
  pivot_wider(
    names_from = scenario,
    values_from = quantity,
    names_prefix = "quantity_"
  ) %>%
  mutate(
    delta_quantity = `quantity_Stackelberg CF2` - `quantity_Stackelberg Base`
  )

t_test_delta <- t.test(aux_yc$delta_quantity, mu = 0)
print(t_test_delta)
