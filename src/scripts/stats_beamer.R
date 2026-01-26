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
library(scales)

library(tidyverse)
library(scales)
library(countrycode) 

library(ggrepel)

# Rutas
fig_path <- "/Users/sebastianchacon/Desktop/sc_tesis/beamer/figures/"
tab_path <- "/Users/sebastianchacon/Desktop/sc_tesis/beamer/tables/"
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

#------------------------------
# PLOT. Cambio en costos y cantidades 
#------------------------------

# Calcular factor de escala para el eje derecho
max_cost <- max(abs(aux$diff_cost), na.rm = TRUE)
max_quantity <- max(abs(aux$diff_quantity), na.rm = TRUE)
scale_factor <- max_cost / max_quantity

diff_costs_diff_qe <- ggplot(aux, aes(x = country)) +
  # Línea de referencia horizontal
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # Segmentos para diff_cost (líneas verticales)
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_cost),
               color = "steelblue", linewidth = 0.4, alpha = 0.6) +
  
  # Puntos para diff_cost
  geom_point(aes(y = diff_cost, shape = grupo_cost, color = "diff_cost"), 
             size = 3, stroke = 1) +
  
  # Puntos para diff_quantity
  geom_point(aes(y = diff_quantity * scale_factor, shape = "diff_quantity", 
                 color = "diff_quantity"), 
             size = 3, stroke = 1) +
  
  # Escalas de color y forma
  scale_color_manual(
    name = "Variable",
    values = c("diff_cost" = "#2E86AB",  # Azul académico
               "diff_quantity" = "#A23B72"),  # Púrpura académico
    labels = c("diff_cost" = "Costo", "diff_quantity" = "Cantidad")
  ) +
  
  scale_shape_manual(
    name = "Variable",
    values = c("Negativo" = 16,  # Círculo sin relleno para diff_cost negativo
               "Positivo" = 16,  # Círculo relleno para diff_cost positivo
               "Neutro" = 4,     # Cruz para diff_cost neutro
               "diff_quantity" = 18)
  ) +
  
  # Escalas de los ejes Y (izquierdo y derecho)
  scale_y_continuous(
    name = "Cambio en costo (%)",
    labels = scales::percent,
    limits = c(-0.3, 0.6),
    sec.axis = sec_axis(
      ~./scale_factor,  # Revertir la escala para diff_quantity
      name = "Cambio en cantidad (%)",
      labels = scales::percent
    )
  ) +
  
  labs(
    x = NULL,
    shape = NULL,
    color = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, color = "#2E86AB", margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, color = "#A23B72", margin = margin(l = 10)),
    
    # Leyenda
    legend.position = "none",
    legend.box = "horizontal",
    legend.margin = margin(t = -10),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 9),
    
    # Grid y fondo
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Bordes y márgenes
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15),
    
    # Títulos de leyenda
    legend.title = element_blank()
  )

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_costs_diff_qe.png"), 
       plot = diff_costs_diff_qe,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

#------------------------------
# PLOT. Cambio en costos y profits 
#------------------------------

# Calcular factor de escala para el eje derecho
max_cost <- max(abs(aux$diff_cost), na.rm = TRUE)
max_profit <- max(abs(aux$diff_profit), na.rm = TRUE)
scale_factor <- max_cost / max_profit

diff_costs_diff_profit <- ggplot(aux, aes(x = country)) +
  # Línea de referencia horizontal
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # Segmentos para diff_cost (líneas verticales)
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_cost),
               color = "steelblue", linewidth = 0.4, alpha = 0.6) +
  
  # Puntos para diff_cost
  geom_point(aes(y = diff_cost, shape = grupo_cost, color = "diff_cost"), 
             size = 3, stroke = 1) +
  
  # Puntos para diff_quantity
  geom_point(aes(y = diff_profit * scale_factor, shape = "diff_profit", 
                 color = "diff_profit"), 
             size = 3, stroke = 1) +
  
  # Escalas de color y forma
  scale_color_manual(
    name = "Variable",
    values = c("diff_cost" = "#2E86AB",  # Azul académico
               "diff_profit" = "#A23B72"),  # Púrpura académico
    labels = c("diff_cost" = "Costo", "diff_profit" = "Beneficio")
  ) +
  
  scale_shape_manual(
    name = "Variable",
    values = c("Negativo" = 16,  # Círculo sin relleno para diff_cost negativo
               "Positivo" = 16,  # Círculo relleno para diff_cost positivo
               "Neutro" = 4,     # Cruz para diff_cost neutro
               "diff_profit" = 18)
  ) +
  
  # Escalas de los ejes Y (izquierdo y derecho)
  scale_y_continuous(
    name = "Cambio en costo (%)",
    labels = scales::percent,
    limits = c(-0.3, 0.6),
    sec.axis = sec_axis(
      ~./scale_factor,  # Revertir la escala para diff_quantity
      name = "Cambio en beneficio (%)",
      labels = scales::percent
    )
  ) +
  
  labs(
    x = NULL,
    shape = NULL,
    color = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, color = "#2E86AB", margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, color = "#A23B72", margin = margin(l = 10)),
    
    # Leyenda
    legend.position = "none",
    legend.box = "horizontal",
    legend.margin = margin(t = -10),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 9),
    
    # Grid y fondo
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Bordes y márgenes
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15),
    
    # Títulos de leyenda
    legend.title = element_blank()
  )

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_costs_diff_profit.png"), 
       plot = diff_costs_diff_profit,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

#------------------------------
# PLOT. Regresiones de diferencias
#------------------------------
aux_yc <- results_yc %>%
  filter(scenario %in% c("CF0", "CF1")) %>%
  rename(mc = mc_yc_hat_s) %>%
  select(date, country, dummy_leader, quantity, revenue, profit, mc, price, scenario) %>%
  pivot_wider(
    id_cols = c(date, country, dummy_leader),
    names_from = scenario,
    values_from = c(quantity, revenue, profit, mc, price)
  ) %>%
  mutate(
    diff_profit = round(`profit_CF0` / `profit_CF1` - 1, 4),
    diff_revenue = round(revenue_CF0 / revenue_CF1 - 1, 4),
    diff_quantity = round(`quantity_CF0` / `quantity_CF1` - 1, 4),
    diff_cost = round(`mc_CF0` / `mc_CF1` - 1, 4)
  )

ggplot(aux_yc, aes(x = mc_CF1, y = profit_CF1)) +
  # Líneas de referencia en cero
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  
  # Línea de regresión lineal
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#2E86AB",
    fill = alpha("#2E86AB", 0.15),
    linewidth = 1,
    alpha = 0.2
  ) +
  
  # Puntos de dispersión (círculos y triángulos)
  geom_point(
    aes(shape = ifelse(dummy_leader == 1, "Líder", "Seguidor"),
        fill = ifelse(dummy_leader == 1, "Líder", "Seguidor")),
    size = 3.5,
    stroke = 0.8,
    alpha = 0.85
  ) +
  
  # Escalas de forma y relleno
  scale_shape_manual(
    name = NULL,
    values = c("Líder" = 24,      # Triángulo (punta arriba)
               "Seguidor" = 21)   # Círculo
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c("Líder" = "#2E86AB",      # Azul académico
               "Seguidor" = "#A23B72")   # Púrpura académico
  ) +
  
  # Escalas de ejes en porcentaje
  scale_x_continuous(
    name = "Costos",
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  scale_y_continuous(
    name = "Beneficios",
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  # Anotación estadística minimalista
  geom_text(
    data = aux_yc %>%
      summarise(
        cor = round(cor(mc_CF1, profit_CF1, use = "complete.obs"), 3),
        r2 = round(summary(lm(profit_CF1 ~ mc_CF1))$r.squared, 3)
      ),
    aes(x = min(aux_yc$mc_CF1, na.rm = TRUE) * 0.95,
        y = max(aux_yc$profit_CF1, na.rm = TRUE) * 0.95,
        label = paste0("r = ", cor, "\nR² = ", r2)),
    size = 3.2,
    hjust = 0,
    vjust = 1,
    color = "gray40",
    fontface = "italic",
    lineheight = 0.9
  ) +
  
  # Títulos
  labs(
    title = "Relación entre Diferencia de Costo y Ganancia",
    subtitle = "Líderes vs. Seguidores",
    caption = "Nota: Línea de regresión con intervalo de confianza al 95%"
  ) +
  
  # Tema académico simple
  theme_minimal(base_size = 12) +
  theme(
    # Títulos
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray50",
      hjust = 1,
      margin = margin(t = 10)
    ),
    
    # Ejes
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = 11,
      margin = margin(r = 8)
    ),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "gray70", linewidth = 0.5),
    
    # Leyenda
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(b = 5),
    legend.text = element_text(size = 10),
    
    # Fondo
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Márgenes
    plot.margin = margin(15, 15, 15, 15)
  )

#------------------------------
# PLOT. Cambio en calor y en costos
#------------------------------
aux_heat <- data_cf %>%
  select(year, country, dummy_leader, heatExp_yc, heatExp_yc_cf) %>%
  group_by(country) %>%
  summarise(
    dummy_leader = first(dummy_leader),
    heatExp_c = mean(heatExp_yc, na.rm = TRUE),
    heatExp_cf_c = mean(heatExp_yc_cf, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    country = factor(country, levels = country),
    diff = heatExp_c / heatExp_cf_c - 1
  ) %>%
  filter(country %in% country_plots)

aux2 <- aux %>%
  left_join(aux_heat %>% select(country, diff), by = "country")

# Calcular factor de escala para el eje derecho
max_cost <- max(abs(aux2$diff_cost), na.rm = TRUE)
max_heat <- max(abs(aux2$diff), na.rm = TRUE)
scale_factor <- max_cost / max_heat

diff_heat_diff_costs <- ggplot(aux2, aes(x = country)) +
  # Línea de referencia horizontal
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # Segmentos para diff_cost (líneas verticales)
  geom_segment(aes(x = country, xend = country, y = 0, yend = diff_cost),
               color = "steelblue", linewidth = 0.4, alpha = 0.6) +
  
  # Puntos para diff_cost
  geom_point(aes(y = diff_cost, shape = grupo_cost, color = "diff_cost"), 
             size = 3, stroke = 1) +
  
  # Puntos para diff_quantity
  geom_point(aes(y = diff * scale_factor, shape = "diff", 
                 color = "diff"), 
             size = 3, stroke = 1) +
  
  # Escalas de color y forma
  scale_color_manual(
    name = "Variable",
    values = c("diff_cost" = "#2E86AB",  # Azul académico
               "diff" = "#A23B72"),  # Púrpura académico
    labels = c("diff_cost" = "Costo", "diff" = "Calor")
  ) +
  
  scale_shape_manual(
    name = "Variable",
    values = c("Negativo" = 16,  # Círculo sin relleno para diff_cost negativo
               "Positivo" = 16,  # Círculo relleno para diff_cost positivo
               "Neutro" = 4,     # Cruz para diff_cost neutro
               "diff" = 18)
  ) +
  
  # Escalas de los ejes Y (izquierdo y derecho)
  scale_y_continuous(
    name = "Cambio en costo (%)",
    labels = scales::percent,
    limits = c(-0.3, 0.6),
    sec.axis = sec_axis(
      ~./scale_factor,  # Revertir la escala para diff_quantity
      name = "Cambio en calor (%)",
      labels = scales::percent
    )
  ) +
  
  labs(
    x = NULL,
    shape = NULL,
    color = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, color = "#2E86AB", margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, color = "#A23B72", margin = margin(l = 10)),
    
    # Leyenda
    legend.position = "none",
    legend.box = "horizontal",
    legend.margin = margin(t = -10),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 9),
    
    # Grid y fondo
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Bordes y márgenes
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15),
    
    # Títulos de leyenda
    legend.title = element_blank()
  )

print(diff_heat_diff_costs)

# Guardar en alta resolución
ggsave(paste0(fig_path, "diff_heat_diff_costs.png"), 
       plot = diff_heat_diff_costs,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

#------------------------------
# PLOT Pérdida y efecto estratégico
#------------------------------

# Descomposición

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
    
    price_effect = (price_CF1 - price_CF0) * quantity_CF0 * conversion,
    
    quantity_effect = (price_CF1 - costs_CF1) * (quantity_CF1 - quantity_CF0) * conversion,
    
    strategic_effect = total_effect - direct_effect,
    
    abs_total = abs(direct_effect) + abs(strategic_effect),
    
    direct_prop = ifelse(abs_total > 0, abs(direct_effect) / abs_total, NA),
    
    strategic_prop = ifelse(abs_total > 0, abs(strategic_effect) / abs_total, NA),
    
    diff_cost = costs_CF0 / costs_CF1 - 1,
    diff_quantity = quantity_CF0 / quantity_CF1 - 1,
    
    year = as.factor(year(date))
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

ax_c <- descomp_c %>% filter(!country %in% c("Côte d'Ivoire", "Cuba"))
ax_yc <- descomp_yc %>% filter(!country %in% c("Côte d'Ivoire", "Viet Nam", "Cuba", "Colombia", "Brazil"))

plot(ax_c$strategic_prop, ax_c$total_effect)
plot(ax_yc$strategic_prop, ax_yc$total_effect)

efecto_est_prop_nout <- ggplot(ax_c, aes(x = strategic_prop, y = total_effect)) +
  # Líneas de referencia
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  
  # Puntos - solo diferencia líderes por color
  geom_point(aes(color = ifelse(dummy_leader == 1, "Líder", "Seguidor")),
             size = 3) +
  
  # Escala de colores
  scale_color_manual(
    values = c("Líder" = "#2E86AB", "Seguidor" = "#A23B72")
  ) +
  
  # Ejes
  scale_x_continuous(
    name = "Efecto estratégico (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  
  scale_y_continuous(
    name = "Pérdida de excedente (mill. USD)"
  ) +
  
  # Tema con "caja" (borde)
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(r = 8)),
    axis.title.y = element_text(size = 10, margin = margin(t = 8)),
    
    # Leyenda
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    
    # Grid y fondo
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Bordes (esto crea la "caja")
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    
    # Márgenes
    plot.margin = margin(15, 15, 15, 15)
  )

print(efecto_est_prop_nout)

# Guardar en alta resolución
ggsave(paste0(fig_path, "efecto_est_prop_nout.png"), 
       plot = efecto_est_prop_nout,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

#------------------------------
# PLOT Origen de la estrategia
#------------------------------
bb_yc <- descomp_yc %>%
  group_by(year) %>%
  mutate(
    qe_y_CF0 = sum(quantity_CF0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    share_yc_CF0 = quantity_CF0 / qe_y_CF0,
    diff_costs_nivel = abs(costs_CF0 - costs_CF1),
    diff_cost = abs(diff_cost)
  ) #%>%
  #filter(!country %in% c("Côte d'Ivoire", "Viet Nam", "Cuba", "Colombia", "Brazil"))

hh_yc <- data_cf %>%
  select(year, country, heatExp_yc, heatExp_yc_cf) %>%
  mutate(
    diff_heat_nivel = abs(heatExp_yc - heatExp_yc_cf)
  )

aa_yc <- bb_yc %>%
  left_join(hh_yc, by = c("country", "year")) %>%
  mutate(
    dummy_leader = case_when(
      country %in% c("Brazil", "Colombia", "Viet Nam") ~ 1,
                     TRUE ~ 0)
    )

# Costos de rivales y cantidades
aa_yc <- aa_yc %>%
  group_by(year) %>%
  mutate(
    costs_total_CF0 = sum(costs_CF0, na.rm = TRUE),
    costs_total_CF1 = sum(costs_CF1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    costs_rivals_CF0 = costs_total_CF0 - costs_CF0,
    costs_rivals_CF1 = costs_total_CF1 - costs_CF1,
    diff_costs_rivals = costs_rivals_CF0 / costs_rivals_CF1 - 1,
    i_costs_CF0 = -costs_CF0 + costs_rivals_CF0,
    i_costs_CF1 = -costs_CF1 + costs_rivals_CF1,
    i_costs_diff = i_costs_CF0 - i_costs_CF1
  )

aa_c <- aa_yc %>%
  group_by(country) %>%
  summarise(
    strategic_prop = mean(strategic_prop, na.rn = TRUE),
    diff_heat_nivel = mean(diff_heat_nivel, na.rm = TRUE),
    diff_costs_nivel = mean(diff_costs_nivel, na.rm = TRUE),
    dummy_leader = first(dummy_leader),
    diff_costs_rivals = mean(diff_costs_rivals, na.rm = TRUE),
    .groups = 'drop'
  )
  
plot(aa_c$diff_costs_rivals, aa_c$strategic_prop)

plot(aa_c$diff_heat_nivel, aa_c$strategic_prop)

a <- feols(quantity_CF0 ~ i_costs_CF0 | country, data = aa_yc)
summary(a)

library(ggplot2)

# Asumiendo que aa_yc es tu dataframe con las variables:
# - strategic_prop (proporción del efecto estratégico, 0-1)
# - diff_costs_nivel (diferencia absoluta en costos)
# - dummy_leader (1 para líder, 0 para seguidor)

costs_strat <- ggplot(aa_c, aes(x = diff_costs_nivel, y = strategic_prop)) +
  # Puntos - diferenciados por líder/seguidor
  geom_point(aes(color = ifelse(dummy_leader == 1, "Líder", "Seguidor")),
             size = 3, alpha = 0.8) +
  
  # Líneas de referencia relevantes para tu análisis
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  
  # Escala de colores (mismos que tu ejemplo)
  scale_color_manual(
    name = "",
    values = c("Líder" = "#2E86AB", "Seguidor" = "#A23B72")
  ) +
  
  # Ejes
  scale_x_continuous(
    name = "Cambio en costos (abs. ¢/lb)",
    limits = c(min(aa_c$diff_costs_nivel, na.rm = TRUE) * 1.1,
               max(aa_c$diff_costs_nivel, na.rm = TRUE) * 1.1)
  ) +
  
  scale_y_continuous(
    name = "Efecto estratégico (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  
  # Tema (adaptado de tu ejemplo)
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    
    # Leyenda (la mantengo activa para este gráfico)
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.margin = margin(b = 5),
    
    # Grid y fondo
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    
    # Bordes
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    
    # Márgenes
    plot.margin = margin(15, 15, 15, 15)
  ) +
  
  # Título opcional para claridad
  labs(
    title = NULL,
    subtitle = NULL
  )

print(costs_strat)

# Guardar en alta resolución
ggsave(paste0(fig_path, "costs_strat.png"), 
       plot = costs_strat,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

heat_strat <- ggplot(aa_c, aes(x = diff_heat_nivel, y = strategic_prop)) +
  # Puntos - diferenciados por líder/seguidor
  geom_point(aes(color = ifelse(dummy_leader == 1, "Líder", "Seguidor")),
             size = 3, alpha = 0.8) +
  
  # Líneas de referencia relevantes para tu análisis
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  
  # Escala de colores (mismos que tu ejemplo)
  scale_color_manual(
    name = "",
    values = c("Líder" = "#2E86AB", "Seguidor" = "#A23B72")
  ) +
  
  # Ejes
  scale_x_continuous(
    name = "Cambio en calor (abs. dias)",
    limits = c(min(aa_c$diff_heat_nivel, na.rm = TRUE) * 1.1,
               max(aa_c$diff_heat_nivel, na.rm = TRUE) * 1.1)
  ) +
  
  scale_y_continuous(
    name = "Efecto estratégico (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  
  # Tema (adaptado de tu ejemplo)
  theme(
    # Ajustes de ejes
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    
    # Leyenda (la mantengo activa para este gráfico)
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.margin = margin(b = 5),
    
    # Grid y fondo
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    
    # Bordes
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    
    # Márgenes
    plot.margin = margin(15, 15, 15, 15)
  ) +
  
  # Título opcional para claridad
  labs(
    title = NULL,
    subtitle = NULL
  )

print(heat_strat)

# Guardar en alta resolución
ggsave(paste0(fig_path, "heat_strat.png"), 
       plot = heat_strat,
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

#------------------------------
# PLOT Ganadores estratégicos
#------------------------------

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

#------------------------------
# Markup líderes y seguidores
#------------------------------

aux_yc <- results_yc %>%
  filter(scenario %in% c("CF0", "CF1")) %>%
  rename(mc = mc_yc_hat_s) %>%
  mutate(
    markup = (price - mc) / mc,
    year = year(date)
  ) %>%
  group_by(year, dummy_leader) %>%
  summarise(
    markup_group = mean(markup, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(
    names_from = dummy_leader,
    values_from = markup_group,
    names_prefix = "markup_"
  )

ggplot(aux_yc, aes(x = year)) +
  geom_line(aes(y = markup_1, color = "Líder"), size = 1) +
  geom_line(aes(y = markup_0, color = "Seguidor"), size = 1) +
  labs(x = "Año", y = "Markup", color = "Grupo") +
  scale_color_manual(values = c("Líder" = "#2E86AB", "Seguidor" = "#A23B72")) +
  theme_classic()


