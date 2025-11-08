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
data <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/data.rds")

# ------------------------------------------------------------------------------
# [] Climate over Q exports
# ------------------------------------------------------------------------------

data_yc <- data %>%
  filter(dummy_yc == 1 & year >= 1990)


model_1 <- feols(
  qe_yc ~ 
    gdd_yc | year + country,
  data = data_yc,
  cluster = ~country
)

summary(model_1)

# ------------------------------------------------------------------------------
# [] Long differences
# ------------------------------------------------------------------------------
# Definir períodos
period_early <- 1995:1999
period_late <- 2015:2019

# Promedios por país y año

data_yc <- data %>%
  filter(dummy_yc == 1)

df_long_diff <- data_yc %>%
  filter(year %in% c(period_early, period_late)) %>%
  mutate(period = case_when(
    year %in% period_early ~ "early",
    year %in% period_late ~ "late"
  )) %>%
  group_by(country, period) %>%
  summarise(
    qe_yc_avg = mean(qe_yc, na.rm = TRUE),
    gdd_yc_avg = mean(gdd_yc, na.rm = TRUE),
    hdd_yc_avg = mean(hdd_yc, na.rm = TRUE),
    fdd_yc_avg = mean(fdd_yc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Crear variables en formato wide
  pivot_wider(
    names_from = period,
    values_from = c(qe_yc_avg, gdd_yc_avg, hdd_yc_avg, fdd_yc_avg)
  ) %>%
  # Calcular diferencias (late - early)
  mutate(
    delta_qe = log(qe_yc_avg_late) - log(qe_yc_avg_early),
    delta_gdd = gdd_yc_avg_late - gdd_yc_avg_early,
    delta_hdd = hdd_yc_avg_late - hdd_yc_avg_early,
    delta_fdd = fdd_yc_avg_late - fdd_yc_avg_early
  ) %>%
  # Filtrar observaciones completas
  filter(!is.na(delta_qe) & !is.na(delta_gdd))

cat("Países en el análisis:", nrow(df_long_diff), "\n")

# --- ESTADÍSTICAS DESCRIPTIVAS ---
cat("\n=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
df_long_diff %>%
  select(starts_with("delta_")) %>%
  summary() %>%
  print()

# --- MODELO 1: ESPECIFICACIÓN BASE ---
model1 <- feols(delta_qe ~ delta_gdd, data = df_long_diff)

cat("\n=== MODELO 1: ESPECIFICACIÓN BASE ===\n")
summary(model1)

# --- MODELO 2: CONTROLES ADICIONALES ---
model2 <- feols(delta_qe ~ delta_fdd, data = df_long_diff)

cat("\n=== MODELO 2: CON CONTROLES ===\n")
summary(model2)

# --- TABLA DE RESULTADOS ---
cat("\n=== TABLA COMPARATIVA ===\n")
etable(model1, model2, 
       vcov = "HC1",
       digits = 4,
       digits.stats = 3)
