# =============================================================================
# PROCESAMIENTO DE DATOS CLIMÁTICOS PARA CULTIVO DE CAFÉ - PANEL MENSUAL POR PAÍS
# =============================================================================

library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(sf)
library(ncdf4)

# --- CONFIGURACIÓN DE RUTAS Y CARGA DE BASES DE DATOS ---
base_path1 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/"
base_path2 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmax"
base_path3 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmin"

# Archivos SPAM (área cosechada)
arabica_file <- file.path(base_path1, "spam2020_V2r0_global_H_COFF_A.tif")
robusta_file <- file.path(base_path1, "spam2020_V2r0_global_H_RCOF_A.tif")

# Cargar datos SPAM
arabica <- rast(arabica_file)
robusta <- rast(robusta_file)

# Archivos ISIMIP (temperaturas)
tasmax_file <- file.path(base_path2, "20crv3-era5_obsclim_tasmax_global_daily_2011_2020.nc")
tasmin_file <- file.path(base_path3, "20crv3-era5_obsclim_tasmin_global_daily_2011_2020.nc")

# Cargar datos ISIMIP
tasmax <- rast(tasmax_file)
tasmin <- rast(tasmin_file)

# --- 1. DESCARGAR Y CARGAR SHAPEFILE DE PAÍSES (Natural Earth) ---
cat("Paso 1: Descargando shapefile de países...\n")

# Descargar shapefile de países de Natural Earth
temp_dir <- tempdir()
countries_url <- "https://naturalearth.s3.amazonaws.com/50m_cultural/ne_50m_admin_0_countries.zip"
countries_zip <- file.path(temp_dir, "countries.zip")
countries_shp <- file.path(temp_dir, "ne_50m_admin_0_countries.shp")

if (!file.exists(countries_shp)) {
  download.file(countries_url, countries_zip, quiet = TRUE)
  unzip(countries_zip, exdir = temp_dir)
}

# Cargar shapefile
countries <- st_read(countries_shp, quiet = TRUE) %>%
  st_make_valid()

# Seleccionar columnas relevantes y simplificar geometría para eficiencia
countries <- countries %>%
  select(NAME, ISO_A3, CONTINENT) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 0.1)

cat("Shapefile cargado:", nrow(countries), "países\n")

# --- 2. CREAR MÁSCARA DE CAFÉ Y IDENTIFICAR CELDAS POR PAÍS ---
cat("Paso 2: Creando máscara de café e identificando celdas por país...\n")

# Reemplazar NAs por 0 y combinar
arabica[is.na(arabica)] <- 0
robusta[is.na(robusta)] <- 0
coffee_area <- arabica + robusta
names(coffee_area) <- "coffee_area_ha"

coffee_area <- resample(coffee_area, tasmax, method = "sum")

# Crear máscara binaria
coffee_mask <- coffee_area
coffee_mask[coffee_mask > 0] <- 1
coffee_mask[coffee_mask ==   0] <- NA

# Obtener cells id reales de raster
coffee_cell <- which(!is.na(values(coffee_mask)) & values(coffee_mask) == 1)

# Extraer valores de área de café por país
coffee_by_country <- terra::extract(coffee_area, countries, fun = sum, na.rm = TRUE, bind = TRUE)
coffee_by_country <- st_as_sf(coffee_by_country)

# Filtrar países que tienen cultivo de café
coffee_countries <- coffee_by_country %>% 
  filter(coffee_area_ha > 0) %>%
  arrange(desc(coffee_area_ha))

cat("Países con cultivo de café:", nrow(coffee_countries), "\n")

# MODIFICACIÓN: Identificar celdas de café por país (sin ponderaciones)
identify_country_cells <- function(country_sf, coffee_raster, coffee_cell) {
  country_cells <- list()
  
  # Crear raster temporal con cell ids
  cell_id_raster <- coffee_raster
  values(cell_id_raster) <- 1:ncell(cell_id_raster)
  names(cell_id_raster) <- "cell_id"
  
  for(i in 1:nrow(country_sf)) {
    country <- country_sf[i, ]
    country_name <- country$NAME
    
    cat("Procesando:", country_name, "\n")
    
    # Extraer cell IDs dentro del país
    country_cells_ids <- terra::extract(cell_id_raster, country, na.rm = TRUE)
    
    # Filtrar solo celdas que tienen café
    country_data <- country_cells_ids %>%
      filter(cell_id %in% coffee_cell)
    
    if(nrow(country_data) > 0) {
      result <- data.frame(
        cell_id = country_data$cell_id,
        country = country_name
      )
      country_cells[[country_name]] <- result
      cat("  Celdas encontradas:", nrow(result), "\n")
    } else {
      cat("  No se encontraron celdas con café\n")
    }
  }
  return(bind_rows(country_cells))
}

# Identificar celdas por país
country_cells <- identify_country_cells(coffee_countries, coffee_area, coffee_cell)

cat("Celdas identificadas para", n_distinct(country_cells$country), "países cafetaleros\n")
cat("Total de celdas únicas:", n_distinct(country_cells$cell_id), "\n")

# --- 3. PROCESAR DATOS DE TEMPERATURA ---
cat("Paso 3: Procesando datos de temperatura...\n")

# Extraer fechas
nc <- nc_open(tasmax_file)
time <- ncvar_get(nc, "time")
units <- ncatt_get(nc, "time", "units")$value
origin <- as.Date(sub("days since ", "", units))
dates <- as.Date(origin + time)
nc_close(nc)

# Extraer matrices filtradas (SIN NAs)
tasmax <- mask(tasmax, coffee_mask)
tasmin <- mask(tasmin, coffee_mask)

# Convertir a Celsius
tasmax <- tasmax - 273.15
tasmin <- tasmin - 273.15

# --- 4. CALCULAR TMIN Y TMAX PROMEDIO SIMPLE POR PAÍS ---
cat("Paso 4: Calculando tmin y tmax promedio simple por país...\n")

# Usar todas las celdas únicas que tienen café
coffee_cells_all <- unique(country_cells$cell_id)
cat("Celdas únicas con café:", length(coffee_cells_all), "\n")

# Crear mapeo de cell_id a posición en la matriz
cell_mapping <- data.frame(
  cell_id = coffee_cells_all,
  matrix_position = 1:length(coffee_cells_all)
)

# Parámetros para umhbrales de temperatura
k1 <- 10
k2 <- 18
k3 <- 26
k4 <- 30
#day <- 12

# Inicializar lista para almacenar resultados
all_monthly_temps <- list()

# Procesar por años para eficiencia
for(year in 2011:2020) {
  cat("Procesando año:", year, "\n")
  
  year_indices <- which(year(dates) == year)
  year_dates <- dates[year_indices]
  
  # Extraer matrices de temperatura para el año (solo celdas con café)
  tasmax_year <- values(tasmax[[year_indices]])[coffee_cells_all, , drop = FALSE]
  tasmin_year <- values(tasmin[[year_indices]])[coffee_cells_all, , drop = FALSE]
  
  cat("  Dimensiones tasmax_year:", dim(tasmax_year), "\n")
  cat("  Número de días:", ncol(tasmax_year), "\n")
  
  # Lista para datos diarios del año
  monthly_year_data <- list()
  
  # Para cada país, calcular promedios simples diarios
  coffee_countries_list <- unique(country_cells$country)
  
  for(country_name in coffee_countries_list) {
    cat("  País:", country_name, "\n")
    
    # Obtener celdas para este país
    country_cells_data <- country_cells %>% 
      filter(country == country_name)
    
    if(nrow(country_cells_data) == 0) {
      cat("    Sin celdas para", country_name, "\n")
      next
    }
    
    cat("    Celdas encontradas:", nrow(country_cells_data), "\n")
    
    # Encontrar las posiciones de estas celdas en la matriz filtrada
    country_positions <- cell_mapping %>%
      filter(cell_id %in% country_cells_data$cell_id) %>%
      pull(matrix_position)
    
    if(length(country_positions) == 0) {
      cat("    No se encontraron posiciones para", country_name, "\n")
      next
    }
    
    cat("    Posiciones en matriz:", length(country_positions), "\n")
    
    # Verificar que las posiciones son válidas
    if(any(country_positions > nrow(tasmax_year)) || any(country_positions < 1)) {
      cat("    ERROR: Posiciones fuera de rango. Máximo:", nrow(tasmax_year), "\n")
      next
    }
    
    # Extraer temperaturas para las celdas de este país
    tmax_country <- tasmax_year[country_positions, , drop = FALSE]
    tmin_country <- tasmin_year[country_positions, , drop = FALSE]
    
    # Verificar dimensiones
    cat("    Dimensiones tmax_country:", dim(tmax_country), "\n")
    
    # MODIFICACIÓN: Calcular promedio simple (sin ponderaciones) para cada día
    daily_tmax <- numeric(length(year_dates))
    daily_tmin <- numeric(length(year_dates))
    
    for(day in 1:length(year_dates)) {
      daily_tmax[day] <- mean(tmax_country[, day], na.rm = TRUE)
      daily_tmin[day] <- mean(tmin_country[, day], na.rm = TRUE)
    }
    
    # Crear dataframe para este país
    country_daily <- data.frame(
      date = year_dates,
      year = year(year_dates),
      month_num = month(year_dates),
      country = country_name,
      tmax = daily_tmax,
      tmin = daily_tmin) %>%
      mutate(
      gdd = case_when(
        (tmax < k2) ~ 0,
        (tmin < k2 & tmax >= k2 & tmax < k3) ~ (1 - (k2 - tmin)/(tmax - tmin)),
        (tmin < k2 & tmax >= k3) ~ (k3 - k2)/(tmax - tmin),
        (tmin >= k2 & tmax < k3) ~ 1,
        (tmin >= k2 & tmin < k3 & tmax >= k3) ~ (k3 - tmin)/(tmax - tmin),
        (tmin > k3) ~ 0,
        TRUE ~ 0
      ),
      hdd = case_when(
        (tmin >= k4) ~ 1,
        (tmin < k4 & tmax >= k4) ~ (1 - (k4 - tmin)/(tmax - tmin)),
        (tmax < k4) ~ 0,
        TRUE ~ 0
      ),
      fdd = case_when(
        (tmin > k1) ~ 0,
        (tmin <= k1 & tmax > k1) ~ (k1 - tmin)/(tmax - tmin),
        (tmax <= k1) ~ 1,
        TRUE ~ 0
      )
    )
    
    country_monthly <- country_daily %>%
      group_by(country, year, month_num) %>%
      summarise(
        date = min(date),
        tmax_ymc = max(tmax, na.rm = TRUE),
        tmin_ymc = min(tmin, na.rm = TRUE),
        gdd_ymc = sum(gdd, na.rm = TRUE),
        hdd_ymc = sum(hdd, na.rm = TRUE),
        fdd_ymc = sum(fdd, na.rm = TRUE),
        .groups = 'drop'
      )
    
    monthly_year_data[[country_name]] <- country_monthly
    cat("    Datos creados para", country_name, "-", nrow(country_monthly), "días\n")
  }
  
  # Combinar todos los países para este año
  if(length(monthly_year_data) > 0) {
    year_monthly <- bind_rows(monthly_year_data)
    all_monthly_temps[[as.character(year)]] <- year_monthly
    cat("  Año", year, "completado -", nrow(year_monthly), "observaciones mensuales\n")
  } else {
    cat("  Año", year, "completado - 0 observaciones mensuales (sin datos)\n")
  }

}
  
  # Combinar todos los años (solo datos mensuales)
  if(length(all_monthly_temps) > 0) {
    monthly_country_temps <- bind_rows(all_monthly_temps)
    
    # Ordenar por país y fecha
    monthly_country_temps <- monthly_country_temps %>%
      arrange(country, date)
    
    cat("Procesamiento completado.\n")
    cat("Dimensiones finales mensuales:", dim(monthly_country_temps), "\n")
    cat("Países procesados:", n_distinct(monthly_country_temps$country), "\n")
    cat("Rango de fechas:", min(monthly_country_temps$date), "a", max(monthly_country_temps$date), "\n")
    cat("Total de meses procesados:", nrow(monthly_country_temps), "\n")
    
    # Ver estructura del resultado
    print(head(monthly_country_temps))
    
  } else {
    cat("ERROR: No se generaron datos mensuales. Revisar la extracción de celdas.\n")
    monthly_country_temps <- data.frame()
  }

saveRDS(monthly_country_temps, file = "/Users/sebastianchacon/Desktop/ObsData/ProcessedData/GDD/df_gdd_11_20.rds")

