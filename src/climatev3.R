library(terra)
library(dplyr)
library(lubridate)
library(sf)
library(ncdf4)

# --- CONFIGURACIÓN ---
base_path1 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/"
base_path2 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmax"
base_path3 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmin"
output_path <- "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/"

# Parámetros de umbrales de temperatura (comunes)
k1 <- 10
k2 <- 15
k3 <- 26

# Parámetros diferenciados por tipo de café
k4_arabica <- 26
k4_robusta <- 30

# Períodos a procesar
periods <- list(
  c(1961, 1970),
  c(1971, 1980),
  c(1981, 1990),
  c(1991, 2000),
  c(2001, 2010),
  c(2011, 2020)
)

# --- PASO 1: PREPARAR MÁSCARAS DE CAFÉ Y PAÍSES ---
cat("=== PREPARACIÓN INICIAL ===\n")

# Cargar datos SPAM (mantener separados)
arabica <- rast(file.path(base_path1, "spam2020_V2r0_global_H_COFF_A.tif"))
robusta <- rast(file.path(base_path1, "spam2020_V2r0_global_H_RCOF_A.tif"))

# Reemplazar NAs con 0
arabica[is.na(arabica)] <- 0
robusta[is.na(robusta)] <- 0

# Combinar para área total
coffee_area <- arabica + robusta
names(coffee_area) <- "coffee_area_ha"

# Crear clasificación de tipo dominante
coffee_type <- coffee_area
values(coffee_type) <- NA
values(coffee_type)[values(arabica) > values(robusta)] <- 1  # 1 = Arábica dominante
values(coffee_type)[values(robusta) > values(arabica)] <- 2  # 2 = Robusta dominante
values(coffee_type)[values(arabica) == values(robusta) & values(arabica) > 0] <- 1  # Empate = Arábica
names(coffee_type) <- "coffee_type"

cat("Celdas con Arábica dominante:", sum(values(coffee_type) == 1, na.rm = TRUE), "\n")
cat("Celdas con Robusta dominante:", sum(values(coffee_type) == 2, na.rm = TRUE), "\n")

# Descargar y cargar shapefile de países
temp_dir <- tempdir()
countries_url <- "https://naturalearth.s3.amazonaws.com/50m_cultural/ne_50m_admin_0_countries.zip"
countries_zip <- file.path(temp_dir, "countries.zip")
countries_shp <- file.path(temp_dir, "ne_50m_admin_0_countries.shp")

if (!file.exists(countries_shp)) {
  download.file(countries_url, countries_zip, quiet = TRUE)
  unzip(countries_zip, exdir = temp_dir)
}

countries <- st_read(countries_shp, quiet = TRUE) %>%
  st_make_valid() %>%
  select(NAME, ISO_A3, CONTINENT) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 0.1)

cat("Países cargados:", nrow(countries), "\n")

# Función para identificar celdas por país (MODIFICADA para incluir tipo de café)
identify_country_cells <- function(country_sf, coffee_raster, coffee_type_raster, coffee_cell) {
  cell_id_raster <- coffee_raster
  values(cell_id_raster) <- 1:ncell(cell_id_raster)
  names(cell_id_raster) <- "cell_id"
  
  country_cells <- lapply(1:nrow(country_sf), function(i) {
    country <- country_sf[i, ]
    country_cells_ids <- terra::extract(cell_id_raster, country, na.rm = TRUE)
    country_data <- country_cells_ids %>% filter(cell_id %in% coffee_cell)
    
    if(nrow(country_data) > 0) {
      # Extraer tipo de café para cada celda
      coffee_types <- values(coffee_type_raster)[country_data$cell_id]
      data.frame(
        cell_id = country_data$cell_id, 
        country = country$NAME,
        coffee_type = coffee_types
      )
    } else NULL
  })
  
  bind_rows(country_cells)
}

# --- PASO 2: FUNCIÓN PARA PROCESAR UN PERÍODO ---
process_period <- function(year_start, year_end, coffee_mask, country_cells, cell_mapping) {
  
  cat("\n=== PROCESANDO PERÍODO", year_start, "-", year_end, "===\n")
  
  # Construir nombres de archivos
  tasmax_file <- file.path(base_path2, 
                           sprintf("20crv3-era5_obsclim_tasmax_global_daily_%d_%d.nc", year_start, year_end))
  tasmin_file <- file.path(base_path3, 
                           sprintf("20crv3-era5_obsclim_tasmin_global_daily_%d_%d.nc", year_start, year_end))
  
  if (!file.exists(tasmax_file) || !file.exists(tasmin_file)) {
    cat("ADVERTENCIA: Archivos no encontrados para", year_start, "-", year_end, "\n")
    return(NULL)
  }
  
  # Cargar datos de temperatura
  tasmax <- rast(tasmax_file)
  tasmin <- rast(tasmin_file)
  
  # Extraer fechas
  nc <- nc_open(tasmax_file)
  time <- ncvar_get(nc, "time")
  units <- ncatt_get(nc, "time", "units")$value
  origin <- as.Date(sub("days since ", "", units))
  dates <- as.Date(origin + time)
  nc_close(nc)
  
  # Aplicar máscara y convertir a Celsius
  tasmax <- mask(tasmax, coffee_mask) - 273.15
  tasmin <- mask(tasmin, coffee_mask) - 273.15
  
  # Procesar por año
  all_monthly <- list()
  coffee_cells_all <- unique(country_cells$cell_id)
  
  for(year in year_start:year_end) {
    cat("  Año:", year, "\n")
    
    year_indices <- which(year(dates) == year)
    year_dates <- dates[year_indices]
    
    tasmax_year <- values(tasmax[[year_indices]])[coffee_cells_all, , drop = FALSE]
    tasmin_year <- values(tasmin[[year_indices]])[coffee_cells_all, , drop = FALSE]
    
    monthly_year <- lapply(unique(country_cells$country), function(country_name) {
      country_cells_info <- country_cells %>%
        filter(country == country_name)
      
      country_positions <- cell_mapping %>%
        filter(cell_id %in% country_cells_info$cell_id) %>%
        pull(matrix_position)
      
      if(length(country_positions) == 0) return(NULL)
      
      # Obtener tipos de café para este país
      coffee_types_country <- country_cells_info$coffee_type
      
      tmax_country <- tasmax_year[country_positions, , drop = FALSE]
      tmin_country <- tasmin_year[country_positions, , drop = FALSE]
      
      # Calcular HDD diferenciado por tipo de café
      hdd_matrix <- matrix(0, nrow = nrow(tmax_country), ncol = ncol(tmax_country))
      
      for(cell_idx in 1:nrow(tmax_country)) {
        # Determinar k4 según tipo de café
        k4_cell <- ifelse(coffee_types_country[cell_idx] == 1, k4_arabica, k4_robusta)
        
        tmin_cell <- tmin_country[cell_idx, ]
        tmax_cell <- tmax_country[cell_idx, ]
        
        # Calcular HDD con k4 específico
        hdd_matrix[cell_idx, ] <- case_when(
          tmin_cell > k4_cell ~ 1,
          tmin_cell < k4_cell & tmax_cell > k4_cell ~ 1 - (k4_cell - tmin_cell)/(tmax_cell - tmin_cell),
          tmax_cell < k4_cell ~ 0,
          TRUE ~ 0
        )
      }
      
      # Promediar sobre todas las celdas del país
      daily_tmax <- colMeans(tmax_country, na.rm = TRUE)
      daily_tmin <- colMeans(tmin_country, na.rm = TRUE)
      daily_hdd <- colMeans(hdd_matrix, na.rm = TRUE)
      
      # Calcular proporción de cada tipo de café
      arabica_prop <- mean(coffee_types_country == 1, na.rm = TRUE)
      
      data.frame(
        date = year_dates,
        year = year(year_dates),
        month_num = month(year_dates),
        country = country_name,
        tmax = daily_tmax,
        tmin = daily_tmin,
        arabica_proportion = arabica_prop
      ) %>%
        mutate(
          gdd = case_when(
            tmax < k2 ~ 0,
            tmin < k2 & tmax >= k2 & tmax < k3 ~ 1 - (k2 - tmin)/(tmax - tmin),
            tmin < k2 & tmax >= k3 ~ (k3 - k2)/(tmax - tmin),
            tmin >= k2 & tmax < k3 ~ 1,
            tmin >= k2 & tmin < k3 & tmax >= k3 ~ (k3 - tmin)/(tmax - tmin),
            tmin > k3 ~ 0,
            TRUE ~ 0
          ),
          hdd = daily_hdd,  # Usar HDD pre-calculado con umbrales diferenciados
          fdd = case_when(
            tmin > k1 ~ 0,
            tmin < k1 & tmax > k1 ~ (k1 - tmin)/(tmax - tmin),
            tmax < k1 ~ 1,
            TRUE ~ 0
          )
        ) %>%
        group_by(country, year, month_num) %>%
        summarise(
          date = min(date),
          tmax_ymc = max(tmax, na.rm = TRUE),
          tmin_ymc = min(tmin, na.rm = TRUE),
          gdd_ymc = sum(gdd, na.rm = TRUE),
          hdd_ymc = sum(hdd, na.rm = TRUE),
          fdd_ymc = sum(fdd, na.rm = TRUE),
          arabica_proportion = first(arabica_proportion),
          .groups = 'drop'
        )
    })
    
    all_monthly[[as.character(year)]] <- bind_rows(monthly_year)
  }
  
  result <- bind_rows(all_monthly) %>% arrange(country, date)
  cat("  Período completado:", nrow(result), "observaciones\n")
  
  return(result)
}

# --- PASO 3: PROCESAR TODOS LOS PERÍODOS ---
cat("\n=== INICIANDO PROCESAMIENTO MULTI-PERÍODO ===\n")

# Preparar máscara con el primer archivo disponible
first_period <- periods[[length(periods)]]
tasmax_ref <- rast(file.path(base_path2, 
                             sprintf("20crv3-era5_obsclim_tasmax_global_daily_%d_%d.nc", first_period[1], first_period[2])))

coffee_area <- resample(coffee_area, tasmax_ref, method = "sum")
coffee_type <- resample(coffee_type, tasmax_ref, method = "near")  # Usar "near" para mantener categorías

coffee_mask <- coffee_area
coffee_mask[coffee_mask > 0] <- 1
coffee_mask[coffee_mask == 0] <- NA
coffee_cell <- which(!is.na(values(coffee_mask)) & values(coffee_mask) == 1)

# Identificar países cafetaleros
coffee_by_country <- terra::extract(coffee_area, countries, fun = sum, na.rm = TRUE, bind = TRUE)
coffee_countries <- st_as_sf(coffee_by_country) %>% 
  filter(coffee_area_ha > 0) %>%
  arrange(desc(coffee_area_ha))

cat("Países cafetaleros:", nrow(coffee_countries), "\n")

# Identificar celdas por país (INCLUYE TIPO DE CAFÉ)
country_cells <- identify_country_cells(coffee_countries, coffee_area, coffee_type, coffee_cell)
coffee_cells_all <- unique(country_cells$cell_id)
cell_mapping <- data.frame(
  cell_id = coffee_cells_all,
  matrix_position = 1:length(coffee_cells_all)
)

cat("Celdas únicas con café:", length(coffee_cells_all), "\n")
cat("  - Celdas Arábica:", sum(country_cells$coffee_type == 1, na.rm = TRUE), "\n")
cat("  - Celdas Robusta:", sum(country_cells$coffee_type == 2, na.rm = TRUE), "\n")

# Procesar cada período
all_results <- list()

for(i in seq_along(periods)) {
  period <- periods[[i]]
  result <- process_period(period[1], period[2], coffee_mask, country_cells, cell_mapping)
  
  if(!is.null(result)) {
    all_results[[i]] <- result
    
    # Guardar resultado individual
    output_file <- file.path(output_path, 
                             sprintf("df_gdd_%d_%d_diff.rds", period[1], period[2]))
    saveRDS(result, output_file)
    cat("  Guardado:", output_file, "\n")
  }
}

# --- PASO 4: COMBINAR Y GUARDAR RESULTADO COMPLETO ---
if(length(all_results) > 0) {
  complete_data <- bind_rows(all_results) %>% arrange(country, date)
  
  output_complete <- file.path(output_path, "df_gdd_complete_1961_2020_v3.rds")
  saveRDS(complete_data, output_complete)
  
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  cat("Total de observaciones:", nrow(complete_data), "\n")
  cat("Países:", n_distinct(complete_data$country), "\n")
  cat("Rango:", min(complete_data$date), "a", max(complete_data$date), "\n")
  cat("Archivo completo guardado:", output_complete, "\n")
  
  # Resumen por tipo de café
  cat("\nDISTRIBUCIÓN POR TIPO DE CAFÉ:\n")
  summary_by_type <- complete_data %>%
    group_by(country) %>%
    summarise(arabica_prop = mean(arabica_proportion, na.rm = TRUE)) %>%
    mutate(dominant_type = ifelse(arabica_prop > 0.5, "Arábica", "Robusta"))
  
  cat("Países con Arábica dominante:", sum(summary_by_type$dominant_type == "Arábica"), "\n")
  cat("Países con Robusta dominante:", sum(summary_by_type$dominant_type == "Robusta"), "\n")
}