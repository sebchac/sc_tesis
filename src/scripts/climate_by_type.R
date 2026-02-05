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
k3 <- 28

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

# Cargar datos SPAM (mantener separados con sus hectáreas)
arabica <- rast(file.path(base_path1, "spam2020_V2r0_global_H_COFF_A.tif"))
robusta <- rast(file.path(base_path1, "spam2020_V2r0_global_H_RCOF_A.tif"))

# Reemplazar NAs con 0
arabica[is.na(arabica)] <- 0
robusta[is.na(robusta)] <- 0

# Combinar para área total
coffee_area <- arabica + robusta
names(coffee_area) <- "coffee_area_ha"

cat("Total hectáreas Arábica:", round(global(arabica, "sum", na.rm = TRUE)[1,1]), "\n")
cat("Total hectáreas Robusta:", round(global(robusta, "sum", na.rm = TRUE)[1,1]), "\n")

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

# Función para identificar celdas por país CON hectáreas por tipo
identify_country_cells <- function(country_sf, coffee_raster, arabica_raster, robusta_raster, coffee_cell) {
  cell_id_raster <- coffee_raster
  values(cell_id_raster) <- 1:ncell(cell_id_raster)
  names(cell_id_raster) <- "cell_id"
  
  country_cells <- lapply(1:nrow(country_sf), function(i) {
    country <- country_sf[i, ]
    country_cells_ids <- terra::extract(cell_id_raster, country, na.rm = TRUE)
    country_data <- country_cells_ids %>% filter(cell_id %in% coffee_cell)
    
    if(nrow(country_data) > 0) {
      # Extraer hectáreas de cada tipo para cada celda
      arabica_ha <- values(arabica_raster)[country_data$cell_id]
      robusta_ha <- values(robusta_raster)[country_data$cell_id]
      
      data.frame(
        cell_id = country_data$cell_id, 
        country = country$NAME,
        arabica_ha = arabica_ha,
        robusta_ha = robusta_ha,
        total_ha = arabica_ha + robusta_ha
      )
    } else NULL
  })
  
  bind_rows(country_cells)
}

# --- PASO 2: FUNCIÓN PARA PROCESAR UN PERÍODO ---
# MODIFICACIÓN PRINCIPAL: Ahora genera dos filas por país-mes (una por especie)
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
      
      # Obtener hectáreas por tipo para cada celda
      arabica_ha_vec <- country_cells_info$arabica_ha
      robusta_ha_vec <- country_cells_info$robusta_ha
      
      # Calcular totales POR ESPECIE
      total_ha_arabica <- sum(arabica_ha_vec)
      total_ha_robusta <- sum(robusta_ha_vec)
      
      tmax_country <- tasmax_year[country_positions, , drop = FALSE]
      tmin_country <- tasmin_year[country_positions, , drop = FALSE]
      
      # Inicializar matrices para acumular índices por ESPECIE
      n_days <- ncol(tmax_country)
      n_cells <- nrow(tmax_country)
      
      # Matrices separadas para cada especie
      gdd_arabica_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      hdd_arabica_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      fdd_arabica_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      
      gdd_robusta_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      hdd_robusta_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      fdd_robusta_weighted <- matrix(0, nrow = n_cells, ncol = n_days)
      
      # Procesar cada celda
      for(cell_idx in 1:n_cells) {
        tmin_cell <- tmin_country[cell_idx, ]
        tmax_cell <- tmax_country[cell_idx, ]
        
        ha_arabica <- arabica_ha_vec[cell_idx]
        ha_robusta <- robusta_ha_vec[cell_idx]
        
        # Calcular GDD (igual para ambos tipos)
        gdd_cell <- case_when(
          tmax_cell < k2 ~ 0,
          tmin_cell < k2 & tmax_cell >= k2 & tmax_cell < k3 ~ 1 - (k2 - tmin_cell)/(tmax_cell - tmin_cell),
          tmin_cell < k2 & tmax_cell >= k3 ~ (k3 - k2)/(tmax_cell - tmin_cell),
          tmin_cell >= k2 & tmax_cell < k3 ~ 1,
          tmin_cell >= k2 & tmin_cell < k3 & tmax_cell >= k3 ~ (k3 - tmin_cell)/(tmax_cell - tmin_cell),
          tmin_cell > k3 ~ 0,
          TRUE ~ 0
        )
        
        # Calcular FDD (igual para ambos tipos)
        fdd_cell <- case_when(
          tmin_cell > k1 ~ 0,
          tmin_cell < k1 & tmax_cell > k1 ~ (k1 - tmin_cell)/(tmax_cell - tmin_cell),
          tmax_cell < k1 ~ 1,
          TRUE ~ 0
        )
        
        # Calcular HDD para Arábica (k4 = 26)
        hdd_arabica <- case_when(
          tmin_cell > k4_arabica ~ 1,
          tmin_cell < k4_arabica & tmax_cell > k4_arabica ~ 1 - (k4_arabica - tmin_cell)/(tmax_cell - tmin_cell),
          tmax_cell < k4_arabica ~ 0,
          TRUE ~ 0
        )
        
        # Calcular HDD para Robusta (k4 = 30)
        hdd_robusta <- case_when(
          tmin_cell > k4_robusta ~ 1,
          tmin_cell < k4_robusta & tmax_cell > k4_robusta ~ 1 - (k4_robusta - tmin_cell)/(tmax_cell - tmin_cell),
          tmax_cell < k4_robusta ~ 0,
          TRUE ~ 0
        )
        
        # Ponderar por hectáreas DE CADA ESPECIE
        gdd_arabica_weighted[cell_idx, ] <- gdd_cell * ha_arabica
        fdd_arabica_weighted[cell_idx, ] <- fdd_cell * ha_arabica
        hdd_arabica_weighted[cell_idx, ] <- hdd_arabica * ha_arabica
        
        gdd_robusta_weighted[cell_idx, ] <- gdd_cell * ha_robusta
        fdd_robusta_weighted[cell_idx, ] <- fdd_cell * ha_robusta
        hdd_robusta_weighted[cell_idx, ] <- hdd_robusta * ha_robusta
      }
      
      # PARTE CRÍTICA: Crear dos data frames (uno por especie)
      
      # ------ ARÁBICA ------
      if(total_ha_arabica > 0) {
        daily_gdd_arabica <- colSums(gdd_arabica_weighted, na.rm = TRUE) / total_ha_arabica
        daily_hdd_arabica <- colSums(hdd_arabica_weighted, na.rm = TRUE) / total_ha_arabica
        daily_fdd_arabica <- colSums(fdd_arabica_weighted, na.rm = TRUE) / total_ha_arabica
        
        # Temperatura promedio ponderada solo por celdas con arábica
        arabica_cells_mask <- arabica_ha_vec > 0
        if(sum(arabica_cells_mask) > 0) {
          weights_arabica <- matrix(rep(arabica_ha_vec, n_days), ncol = n_days)
          daily_tmax_arabica <- colSums(tmax_country * weights_arabica, na.rm = TRUE) / total_ha_arabica
          daily_tmin_arabica <- colSums(tmin_country * weights_arabica, na.rm = TRUE) / total_ha_arabica
        } else {
          daily_tmax_arabica <- rep(NA, n_days)
          daily_tmin_arabica <- rep(NA, n_days)
        }
        
        df_arabica <- data.frame(
          date = year_dates,
          year = year(year_dates),
          month_num = month(year_dates),
          country = country_name,
          dummy_arabica = 1,  # Variable dummy
          tmax = daily_tmax_arabica,
          tmin = daily_tmin_arabica,
          gdd = daily_gdd_arabica,
          hdd = daily_hdd_arabica,
          fdd = daily_fdd_arabica,
          total_ha = total_ha_arabica
        ) %>%
          group_by(country, dummy_arabica, year, month_num) %>%
          summarise(
            date = min(date),
            tmax_ymc = max(tmax, na.rm = TRUE),
            tmin_ymc = min(tmin, na.rm = TRUE),
            gdd_ymc = sum(gdd, na.rm = TRUE),
            hdd_ymc = sum(hdd, na.rm = TRUE),
            fdd_ymc = sum(fdd, na.rm = TRUE),
            total_ha = first(total_ha),
            .groups = 'drop'
          )
      } else {
        df_arabica <- NULL
      }
      
      # ------ ROBUSTA ------
      if(total_ha_robusta > 0) {
        daily_gdd_robusta <- colSums(gdd_robusta_weighted, na.rm = TRUE) / total_ha_robusta
        daily_hdd_robusta <- colSums(hdd_robusta_weighted, na.rm = TRUE) / total_ha_robusta
        daily_fdd_robusta <- colSums(fdd_robusta_weighted, na.rm = TRUE) / total_ha_robusta
        
        # Temperatura promedio ponderada solo por celdas con robusta
        robusta_cells_mask <- robusta_ha_vec > 0
        if(sum(robusta_cells_mask) > 0) {
          weights_robusta <- matrix(rep(robusta_ha_vec, n_days), ncol = n_days)
          daily_tmax_robusta <- colSums(tmax_country * weights_robusta, na.rm = TRUE) / total_ha_robusta
          daily_tmin_robusta <- colSums(tmin_country * weights_robusta, na.rm = TRUE) / total_ha_robusta
        } else {
          daily_tmax_robusta <- rep(NA, n_days)
          daily_tmin_robusta <- rep(NA, n_days)
        }
        
        df_robusta <- data.frame(
          date = year_dates,
          year = year(year_dates),
          month_num = month(year_dates),
          country = country_name,
          dummy_arabica = 0,  # Variable dummy
          tmax = daily_tmax_robusta,
          tmin = daily_tmin_robusta,
          gdd = daily_gdd_robusta,
          hdd = daily_hdd_robusta,
          fdd = daily_fdd_robusta,
          total_ha = total_ha_robusta
        ) %>%
          group_by(country, dummy_arabica, year, month_num) %>%
          summarise(
            date = min(date),
            tmax_ymc = max(tmax, na.rm = TRUE),
            tmin_ymc = min(tmin, na.rm = TRUE),
            gdd_ymc = sum(gdd, na.rm = TRUE),
            hdd_ymc = sum(hdd, na.rm = TRUE),
            fdd_ymc = sum(fdd, na.rm = TRUE),
            total_ha = first(total_ha),
            .groups = 'drop'
          )
      } else {
        df_robusta <- NULL
      }
      
      # Combinar ambas especies
      bind_rows(df_arabica, df_robusta)
    })
    
    all_monthly[[as.character(year)]] <- bind_rows(monthly_year)
  }
  
  result <- bind_rows(all_monthly) %>% arrange(country, dummy_arabica, date)
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
arabica <- resample(arabica, tasmax_ref, method = "sum")
robusta <- resample(robusta, tasmax_ref, method = "sum")

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

# Identificar celdas por país CON hectáreas
country_cells <- identify_country_cells(coffee_countries, coffee_area, arabica, robusta, coffee_cell)
coffee_cells_all <- unique(country_cells$cell_id)
cell_mapping <- data.frame(
  cell_id = coffee_cells_all,
  matrix_position = 1:length(coffee_cells_all)
)

cat("Celdas únicas con café:", length(coffee_cells_all), "\n")
cat("  - Total hectáreas Arábica:", round(sum(country_cells$arabica_ha)), "\n")
cat("  - Total hectáreas Robusta:", round(sum(country_cells$robusta_ha)), "\n")

# Procesar cada período
all_results <- list()

for(i in seq_along(periods)) {
  period <- periods[[i]]
  result <- process_period(period[1], period[2], coffee_mask, country_cells, cell_mapping)
  
  if(!is.null(result)) {
    all_results[[i]] <- result
    
    # Guardar resultado individual
    output_file <- file.path(output_path, 
                             sprintf("df_gdd_%d_%d_by_species.rds", period[1], period[2]))
    saveRDS(result, output_file)
    cat("  Guardado:", output_file, "\n")
  }
}

# --- PASO 4: COMBINAR Y GUARDAR RESULTADO COMPLETO ---
if(length(all_results) > 0) {
  complete_data <- bind_rows(all_results) %>% arrange(country, dummy_arabica, date)
  
  output_complete <- file.path(output_path, "df_gdd_complete_1961_2020_by_species.rds")
  saveRDS(complete_data, output_complete)
  
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  cat("Total de observaciones:", nrow(complete_data), "\n")
  cat("Países:", n_distinct(complete_data$country), "\n")
  cat("Rango:", min(complete_data$date), "a", max(complete_data$date), "\n")
  cat("Archivo completo guardado:", output_complete, "\n")
  
  # Resumen por país Y ESPECIE
  cat("\nRESUMEN POR PAÍS Y ESPECIE (primeros 20):\n")
  summary_by_country_species <- complete_data %>%
    group_by(country, dummy_arabica) %>%
    summarise(
      total_ha = first(total_ha),
      n_obs = n(),
      .groups = 'drop'
    ) %>%
    mutate(especie = ifelse(dummy_arabica == 1, "Arábica", "Robusta")) %>%
    arrange(desc(total_ha)) %>%
    head(20)
  
  print(summary_by_country_species)
}