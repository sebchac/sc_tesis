library(terra)
library(dplyr)
library(lubridate)
library(sf)
library(ncdf4)

# --- CONFIGURACIÓN ---
base_path1 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/"
base_path2 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmax"
base_path3 <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/tasmin"
output_path <- "/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/"

# Parámetros de umbrales de temperatura
k1 <- 10; k2 <- 18; k3 <- 26; k4 <- 30

# Períodos a procesar (formato: inicio-fin)
periods <- list(
  c(1961, 1970),
  c(1971, 1980),
  c(1981, 1990),
  c(1991, 2000),
  c(2001, 2010),
  c(2011, 2020)
)

# --- PASO 1: PREPARAR MÁSCARA DE CAFÉ Y PAÍSES (UNA SOLA VEZ) ---
cat("=== PREPARACIÓN INICIAL ===\n")

# Cargar datos SPAM
arabica <- rast(file.path(base_path1, "spam2020_V2r0_global_H_COFF_A.tif"))
robusta <- rast(file.path(base_path1, "spam2020_V2r0_global_H_RCOF_A.tif"))

# Combinar y crear máscara
arabica[is.na(arabica)] <- 0
robusta[is.na(robusta)] <- 0
coffee_area <- arabica + robusta
names(coffee_area) <- "coffee_area_ha"

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

# Función para identificar celdas por país
identify_country_cells <- function(country_sf, coffee_raster, coffee_cell) {
  cell_id_raster <- coffee_raster
  values(cell_id_raster) <- 1:ncell(cell_id_raster)
  names(cell_id_raster) <- "cell_id"
  
  country_cells <- lapply(1:nrow(country_sf), function(i) {
    country <- country_sf[i, ]
    country_cells_ids <- terra::extract(cell_id_raster, country, na.rm = TRUE)
    country_data <- country_cells_ids %>% filter(cell_id %in% coffee_cell)
    
    if(nrow(country_data) > 0) {
      data.frame(cell_id = country_data$cell_id, country = country$NAME)
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
      country_positions <- cell_mapping %>%
        filter(cell_id %in% country_cells$cell_id[country_cells$country == country_name]) %>%
        pull(matrix_position)
      
      if(length(country_positions) == 0) return(NULL)
      
      tmax_country <- tasmax_year[country_positions, , drop = FALSE]
      tmin_country <- tasmin_year[country_positions, , drop = FALSE]
      
      daily_tmax <- colMeans(tmax_country, na.rm = TRUE)
      daily_tmin <- colMeans(tmin_country, na.rm = TRUE)
      
      data.frame(
        date = year_dates,
        year = year(year_dates),
        month_num = month(year_dates),
        country = country_name,
        tmax = daily_tmax,
        tmin = daily_tmin
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
          hdd = case_when(
            tmin >= k4 ~ 1,
            tmin < k4 & tmax >= k4 ~ 1 - (k4 - tmin)/(tmax - tmin),
            tmax < k4 ~ 0,
            TRUE ~ 0
          ),
          fdd = case_when(
            tmin > k1 ~ 0,
            tmin <= k1 & tmax > k1 ~ (k1 - tmin)/(tmax - tmin),
            tmax <= k1 ~ 1,
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

# Preparar máscara con el primer archivo disponible (para dimensiones)
first_period <- periods[[length(periods)]]  # Usar el más reciente
tasmax_ref <- rast(file.path(base_path2, 
                             sprintf("20crv3-era5_obsclim_tasmax_global_daily_%d_%d.nc", first_period[1], first_period[2])))

coffee_area <- resample(coffee_area, tasmax_ref, method = "sum")
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

# Identificar celdas por país
country_cells <- identify_country_cells(coffee_countries, coffee_area, coffee_cell)
coffee_cells_all <- unique(country_cells$cell_id)
cell_mapping <- data.frame(
  cell_id = coffee_cells_all,
  matrix_position = 1:length(coffee_cells_all)
)

cat("Celdas únicas con café:", length(coffee_cells_all), "\n")

# Procesar cada período
all_results <- list()

for(i in seq_along(periods)) {
  period <- periods[[i]]
  result <- process_period(period[1], period[2], coffee_mask, country_cells, cell_mapping)
  
  if(!is.null(result)) {
    all_results[[i]] <- result
    
    # Guardar resultado individual
    output_file <- file.path(output_path, 
                             sprintf("df_gdd_%d_%d.rds", period[1], period[2]))
    saveRDS(result, output_file)
    cat("  Guardado:", output_file, "\n")
  }
}

# --- PASO 4: COMBINAR Y GUARDAR RESULTADO COMPLETO ---
if(length(all_results) > 0) {
  complete_data <- bind_rows(all_results) %>% arrange(country, date)
  
  output_complete <- file.path(output_path, "df_gdd_complete_1961_2020.rds")
  saveRDS(complete_data, output_complete)
  
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  cat("Total de observaciones:", nrow(complete_data), "\n")
  cat("Países:", n_distinct(complete_data$country), "\n")
  cat("Rango:", min(complete_data$date), "a", max(complete_data$date), "\n")
  cat("Archivo completo guardado:", output_complete, "\n")
}