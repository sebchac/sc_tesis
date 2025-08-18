library(ncdf4)
library(lubridate)
library(rnaturalearth)
library(sf)
library(dplyr)
library(foreach)
library(doParallel)
library(progressr)

# 1. Configuración inicial -------------------------------------------------
folder_path <- "/Users/sebastianchacon/Desktop/ObsData/OriginalData/tas"
file_paths <- list.files(folder_path, pattern = "\\.nc$", full.names = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Lista completa de países de interés
exportCountriesICO <- c("Angola", "Benin", "Bolivia", "Brazil", "Burundi", "Cameroon",
                        "Central African Republic", "Colombia", "Congo", "Costa Rica",
                        "Côte d'Ivoire", "Cuba", "Dominican Republic", "Ecuador", "El Salvador",
                        "Ethiopia", "Gabon", "Ghana", "Guatemala", "Guinea", "Haiti", "Honduras",
                        "India", "Indonesia", "Jamaica", "Kenya", "Liberia", "Madagascar",
                        "Malawi", "Mexico", "Nicaragua", "Nigeria", "Panama", "Papua New Guinea",
                        "Peru", "Philippines", "Rwanda", "Sierra Leone", "Sri Lanka", "Tanzania",
                        "Thailand", "Togo", "Trinidad and Tobago", "Uganda", "Venezuela", "Viet Nam",
                        "Zambia", "Zimbabwe")

# 2. Función para precomputar la grilla espacial --------------------------
precompute_spatial_grid <- function(lon, lat, world) {
  message("Precomputando grilla espacial...")
  
  # 1. Crear la grilla y asignar CRS WGS84 (4326) inicial
  grid <- expand.grid(lon = lon, lat = lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # 2. Asegurar que el mapa de países esté en el mismo CRS que la grilla
  world <- st_transform(world, 4326)
  
  # 3. Transformar a CRS proyectado para cálculos métricos exactos
  grid_proj <- st_transform(grid, 3395)  # EPSG:3395 (Mercator) para buffers/áreas
  
  # 4. Crear buffers y calcular áreas
  grid_proj <- grid_proj %>%
    st_buffer(dist = 27500) %>%  # 27.5 km
    mutate(
      area_km2 = as.numeric(st_area(.)) / 1e6,
      id = 1:n()
    )
  
  # 5. Transformar de vuelta a WGS84 para el join espacial
  grid_wgs84 <- st_transform(grid_proj, 4326)
  
  # 6. Realizar el join espacial (asegurando mismo CRS)
  world_renamed <- world %>%
    mutate(
      name = case_when(
        name == "Democratic Republic of the Congo" ~ "Congo",
        name == "Vietnam" ~ "Viet Nam",
        name == "Dominican Rep." ~ "Dominican Republic",
        name == "Central African Rep." ~ "Central African Republic",
        TRUE ~ name
      )
    )
  
  grid_joined <- st_join(grid_wgs84, world_renamed["name"]) %>%
    filter(name %in% exportCountriesICO) 
  
  message("Grilla espacial precomputada con ", nrow(grid_joined), " puntos válidos")
  return(grid_joined)
}

# 3. Obtener dimensiones de los datos -------------------------------------
nc_sample <- nc_open(file_paths[1])
lon <- ncvar_get(nc_sample, "lon")
lat <- ncvar_get(nc_sample, "lat")
nc_close(nc_sample)

# 4. Precomputar la grilla espacial (una sola vez) ------------------------
grid_data <- precompute_spatial_grid(lon, lat, world)

# 5. Configurar paralelización y progreso ---------------------------------
n_cores <- detectCores() - 1  # Dejar un núcleo libre
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Habilitar el sistema de progreso
handlers(global = TRUE)
handlers(list(
  handler_progress(
    format = ":spin :current/:total (:message) [:bar] :percent in :elapsedfull",
    width = 60
  )
))

# 6. Función para procesar un archivo -------------------------------------
process_nc_file <- function(file_path, grid_data, exportCountries) {
  nc_data <- nc_open(file_path)
  time_values <- ncvar_get(nc_data, "time")
  dates <- as.Date("1900-01-01") + time_values
  
  df_file <- data.frame()
  
  for (day_index in seq_along(dates)) {
    current_date <- dates[day_index]
    tas_day <- ncvar_get(nc_data, "tas", 
                         start = c(1, 1, day_index), 
                         count = c(-1, -1, 1)) - 273.15  # Kelvin a Celsius
    
    # Asignar temperaturas a la grilla precomputada
    day_data <- grid_data %>%
      mutate(tas = as.vector(tas_day)[id]) %>%
      filter(!is.na(tas))
    
    # Clasificación de días
    day_results <- day_data %>%
      mutate(
        fdd = ifelse(tas <= 15, 1, 0),
        gdd = ifelse(tas > 15 & tas <= 25, 1, 0),
        hdd = ifelse(tas > 25, 1, 0),
        fdd_weighted = fdd * area_km2,
        gdd_weighted = gdd * area_km2,
        hdd_weighted = hdd * area_km2
      ) %>%
      st_drop_geometry() %>%
      group_by(name) %>%
      summarise(
        total_area = sum(area_km2, na.rm = TRUE),
        sum_fdd = sum(fdd_weighted, na.rm = TRUE),
        sum_gdd = sum(gdd_weighted, na.rm = TRUE),
        sum_hdd = sum(hdd_weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        date = current_date,
        year = year(current_date),
        month_num = month(current_date)
      ) %>%
      rename(country = name) %>%
      filter(country %in% exportCountries)
    
    df_file <- bind_rows(df_file, day_results)
  }
  
  nc_close(nc_data)
  return(df_file)
}

# 7. Procesamiento paralelo con barra de progreso -------------------------
message("Iniciando procesamiento paralelo de ", length(file_paths), " archivos...")

with_progress({
  p <- progressor(along = file_paths)
  
  df_final_new <- foreach(
    file_path = file_paths,
    .combine = bind_rows,
    .packages = c("ncdf4", "lubridate", "sf", "dplyr"),
    .export = c("process_nc_file", "grid_data", "exportCountriesICO")
  ) %dopar% {
    result <- process_nc_file(file_path, grid_data, exportCountriesICO)
    p(sprintf("Procesado: %s", basename(file_path)))  # Actualizar progreso
    result
  }
})

# 8. Cerrar cluster ------------------------------------------------------
stopCluster(cl)

# 9. Procesamiento final de los resultados --------------------------------
message("Procesando resultados finales...")

# Corrección de nombres de países
df_final_new <- df_final_new %>%
  mutate(country = recode(country,
                          "Central African Rep." = "Central African Republic",
                          "Dem. Rep. Congo" = "Congo",
                          "Dominican Rep." = "Dominican Republic",
                          "Vietnam" = "Viet Nam"))

# Agregación mensual
df_monthly <- df_final_new %>%
  group_by(country, year, month_num) %>%
  summarise(
    gdd_ymc = sum(sum_gdd / total_area, na.rm = TRUE),
    hdd_ymc = sum(sum_hdd / total_area, na.rm = TRUE),
    fdd_ymc = sum(sum_fdd / total_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, year, month_num)

# 10. Guardar resultados -------------------------------------------------
output_path <- "/Users/sebastianchacon/Desktop/ObsData/ProcessedData/df_tas_daily_indicators.rds"
saveRDS(df_monthly, output_path)

message("Proceso completado. Resultados guardados en: ", output_path)