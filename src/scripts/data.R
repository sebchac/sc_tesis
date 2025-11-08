#----------
# [0] Preliminary
#----------

# Packages

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(haven)

# Input files

filename_gdp       <- "pwt1001.xlsx"
filename_igrea     <- "igrea.xlsx"    
filename_icip      <- "PMJM - ICIP.xlsx"  
filename_growers   <- "PMJM- Growers.xlsx"
filename_psdcoffee <- "psd_coffee.csv"        
filename_tea       <- "CMO-Historical-Data-Monthly.xlsx"
filename_retail    <- "CUUR0000SEFP01.xls"
filename_ndgain    <- "gain.csv"
filename_soi       <- "soi.txt"
filename_isimip_y  <- "df_clima_y.rds"
filename_isimip_ym <- "df_clima_ym.rds"
filename_cpi       <- "CPIAUCSL.csv"
filename_oni       <- "oni.xlsx"

filename_tas_ymc   <- "df_tas_monthly.rds"
filename_pr_ymc    <- "df_pr_monthly.rds"

filename_tas_gdd   <- "df_tas_daily_indicators_2.rds"

filename_gdd       <- "df_monthly_gdd.rds"

main_dir <- "/Users/sebastianchacon/Desktop/sc_tesis/src/original_data/"

filepath_gdp       <- paste(main_dir, filename_gdp,sep="") 
filepath_igrea     <- paste(main_dir, filename_igrea,sep="") 
filepath_icip      <- paste(main_dir, filename_icip,sep="") 
filepath_growers   <- paste(main_dir, filename_growers,sep="") 
filepath_psdcoffee <- paste(main_dir, filename_psdcoffee,sep="") 
filepath_tea       <- paste(main_dir, filename_tea,sep="")   
filepath_retail    <- paste(main_dir, filename_retail,sep="")   
filepath_ndgain    <- paste(main_dir, filename_ndgain,sep="")
filepath_soi       <- paste(main_dir, filename_soi, sep = "")
filepath_isimip_ym <- paste(main_dir, filename_isimip_ym, sep = "")
filepath_isimip_y  <- paste(main_dir, filename_isimip_y, sep = "")
filepath_cpi       <- paste(main_dir, filename_cpi, sep = "")
filepath_oni       <- paste(main_dir, filename_oni, sep = "")

filepath_tas_ymc   <- paste(main_dir, filename_tas_ymc, sep = "")
filepath_pr_ymc    <- paste(main_dir, filename_pr_ymc, sep = "")

filepath_tas_gdd   <- paste(main_dir, filename_tas_gdd, sep = "")

filepath_gdd       <- paste(main_dir, filename_gdd, sep = "")

# Output files

main_out <- "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/"

filepath_psd       <- paste(main_out, "psd_data.rds", sep = "")

# Some functions

create_month <- function(db) {

  db$year = format(db$date, "%Y")
  db$year = as.double(db$year)
  db$month = format(db$date, "%m")
  db$month_num = 0
  db$month_num[db$month == "01"] = 1
  db$month_num[db$month == "02"] = 2
  db$month_num[db$month == "03"] = 3
  db$month_num[db$month == "04"] = 4
  db$month_num[db$month == "05"] = 5
  db$month_num[db$month == "06"] = 6
  db$month_num[db$month == "07"] = 7
  db$month_num[db$month == "08"] = 8
  db$month_num[db$month == "09"] = 9
  db$month_num[db$month == "10"] = 10
  db$month_num[db$month == "11"] = 11
  db$month_num[db$month == "12"] = 12
  db$month_num = as.double(db$month_num)
  
  db$month = NULL
  db$date = NULL
  
  return(db)
}

#-------------------------------------------------------------------------------
################################### [1] Variables ##############################
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# [1.0] CPI
#-------------------------------------------------------------------------------
cpi <- read_csv(filepath_cpi) 
cpi <- cpi %>%
  rename(date = observation_date, cpi_0 = CPIAUCSL) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month_num = month(date)
  )

# Factor de conversión
factor_2017 <- mean(cpi$cpi_0[cpi$year == 2017], na.rm = TRUE)
factor_2017 <- 100 / factor_2017

# Convertir a base 2017 = 100
cpi <- cpi %>%
  mutate(cpi_2017 = cpi_0 * factor_2017) %>%
  select(year, month_num, cpi_2017)

#-------------------------------------------------------------------------------
# [1.1] Countries
#-------------------------------------------------------------------------------

# Read .csv and filter by operations
psd_coffee <- read_csv(filepath_psdcoffee)
psd_coffee <- psd_coffee %>%
  select(Country_Name, Market_Year, Attribute_Description, Attribute_ID, Value) %>%
  rename(country = Country_Name, year = Market_Year, operation = Attribute_Description,
         id = Attribute_ID, value = Value) %>%
  mutate(country = if_else(country == "Congo (Brazzaville)","Congo",
                   if_else(country == "Congo (Kinshasa)","Congo",
                   if_else(country == "Yemen (Sanaa)", "Yemen",
                   if_else(country == "Vietnam","Viet Nam",
                   if_else(country == "Cote d'Ivoire","Côte d'Ivoire", country)))))) %>%
  filter(value > 0 & (id == "090" | id == "058")) # id: 57 (imports); 88 (exports) ; 58 (bean imports) ; 90 (bean exports) 

# We sum both Congos
psd_coffee <- psd_coffee %>%
  group_by(year, country, operation, id) %>%
  summarise( value = sum(value))

# These are just lists of countries
# All export countries
exportCountries <- psd_coffee %>%
  ungroup() %>%
  filter(id=="090") %>%
  distinct(country) %>%
  pull(country)
# All import countries
importCountries <- psd_coffee %>%
  ungroup() %>%
  filter(id=="058") %>%
  distinct(country) %>%
  pull(country)
# European Union
europeanUnion <- c("Belgium","Bulgaria","Czech Republic","Denmark","Germany","Estonia",
                   "Ireland","Greece","Spain","France","Croatia","Italy","Cyprus",
                   "Latvia","Lithuania","Luxembourg","Hungary","Malta","Netherlands", 
                   "Austria","Poland","Portugal","Romania","Slovenia","Slovakia",
                   "Finland","Sweden")
# Import Countries with European Union
importCountriesGDP <- c(importCountries,europeanUnion)
importCountriesGDP <- subset(importCountriesGDP, importCountriesGDP != "European Union")

saveRDS(psd_coffee, filepath_psd)

#-------------------------------------------------------------------------------
# [1.2] Prices (ICIP - US cents per lb) (1965-2023)
#-------------------------------------------------------------------------------

prices <- read_excel(filepath_icip, skip=5)
prices <- prices %>% 
  select(-Average) %>%
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "price") %>%
  mutate(month = match(month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         date = as.Date(paste(Year, month, 1, sep = "-"))
  ) %>%
  select(date, price, Year  ) %>%
  rename(year = Year, price_ym = price)

full_prices <- create_month(prices)
full_prices <- full_prices %>% 
  left_join(cpi, by = c("year", "month_num")) %>%
  mutate(price_ym = price_ym * (100 / cpi_2017)) %>%
  select(year, month_num, price_ym)

full_prices_annual <- full_prices %>%
  group_by(year) %>%
  mutate(price_y = mean(price_ym, na.rm = TRUE)) %>%
  distinct(year, price_y)


#-------------------------------------------------------------------------------
# [1.3] Exports (1000 60 kg bags) (1960-2023)
#-------------------------------------------------------------------------------

exports <- psd_coffee %>%
  filter(value > 0 & id == "090") %>%
  mutate(value = value/1000) %>% # To get millions 60 kg bags as Igami
  rename(qe_yc = value) %>%
  ungroup() %>%
  select(-c(id, operation)) %>%
  tidyr::expand(nesting(year, country, qe_yc), month_num = 1:12) %>%
  mutate(qe_ymc = qe_yc / 12) %>%
  group_by(year, month_num) %>%
  mutate(qe_ym = sum(qe_ymc)) %>%
  ungroup() %>%
  mutate(qe_y = qe_ym * 12,
         share_yce = qe_yc / qe_y, 
         share_ymce = qe_ymc / qe_ym)

# Creates dummies export
exports$export_dummy = 0
for (x in exportCountries){
  exports$export_dummy[exports$country == x] = 1
}

# Consolidates
full_export <- exports

#-------------------------------------------------------------------------------
# [1.4] Imports (1000 60 kg bags) (1960-2023)
#-------------------------------------------------------------------------------

imports <- psd_coffee %>%
  filter(value > 0 & id == "058") %>%
  mutate(value = value/1000) %>% # To get millions 60 kg bags as Igami
  rename(qi_yc = value) %>%
  ungroup() %>%
  select(-c(id, operation)) %>%
  tidyr::expand(nesting(year, country, qi_yc), month_num = 1:12) %>%
  mutate(qi_ymc = qi_yc / 12) %>%
  group_by(year, month_num) %>%
  mutate(qi_ym = sum(qi_ymc)) %>%
  ungroup() %>%
  mutate(qi_y = qi_ym * 12,
         share_yci = qi_yc / qi_y,
         share_ymci = qi_ymc / qi_ym)

# Creates dummies export
imports$import_dummy = 0
for (x in importCountries){
  imports$import_dummy[imports$country == x] = 1
}

# Consolidates
full_import <- imports

#-------------------------------------------------------------------------------
# [1.5] Farm gate prices (Prices Paid to Growers - US cents per lb)
#-------------------------------------------------------------------------------

sheets <- c(1, 2, 3, 4)

growers_list <- list()

for (sheet in sheets) {
  growers <- read_excel(filepath_growers, sheet = sheet, skip = 4) %>%
    rename(country = Country,
           type = `Type of Coffee`) %>%
    select(-c(Currency)) %>%
    pivot_longer(cols = -c(country, type),
                 names_to = "date", 
                 values_to = "farm_prices_ymc") %>%
    mutate(month = str_split(date, "-") %>% sapply(`[`, 1),
           year = str_split(date, "-") %>% sapply(`[`, 2),
           month = match(month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
           date = as.Date(paste(year, month, 1, sep = "-"))) %>%
    select(country, type, date, farm_prices_ymc, year)
  
  growers_list[[sheet]] <- growers
}

growers <- bind_rows(growers_list)
growers <- na.omit(growers)
growers <- growers %>% 
  arrange(country, date, type) %>%
  filter(farm_prices_ymc > 0) %>%
  mutate(country = if_else(country == "Bolivia (Plurinational State of)","Bolivia", 
                           if_else(country == "Democratic Republic of Congo","Congo",
                                   if_else(country == "Trinidad & Tobago", "Trinidad and Tobago", country)))) %>%
  group_by(country, date) %>%
  mutate(farm_prices_ymc = mean(farm_prices_ymc)) %>%
  ungroup() %>%
  distinct(country, date, farm_prices_ymc)

growers <- create_month(growers)

full_farm_prices <- growers %>%
  left_join(cpi, by = c("year", "month_num")) %>%
  mutate(farm_prices_ymc = farm_prices_ymc * (100 / cpi_2017)) %>%
  filter(farm_prices_ymc <= 1500) %>%
  select(-cpi_2017) %>%
  ungroup() %>%
  group_by(country, year) %>%
  mutate(farm_prices_yc = mean(farm_prices_ymc, na.rm = TRUE)) %>%
  ungroup()

#-------------------------------------------------------------------------------
# [1.6] Buyers' GDP (Real GDP at constant 2017 national prices (in mil. 2017US$))
#-------------------------------------------------------------------------------

gdp <- read_excel(filepath_gdp, sheet="Data")
gdp <- gdp %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(country = if_else(country == "Iran (Islamic Republic of)","Iran",
                   if_else(country == "Republic of Korea","Korea, South",
                   if_else(country == "Russian Federation", "Russian",
                   if_else(country == "Venezuela (Bolivarian Republic of)","Venezuela",
                   if_else(country %in% europeanUnion, "European Union", country))))))

# GDP per country
gdp_yc <- gdp %>%
  mutate(rgdpe_yc = log(rgdpe),
         rgdpna_yc = log(rgdpna)) %>%
  select(country, year, rgdpe_yc, rgdpna_yc) 
  
# Creates dummies importsGDP
gdp$import_dummy_gdp = 0
for (x in importCountries){
  gdp$import_dummy_gdp[gdp$country == x] = 1
}

gdp <- gdp[gdp$import_dummy_gdp == 1,]

gdp <- gdp %>%
  select(country, year, rgdpe) %>% # No information for Cuba, European Union (each country instead), Kosovo
  group_by(year) %>% 
  mutate(importGDP_y = log(sum(rgdpe)/1000000),
         importGDP_ym = log(sum(rgdpe)/12000000)) %>% # Log of trillion US dollars
  ungroup() %>%
  distinct(year, importGDP_y, importGDP_ym)
#-------------------------------------------------------------------------------
# [1.7] Tea prices (Nominal US dollars ($/kg))
#-------------------------------------------------------------------------------

tea <- read_excel(filepath_tea, 
                  sheet = "Monthly Prices", skip = 6)
tea <- tea %>%
  select(...1, TEA_AVG) %>%
  rename(teaPrices_ym = TEA_AVG, date = ...1) %>%
  mutate(teaPrices_ym = as.numeric(teaPrices_ym, na.rm = TRUE)) %>%
  mutate(
    year      = as.double(substr(date, 1, 4)),
    month_num = as.double(substr(date, 6, 7))  
    ) %>%
  select(-date)

 tea <- tea %>%
   left_join(cpi, by = c("year", "month_num")) %>%
   mutate(teaPrices_ym = teaPrices_ym * (100 / cpi_2017),
          teaPrices_ym = teaPrices_ym * 100) %>% # To cents per kg
   select(-cpi_2017)
 
 tea_annual <- tea %>%
   group_by(year) %>%
   mutate(teaPrices_y = mean(teaPrices_ym, na.rm = TRUE)) %>%
   ungroup() %>%
   distinct(year, teaPrices_y)

#-------------------------------------------------------------------------------
# [1.8] ISIMIP Data
#-------------------------------------------------------------------------------

# data_clima_ym <- readRDS(filepath_isimip_ym)
# data_clima_y <- readRDS(filepath_isimip_y)
# 
# data_clima_y <- data_clima_y %>%
#   group_by(year, country) %>%
#   summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
# 
# umbral_cf1_min <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 0.05, na.rm = TRUE)
# umbral_cf1_max <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 0.95, na.rm = TRUE)
# umbral_cf2_min <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 0.1, na.rm = TRUE)
# umbral_cf2_max <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 0.9, na.rm = TRUE)
# umbral_min <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 0, na.rm = TRUE)
# umbral_max <- quantile(data_clima_ym$tas_ymc_shock_h24_p24, probs = 1, na.rm = TRUE)
# 
# data_clima_ym <- data_clima_ym %>%
#   mutate(tas_cf1 = if_else(tas_ymc_shock_h24_p24 < umbral_cf1_min, umbral_cf1_min - 0.5*(umbral_cf1_min - tas_ymc_shock_h24_p24),
#                            if_else(tas_ymc_shock_h24_p24 > umbral_cf1_max, umbral_cf1_max + 0.5*(tas_ymc_shock_h24_p24 - umbral_cf1_max), tas_ymc_shock_h24_p24)),
#          tas_cf2 = if_else(tas_ymc_shock_h24_p24 < umbral_cf2_min, umbral_cf2_min,
#                            if_else(tas_ymc_shock_h24_p24 > umbral_cf2_max, umbral_cf2_max, tas_ymc_shock_h24_p24)),
#          tas_cf3 = tas_ymc_shock_h24_p24 + 1,
#          tas_cf4 = tas_ymc_shock_h24_p24 + 2)

data_tas_ymc <- readRDS(filepath_tas_ymc)
data_pr_ymc <- readRDS(filepath_pr_ymc)

# We sum both Congos
data_tas_ymc <- data_tas_ymc %>%
  rename(tas_ymc = tas) %>%
  group_by(year, month_num, country) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = 'drop') %>%
  ungroup() %>%
  arrange(country, year, month_num)

# data_clima_y <- data_clima_y %>%
#   group_by(year, country) %>%
#   summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = 'drop') %>%
#   ungroup() %>%
#   arrange(country, year)

#-------------------------------------------------------------------------------
# [1.9] GDD
#-------------------------------------------------------------------------------

#data_tas_gdd <- readRDS(filepath_tas_gdd)

# We sum both Congos and round
#data_tas_gdd <- data_tas_gdd %>%
#  group_by(year, month_num, country) %>%
#  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = 'drop') %>%
#  ungroup() %>%
#  group_by(country, year) %>%
#  mutate(
#    gdd_yc = sum(gdd_ymc, na.rm = TRUE),
#    hdd_yc = sum(hdd_ymc, na.rm = TRUE),
#    fdd_yc = sum(fdd_ymc, na.rm = TRUE)
#         ) %>%
#  ungroup()
  # mutate(
  #   gdd_ymc = round(gdd_ymc, 0),
  #   hdd_ymc = round(hdd_ymc, 0),
  #   fdd_ymc = round(fdd_ymc, 0)
  # )

gdd_files <- list.files("/Users/sebastianchacon/Desktop/ObsData/ProcessedData/GDD/",
                        pattern = "\\.rds$", full.names = TRUE)

gdd_monthly <- bind_rows(lapply(gdd_files, readRDS)) %>%
  arrange(country, date)

gdd_monthly <- gdd_monthly %>%
  mutate(country = case_when(
    country == "Central African Rep." ~ "Central African Republic",
    country == "Dominican Rep." ~ "Dominican Republic", 
    country == "Eq. Guinea" ~ "Equatorial Guinea",
    country == "Vietnam" ~ "Viet Nam",
    TRUE ~ country
  ))

#-------------------------------------------------------------------------------
# [1.10] SOI
#-------------------------------------------------------------------------------
oni_data <- read_excel(filepath_oni)

oni_clean <- oni_data %>%
  select(Year, DJF:NDJ) %>%
  pivot_longer(
    cols = -Year,
    names_to = "period",
    values_to = "oni_value"
  ) %>%
  mutate(
    central_month = case_when(
      period == "DJF" ~ "Jan",  
      period == "JFM" ~ "Feb",
      period == "FMA" ~ "Mar",
      period == "MAM" ~ "Apr",
      period == "AMJ" ~ "May",
      period == "MJJ" ~ "Jun",
      period == "JJA" ~ "Jul",
      period == "JAS" ~ "Aug",
      period == "ASO" ~ "Sep",
      period == "SON" ~ "Oct",
      period == "OND" ~ "Nov",
      period == "NDJ" ~ "Dec"
    ),
    month_num = match(central_month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    date = as.Date(paste(Year, month_num, "01", sep = "-"))
  ) %>%
  select(Year, month_num, oni_value, date) %>%
  rename(year = Year) %>%
  mutate(oni_value = as.numeric(oni_value, na.rm = TRUE),
         year = as.double(year),
         month_num = as.double(month_num))

oni_clean <- na.omit(oni_clean)

# Identify events (5+ quarters ONI ≥ |0.5|)
oni_events <- oni_clean %>%
  mutate(
    event = case_when(
      oni_value >= 0.5 ~ "El Niño",
      oni_value <= -0.5 ~ "La Niña",
      TRUE ~ "Neutral"
    )
  ) %>%
  select(year, month_num, oni_value, event)
  
#-------------------------------------------------------------------------------
# [1.11] Fertilizers
#-------------------------------------------------------------------------------

fertilizers <- read_excel(filepath_tea, 
                  sheet = "Monthly Indices", skip = 6)
fertilizers <- fertilizers %>%
  select(...1, `Fertilizers **`) %>%
  rename(date = ...1, fert_ym = `Fertilizers **`)

fertilizers <- na.omit(fertilizers)

fertilizers <- fertilizers %>%
  mutate(
    year = as.double(substr(date, 1, 4)),
    month_num = as.double(substr(date, 6, 7)),
    fert_ym = as.numeric(fert_ym, na.rm = TRUE)
  ) %>%
  select(-date)

fertilizers <- fertilizers %>%
  left_join(cpi, by = c("year", "month_num")) %>%
  mutate(fert_ym = fert_ym * (100 / cpi_2017)) %>%
  select(-cpi_2017)

fertilizers_y <- fertilizers %>%
  group_by(year) %>%
  mutate(fert_y = mean(fert_ym, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(year, fert_y)

#-------------------------------------------------------------------------------
# [1.12] Supply shocks
#-------------------------------------------------------------------------------

date <- seq(from = as.Date("1960-01-01"), 
            to = as.Date("2022-12-01"), 
            by = "month")
supplyShocks <- data.frame(date=date)
#----- Diseases
supplyShocks <- supplyShocks %>% 
  mutate(disease1 = if_else(date > as.Date("2008-01-01") & date < as.Date("2011-10-01"),1,0), # Colombia: Jan2008-Oct2011
         disease2 = if_else(date > as.Date("2012-11-01") & date < as.Date("2014-09-01"),1,0)) # Central America: Nov2012-Sep2014
#----- Frosts
supplyShocks <- supplyShocks %>% 
  mutate(frost1 = if_else(date > as.Date("1994-06-01") & date <= as.Date("1996-06-01"),1,0), # Frost 1: Nights of June 25-26, 1994
         frost2 = if_else(date > as.Date("1994-07-01") & date <= as.Date("1996-07-01"),1,0)) # Frost 2: Nights of July 9-10, 1994

# Two dummies to capture the impact of droughts in Brazilian coffee-and-sugar-growing-areas
#----- First dummy: to roughly report USDA information
supplyShocks <- supplyShocks %>% 
  mutate(droughts = if_else(date >= as.Date("1999-08-01") & date <= as.Date("2001-07-01"),1,
                            if_else(date >= as.Date("2003-01-01") & date <= as.Date("2003-08-01"),1,
                                    if_else(date >= as.Date("2005-01-01") & date <= as.Date("2005-08-01"),1,
                                            if_else(date >= as.Date("2014-01-01") & date <= as.Date("2014-08-01"),1,0))))) 
#----- Second dummy: based on daily weather from UCAR-NCAR
#
#----- ICA lapse dummy
supplyShocks <- supplyShocks %>%
  mutate(ica_lapse = if_else(date >= as.Date("1989-08-01"),1,0))

supplyShocks$year = substring(supplyShocks$date,1,4)
supplyShocks <- create_month(supplyShocks)
supplyShocks$month=NULL
supplyShocks$year = as.integer(supplyShocks$year)
supplyShocks$date  = NULL
supplyShocks$trend = NULL


#-------------------------------------------------------------------------------
################################### [2] DATA ###################################
#-------------------------------------------------------------------------------

# Merge 1. Exports and imports

merge1 <- full_join(full_export, full_import, by = c("year", "month_num", "country"))
merge1 <- replace(merge1, is.na(merge1), 0)

# Merge 2. Prices and costs
merge2 <- left_join(merge1, full_prices, by = c("year", "month_num"))
merge2 <- left_join(merge2, full_farm_prices, by = c("year", "month_num", "country"))
merge2 <- left_join(merge2, full_prices_annual, by = c("year"))

# Merge 3. Other variables
merge2 <- left_join(merge2, supplyShocks, by = c("year", "month_num"))
merge2 <- left_join(merge2, tea, by = c( "year", "month_num"))
merge2 <- left_join(merge2, gdp, by = c("year"))
merge2 <- left_join(merge2, gdp_yc, by = c("year", "country"))
merge2 <- left_join(merge2, tea_annual, by = c("year"))
merge2 <- left_join(merge2, fertilizers_y, by = c("year"))
merge2 <- left_join(merge2, data_tas_ymc, by = c("year", "month_num", "country"))
#merge2 <- left_join(merge2, data_tas_gdd, by = c("year", "month_num", "country"))
merge2 <- left_join(merge2, gdd_monthly, by = c("year", "month_num", "country"))
# merge2 <- left_join(merge2, data_clima_y, by = c("year", "country"))
# merge2 <- left_join(merge2, data_clima_ym, by = c("year", "month_num","country"))
merge2 <- left_join(merge2, oni_events, by = c("year", "month_num"))
merge2 <- left_join(merge2, fertilizers, by = c("year", "month_num"))
merge2 <- merge2 %>%
  mutate(date = as.Date(paste(year, month_num, "01", sep = "-")))
merge2 <- merge2 %>%
  group_by(year, month_num) %>%
  mutate(farm_prices_ym = sum(farm_prices_ymc * share_ymce, na.rm = TRUE),
         tas_ym = sum(tas_ymc * share_ymce, na.rm = TRUE),
         gdd_ym = sum(gdd_ymc * share_ymce, na.rm = TRUE),
         hdd_ym = sum(hdd_ymc * share_ymce, na.rm = TRUE),
         fdd_ym = sum(fdd_ymc * share_ymce, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    farm_prices_y = sum(farm_prices_yc * share_ymce, na.rm = TRUE),
    farm_prices_y_mean = mean(farm_prices_yc, na.rm = TRUE),
    tas_y = sum(tas_ym * share_ymce, na.rm = TRUE),
    tas_y_mean = mean(tas_ymc, na.rm = TRUE),
    gdd_y = sum(gdd_ym),
    gdd_y_mean = mean(gdd_ymc, na.rm = TRUE),
    hdd_y = sum(hdd_ym),
    hdd_y_mean = mean(hdd_ymc, na.rm = TRUE),
    fdd_y = sum(fdd_ym),
    fdd_y_mean = mean(fdd_ymc, na.rm = TRUE)) %>%
  group_by(year, country) %>%
  mutate(
    tas_yc = mean(tas_ymc),
    gdd_yc = sum(gdd_ymc),
    hdd_yc = sum(hdd_ymc),
    fdd_yc = sum(fdd_ymc)) %>%
  ungroup()

# IDs per country

merge2 <- merge2 %>%
  mutate(id = as.numeric(factor(country)) - 1)

# Identify countries with full data

merge2 <- merge2 %>%
  arrange(year, month_num, country) %>%
  mutate(trend = as.numeric(factor(paste(year, month_num))))

ntrend <- unique(merge2$trend)  # unique year-month

ncountry <- merge2 %>%
  group_by(country, id) %>%
  summarise(
    nobs = n(),
    .groups = 'drop'
  ) %>%
  filter(nobs == length(ntrend))  # full data country

# Creates categorical for fixed effects
## Dummy for year-country
merge2 <- merge2 %>%
  mutate(dummy_ym = if_else(id == ncountry$id[1], 1L, 0L))

# Dummy for year
merge2 <- merge2 %>%
  group_by(year) %>%
  mutate(
    dummy_y = if_else(month_num == 1 & dummy_ym == 1, 1L, 0L)
  ) %>%
  ungroup()

# Dummy for year-country
merge2 <- merge2 %>%
  group_by(country, year) %>%
  mutate(
    dummy_yc = if_else(month_num == min(month_num), 1L, 0L)
  ) %>%
  ungroup()

# Dummy for leaders
merge2 <- merge2 %>%
  mutate(
    dummy_leader = if_else(
      #country == "Indonesia" |
      country == "Brazil" | 
      country == "Colombia" | 
      country == "Viet Nam", 1, 0
    ),
    dummy_follower = if_else(dummy_leader == 1, 0, 1)
  ) %>%
  group_by(year, month_num) %>%
  mutate(
    n_countries_ym = n_distinct(country),
    n_leader_ym = sum(dummy_leader == 1),
    n_follower_ym = sum(dummy_follower == 1)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    n_countries_y = n_distinct(country),
    n_leader_y = sum(dummy_leader == 1),
    n_follower_y = sum(dummy_follower == 1)
  ) %>%
  ungroup()

# GDDs for leaders

aux1 <- merge2 %>%
  filter(country == "Brazil") %>%
  mutate(gdd_brazil_ym = gdd_ymc,
         gdd_brazil_y = gdd_yc) %>%
  distinct(year, month_num, gdd_brazil_ym, gdd_brazil_y)

aux2 <- merge2 %>%
  filter(country == "Colombia") %>%
  mutate(gdd_colombia_ym = gdd_ymc,
         gdd_colombia_y = gdd_yc) %>%
  distinct(year, month_num, gdd_colombia_ym, gdd_colombia_y)

aux3 <- merge2 %>%
  filter(country == "Indonesia") %>%
  mutate(gdd_indonesia_ym = gdd_ymc,
         gdd_indonesia_y = gdd_yc) %>%
  distinct(year, month_num, gdd_indonesia_ym, gdd_indonesia_y)

aux4 <- merge2 %>%
  filter(country == "Viet Nam") %>%
  mutate(gdd_vietnam_ym = gdd_ymc,
         gdd_vietnam_y = gdd_yc) %>%
  distinct(year, month_num, gdd_vietnam_ym, gdd_vietnam_y)

merge2 <- merge2 %>%
  left_join(aux1, by = c("year", "month_num")) %>%
  left_join(aux2, by = c("year", "month_num")) %>%
  left_join(aux3, by = c("year", "month_num")) %>%
  left_join(aux4, by = c("year", "month_num"))

merge2 <- merge2 %>%
  mutate(
    is_leader = country %in% c("Brazil", "Colombia", "Indonesia", "Viet Nam"),
    gdd_leaders_ym = ifelse(is_leader, gdd_ymc, NA),
    gdd_leaders_y = ifelse(is_leader, gdd_yc, NA),
    gdd_followers_ym = ifelse(!is_leader, gdd_ymc, NA),
    gdd_followers_y = ifelse(!is_leader, gdd_yc, NA)
  ) %>%
  group_by(year, month_num) %>%
  mutate(
    gdd_leaders_ym = mean(gdd_leaders_ym, na.rm = TRUE),
    gdd_followers_ym = mean(gdd_followers_ym, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    gdd_leaders_y = mean(gdd_leaders_y, na.rm = TRUE),
    gdd_followers_y = mean(gdd_followers_y, na.rm = TRUE)) %>%
  ungroup() 

# HDDs for leaders

aux1 <- merge2 %>%
  filter(country == "Brazil") %>%
  mutate(hdd_brazil_ym = hdd_ymc,
         hdd_brazil_y = hdd_yc) %>%
  distinct(year, month_num, hdd_brazil_ym, hdd_brazil_y)

aux2 <- merge2 %>%
  filter(country == "Colombia") %>%
  mutate(hdd_colombia_ym = hdd_ymc,
         hdd_colombia_y = hdd_yc) %>%
  distinct(year, month_num, hdd_colombia_ym, hdd_colombia_y)

aux3 <- merge2 %>%
  filter(country == "Indonesia") %>%
  mutate(hdd_indonesia_ym = hdd_ymc,
         hdd_indonesia_y = hdd_yc) %>%
  distinct(year, month_num, hdd_indonesia_ym, hdd_indonesia_y)

aux4 <- merge2 %>%
  filter(country == "Viet Nam") %>%
  mutate(hdd_vietnam_ym = hdd_ymc,
         hdd_vietnam_y = hdd_yc) %>%
  distinct(year, month_num, hdd_vietnam_ym, hdd_vietnam_y)

merge2 <- merge2 %>%
  left_join(aux1, by = c("year", "month_num")) %>%
  left_join(aux2, by = c("year", "month_num")) %>%
  left_join(aux3, by = c("year", "month_num")) %>%
  left_join(aux4, by = c("year", "month_num"))

merge2 <- merge2 %>%
  mutate(
    is_leader = country %in% c("Brazil", "Colombia", "Indonesia", "Viet Nam"),
    hdd_leaders_ym = ifelse(is_leader, hdd_ymc, NA),
    hdd_leaders_y = ifelse(is_leader, hdd_yc, NA),
    hdd_followers_ym = ifelse(!is_leader, hdd_ymc, NA),
    hdd_followers_y = ifelse(!is_leader, hdd_yc, NA)
  ) %>%
  group_by(year, month_num) %>%
  mutate(
    hdd_leaders_ym = mean(hdd_leaders_ym, na.rm = TRUE),
    hdd_followers_ym = mean(hdd_followers_ym, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    hdd_leaders_y = mean(hdd_leaders_y, na.rm = TRUE),
    hdd_followers_y = mean(hdd_followers_y, na.rm = TRUE)) %>%
  ungroup() 
  
# FDDs for leaders

aux1 <- merge2 %>%
  filter(country == "Brazil") %>%
  mutate(fdd_brazil_ym = fdd_ymc,
         fdd_brazil_y = fdd_yc) %>%
  distinct(year, month_num, fdd_brazil_ym, fdd_brazil_y)

aux2 <- merge2 %>%
  filter(country == "Colombia") %>%
  mutate(fdd_colombia_ym = fdd_ymc,
         fdd_colombia_y = fdd_yc) %>%
  distinct(year, month_num, fdd_colombia_ym, fdd_colombia_y)

aux3 <- merge2 %>%
  filter(country == "Indonesia") %>%
  mutate(fdd_indonesia_ym = fdd_ymc,
         fdd_indonesia_y = fdd_yc) %>%
  distinct(year, month_num, fdd_indonesia_ym, fdd_indonesia_y)

aux4 <- merge2 %>%
  filter(country == "Viet Nam") %>%
  mutate(fdd_vietnam_ym = fdd_ymc,
         fdd_vietnam_y = fdd_yc) %>%
  distinct(year, month_num, fdd_vietnam_ym, fdd_vietnam_y)

merge2 <- merge2 %>%
  left_join(aux1, by = c("year", "month_num")) %>%
  left_join(aux2, by = c("year", "month_num")) %>%
  left_join(aux3, by = c("year", "month_num")) %>%
  left_join(aux4, by = c("year", "month_num"))

merge2 <- merge2 %>%
  mutate(
    is_leader = country %in% c("Brazil", "Colombia", "Indonesia", "Viet Nam"),
    fdd_leaders_ym = ifelse(is_leader, fdd_ymc, NA),
    fdd_leaders_y = ifelse(is_leader, fdd_yc, NA),
    fdd_followers_ym = ifelse(!is_leader, fdd_ymc, NA),
    fdd_followers_y = ifelse(!is_leader, fdd_yc, NA)
  ) %>%
  group_by(year, month_num) %>%
  mutate(
    fdd_leaders_ym = mean(fdd_leaders_ym, na.rm = TRUE),
    fdd_followers_ym = mean(fdd_followers_ym, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    fdd_leaders_y = mean(fdd_leaders_y, na.rm = TRUE),
    fdd_followers_y = mean(fdd_followers_y, na.rm = TRUE)) %>%
  ungroup() 

saveRDS(merge2, file = "/Users/sebastianchacon/Desktop/sc_tesis/bld/data/data.rds")

