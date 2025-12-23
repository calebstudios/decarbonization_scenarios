#/******************************************************************************
# PROGRAM NAME: carbon_pathways_model_shell.R
# PROJECT: Decarbonization Scenarios
# DESCRIPTION: Shell for loading inputs + assembling model-ready table
# DATE: 2025-12-22
# R-VERSION: R version 4.5.2 (2025-11-01)
#******************************************************************************/

#### Setup ####
library(tidyverse)
library(readxl)

source("io_excel.R")

#### Config ####
years <- 2025:2030
input_workbook <- "Carbon_Pathways_Inputs_Sample.xlsx"

#### Load Data ####
input_params <- read_xlsx(input_workbook, sheet = "Input_Params") %>% 
  mutate(
    SheetName = as.character(SheetName), 
    YrStartCol = as.integer(YrStartCol), 
    ValueName = as.character(ValueName)
  )

# Split sheets: year-indexed vs non-year-indexed
non_year_sheets <- c("Demand_Tech_Characteristics", "Stock_Turnover")

year_sheet_params <- input_params %>% 
  filter(!SheetName %in% non_year_sheets)

# Load year-indexed sheets into long format
demand_input_tables <- year_sheet_params %>% 
  mutate(
    data = pmap(
      list(SheetName, YrStartCol, ValueName), 
      ~fn_read_year(
        filename_in = input_workbook, 
        sheet_in = ..1, 
        yr_start_col = ..2, 
        value_col_name = ..3
      )
    )
  ) %>% 
  select(SheetName, data) %>% 
  deframe()

# Normalize list names
names(demand_input_tables) <- names(demand_input_tables) %>% 
  str_replace_all("\\s+", "_")

# Load non-year-indexed sheets "raw"
demand_input_tables[["Demand_Tech_Characteristics"]] <- read_xlsx(
  input_workbook, sheet = "Demand_Tech_Characteristics")

demand_input_tables[["Stock_Turnover"]] <- read_xlsx(
  input_workbook, sheet = "Stock_Turnover"
)

#### Validate ####
required_sheets <- c(
  "Demand_Tech_Characteristics", 
  "Baseline_Demand_Fuel_Mix", 
  "Demand_Tech_Unit_Cost", 
  "Demand_Tech_Efficiency", 
  "Baseline_Efficiency", 
  "Stock", 
  "Demand_Scaling", 
  "Stock_Turnover"
)

missing_sheets <- setdiff(required_sheets, names(demand_input_tables))
if (length(missing_sheets) > 0){
  stop(glue::glue(paste0("Missing required input sheets: ", 
                         "{paste(missing_sheets, collapse = ', ')}")))
}

# Quick sanity summary
input_summary <- tibble(
  sheet = names(demand_input_tables), 
  n_rows = map_int(demand_input_tables, nrow), 
  n_cols = map_int(demand_input_tables, ncol)
) %>% 
  arrange(sheet)

print(input_summary)

# Check year coverage where applicable
year_check <- demand_input_tables %>% 
  keep(~"year" %in% names(.x)) %>% 
  map(~tibble(
    min_year = min(.x$year, na.rm = TRUE), 
    max_year = max(.x$year, na.rm = TRUE)
  )) %>% 
  bind_rows(.id = "sheet")

print(year_check)

# Ensure required years exist in key year-indexed sheets
key_year_sheets <- c(
  "Baseline_Demand_Fuel_Mix", 
  "Demand_Tech_Unit_Cost", 
  "Demand_Tech_Efficiency", 
  "Baseline_Efficiency", 
  "Stock", 
  "Demand_Scaling")

missing_years <- map(
  set_names(key_year_sheets), 
  ~setdiff(years, unique(demand_input_tables[[.x]]$year))
)

missing_years <- keep(missing_years, ~length(.x) > 0)
if (length(missing_years) > 0) {
  bad <- paste0(names(missing_years), ": [", 
                map_chr(missing_years, ~paste(.x, collapse = ", ")), "]")
  stop(glue::glue("Missing years detected in -> {paste(bad, collapse = '; ')}"))
}

#### Reference Forecast ####
customer_stock <- demand_input_tables$Stock %>% 
  filter(year %in% years) %>% 
  rename(climate_zone = `Climate Zone`, 
         stock_value = stock_count_or_floor_area)

baseline_efficiency <- demand_input_tables$Baseline_Efficiency %>% 
  filter(year %in% years) %>% 
  rename(service_demand = `Service Demand`, 
         fuel_type = `Fuel Type`,
         base_efficiency = baseline_efficiency_value)

demand_scaling <- demand_input_tables$Demand_Scaling %>% 
  filter(year %in% years) %>% 
  rename(climate_zone = `Climate Zone`, 
         service_demand = `Service Demand`, 
         stock_name = Stock, 
         scale = service_demand_scaler)

baseline_fuel_mix <- demand_input_tables$Baseline_Demand_Fuel_Mix %>% 
  filter(year %in% years) %>% 
  rename(service_demand = `Service Demand`, 
         fuel_type = `Fuel Type`, 
         share = share_of_service_demand)

# Basic duplicate guards (helps prevent cartesian joins)
stopifnot(!any(duplicated(customer_stock %>% 
                            select(Stock, climate_zone, year))))
stopifnot(!any(duplicated(demand_scaling %>% 
                            select(Demand, service_demand, climate_zone, 
                                   stock_name, year))))
stopifnot(!any(duplicated(baseline_efficiency %>% 
                            select(service_demand, fuel_type, year))))
stopifnot(!any(duplicated(baseline_fuel_mix %>% 
                            select(service_demand, fuel_type, year))))

# Service demand output (e.g., MMBtu required at the service level)
mmbtu_output <- customer_stock %>% 
  filter(Stock == "Residential Households") %>% 
  rename(stock_count = stock_value) %>% 
  inner_join(
    demand_scaling %>% 
      filter(stock_name == "Residential Households"), 
    by = c("climate_zone", "year"), 
    relationship = "one-to-many"
    ) %>% 
  mutate(total_service_need = stock_count * scale)

# Convert service need to fuel input using baseline efficiency + fuel mix
mmbtu_input <- mmbtu_output %>% 
  # Lighting is kWh; handle separately
  filter(Demand != "Residential Lighting") %>% 
  inner_join(
    baseline_efficiency, 
    by = c("service_demand", "year"), 
    relationship = "many-to-many"
    ) %>% 
  mutate(fuel_input_need = total_service_need / base_efficiency) %>% 
  inner_join(
    baseline_fuel_mix, 
    by = c("service_demand", "year", "fuel_type"), 
    relationship = "many-to-one"
    ) %>% 
  mutate(total_fuel_consumption_mmbtu = fuel_input_need * share) %>% 
  group_by(climate_zone, year, service_demand, fuel_type) %>% 
  summarise(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, 
                                               na.rm = TRUE), 
            .groups = "drop")

glimpse(mmbtu_input)

#### Next steps ####
# 1) Build drivers from Stock + Demand_Scaling (including lighting kWh path)
# 2) Build measures from Demand_Tech_Characteristics 
#    (+ supply later)
# 3) Build adoption trajectories from saturation + uptake function
# 4) Compute impacts (energy, cost, emissions)