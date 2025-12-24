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

# TODO remove baseline efficiency - it needs to be calculated using the demand
# tech efficiencies and starting saturation percents
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

fuel_mix_check <- baseline_fuel_mix %>% 
  group_by(service_demand, year) %>% 
  summarise(share_sum = sum(share, na.rm = TRUE), .groups = "drop")

if (any(abs(fuel_mix_check$share_sum - 1) > 1e-6)) {
  bad <- fuel_mix_check %>% 
    filter(abs(share_sum - 1) > 1e-6)
  stop(paste0("Fuel mix shares do not sum to 1 ", 
              "for some service_demand-year combinations."))
}

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

# Convert service need to fuel input using fuel mix + baseline efficiency
# Flow: service_need -> allocate by fuel mix -> convert to fuel input via 
# efficiency
mmbtu_input <- mmbtu_output %>% 
  filter(Demand != "Residential Lighting") %>% 
  inner_join(
    baseline_fuel_mix, 
    by = c("service_demand", "year"), 
    relationship = "many-to-many"
    ) %>% 
  mutate(service_need_by_fuel = total_service_need * share) %>% 
  inner_join(
    baseline_efficiency, 
    by = c("service_demand", "fuel_type", "year"), 
    relationship = "many-to-one"
    ) %>% 
  mutate(
    total_fuel_consumption_mmbtu = service_need_by_fuel / base_efficiency
    ) %>% 
  group_by(climate_zone, year, service_demand, fuel_type) %>% 
  summarise(total_fuel_consumption_mmbtu = sum(total_fuel_consumption_mmbtu, 
                                               na.rm = TRUE), 
            .groups = "drop")

glimpse(mmbtu_input)

emissions_table <- data.frame(
  fuel_type = c("Electricity", "Natural Gas"), 
  # TODO replace with sourced table + units
  emissions_factor = c(3.5, 5))

ref_emissions <- mmbtu_input %>% 
  left_join(emissions_table, by = "fuel_type") %>% 
  mutate(emissions = emissions_factor * total_fuel_consumption_mmbtu) %>% 
  group_by(year) %>% 
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")

#### Logic Flow for Calculating the new MMBTU consumption ####
# this will be wrapped in an optimization instead of using the
# manually defined tech_annual_saturation_increase table

demand_tech_char <- demand_input_tables$Demand_Tech_Characteristics

demand_tech_unit_cost <- demand_input_tables$Demand_Tech_Unit_Cost

demand_tech_efficiency <- demand_input_tables$Demand_Tech_Efficiency
# TODO unit cost normalized to possible reduced emissions

# multiple starting saturation * fuel mix = starting technology market share
starting_tech_market_share <- demand_tech_char %>% 
  left_join(baseline_fuel_mix, by = c("Service Demand" = "service_demand", 
                                      "Fuel Type" = "fuel_type")) %>% 
  filter(year == 2025) %>% 
  mutate(StartingSaturation_perc = share * `Starting Saturation_percent`) %>% 
  select(-year)

# TODO - make sure things are being indexed correctly and make sure the number 
# of indexes are sufficient. This annual saturation right now is a function of 
# stock turnover + new customers.
# TODO - building a function that calculates a percent increase based on things 
# like: stock turnover + new customers (base case), technology available, 
# reasonable adoption.
tech_annual_saturation_increase <- data.frame(DemandTechnology = c(
  "Heat Pump Water Heater (50 gal)"), 
  AnnualSatIncrease_perc = c(0.01)) %>% 
  merge(data.frame(Year =seq(2025, 2030, 1))) %>% 
  mutate(YearSatIncrease = AnnualSatIncrease_perc * (Year - 2025))

# Change that starting tech year by year based on an assumed percentage increase
# TODO limited by the max and min saturation increase & calculated through the 
# uptake function that'll be the fuel mix and the demand tech forecast
tech_demand_sat_forecast <- tech_annual_saturation_increase %>% 
  left_join(starting_tech_market_share,
            by = c("DemandTechnology" = "Demand Technology")) %>% 
  mutate(YearSaturation = YearSatIncrease + StartingSaturation_perc) %>% 
  select(DemandTechnology, Year, Sector, 
         `Service Demand`, `Fuel Type`, YearSaturation)

# Calculate the new efficiency vector based on demand tech forecast
new_mmbtu <- tech_demand_sat_forecast %>% 
  mutate(Year = as.integer(Year)) %>% 
  left_join(
    mmbtu_output %>% 
      select(climate_zone, year, service_demand, total_service_need), 
    by = c("Year" = "year", "Service Demand" = "service_demand"), 
    relationship = "many-to-many"
    ) %>% 
  mutate(MMBTUNeed_Tech = YearSaturation * total_service_need) %>% 
  left_join(
    demand_tech_efficiency, 
    by = c("DemandTechnology" = "Demand Technology", "Year" = "year"), 
    relationship = "many-to-one"
    ) %>% 
  mutate(TotalMMBTU_input = MMBTUNeed_Tech / efficiency_metric_value)

new_mmbtu_input <- new_mmbtu %>% 
  group_by(climate_zone, Year, `Service Demand`, `Fuel Type`) %>% 
  summarise(Total_MMBTU_Consumption = sum(TotalMMBTU_input, na.rm = TRUE), 
            .groups = "drop")

# TODO multiply by GHG emissions to get new GHG emissions

#### Next steps ####
# Get reference and first step costs
# New objective function is based on those costs
# Then code in4 an optimization
# 1) Build drivers from Stock + Demand_Scaling (including lighting kWh path)
# 2) Build measures from Demand_Tech_Characteristics 
#    (+ supply later)
# 3) Build adoption trajectories from saturation + uptake function
# 4) Compute impacts (energy, cost, emissions)