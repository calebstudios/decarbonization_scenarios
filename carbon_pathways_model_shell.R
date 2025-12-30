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
source("tbl_ops.R")

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

# TODO - implement f_arr_op
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

# ---- Helpers ----
clamp01 <- function(x) pmin(pmax(x, 0), 1)

discount_rate <- 0.05
disc_tbl <- tibble(Year = years) %>% 
  mutate(disc = 1 / (1 + discount_rate)^(Year - min(Year)))

# Map technologies to fuel types (make this a real input sheet later)
tech_fuel_map <- tibble::tribble(
  ~DemandTechnology, ~fuel_type, 
  "Heat Pump Water Heater (50 gal)", "Electricity"
)

# ---- Eligible customers ----
f_calc_eligible_customers <- function(customer_stock, stock_turnover, years) {
  
  # Expect stock_turnover already has Stock + Lifetime rows
  hh <- customer_stock %>% 
    filter(Stock == "Residential Households", year %in% years) %>% 
    rename(stock_count = stock_value) %>% 
    arrange(climate_zone, year) %>% 
    group_by(climate_zone) %>% 
    mutate(
      prev_stock = lag(stock_count), 
      net_new = dplyr::if_else(is.na(prev_stock), 0, stock_count - prev_stock), 
      net_new_pos = pmax(net_new, 0)
    ) %>% 
    ungroup()
  
  hh %>% 
    left_join(
      stock_turnover %>% 
        select(Lifetime), 
      by = character()
    ) %>% 
    mutate(
      replacements = dplyr::if_else(is.na(prev_stock), 0, 
                                    prev_stock / Lifetime), 
      eligible_customers = replacements + net_new_pos
    ) %>% 
    select(climate_zone, year, stock_count, eligible_customers)
}

# ---- Adoption accounting (tech stock roll-forward) ----
f_calc_adoption <- function(x, years, elig, 
                            starting_tech_market_share, 
                            tech_name) {
  
  adoption_rate <- clamp01(x[1])
  
  # Starting saturation (assumed uniform across climate zones for now)
  start_sat <- starting_tech_market_share %>% 
    filter(`Demand Technology` == tech_name) %>% 
    summarise(start_sat = first(StartingSaturation_perc), .groups = "drop") %>% 
    pull(start_sat)
  
  # Roll forward per climate zone
  out <- elig %>% 
    arrange(climate_zone, year) %>% 
    group_by(climate_zone) %>% 
    group_modify(~{
      cz <- .x
      
      tech_stock_prev <- cz$stock_count[cz$year == min(years)][1] * start_sat
      
      res <- cz %>% 
        mutate(
          DemandTechnology = tech_name, 
          tech_stock = NA_real_, 
          new_adopters = NA_real_
        )
      
      for (i in seq_len(nrow(res))) {
        stock_i <- res$stock_count[i]
        elig_i <- res$eligible_customers[i]
        
        remaining <- pmax(stock_i - tech_stock_prev, 0)
        adopt_i <- pmin(elig_i * adoption_rate, remaining)
        tech_stock_i <- pmin(stock_i, tech_stock_prev + adopt_i)
        
        res$tech_stock[i] <- tech_stock_i
        res$new_adopters[i] <- adopt_i
        
        tech_stock_prev <- tech_stock_i
      }
      
      res
    }) %>% 
    ungroup() %>% 
    mutate(YearSaturation = clamp01(tech_stock / stock_count))
  
  out
}

# ---- Water heating emissions delta vs baseline ---
# Computes (baseline emissions displaced) and (new tech emissions added)
f_calc_water_heat_emissions_delta <- function(adoption_tbl, 
                                              service_need_tbl, 
                                              demand_tech_efficiency, 
                                              baseline_fuel_mix, 
                                              baseline_efficiency, 
                                              emissions_table) {
  
  # Focus on water heating service demand only
  srv <- "Water Heating"
  
  # Baseline water heating fuel consumption by fuel type (from reference tables)
  # service_need -> allocate by baseline fuel mix 
  # -> divide by baseline efficiency
  baseline_wh <- service_need_tbl %>% 
    filter(service_demand == srv) %>% 
    inner_join(
      baseline_fuel_mix %>% 
        filter(service_demand == srv), 
      by = c("service_demand", "year")
    ) %>% 
    mutate(service_need_by_fuel = total_service_need * share) %>% 
    inner_join(
      baseline_efficiency %>% 
        filter(service_demand == srv), 
      by = c("service_demand", "fuel_type", "year")
    ) %>% 
    mutate(baseline_fuel_mmbtu = service_need_by_fuel / base_efficiency) %>% 
    group_by(climate_zone, year, fuel_type) %>% 
    summarise(baseline_fuel_mmbtu = sum(baseline_fuel_mmbtu, na.rm = TRUE), 
              .groups = "drop") %>% 
    left_join(emissions_table, by = "fuel_type") %>% 
    group_by(climate_zone, year) %>% 
    summarise(baseline_emissions = sum(emissions_factor * baseline_fuel_mmbtu, 
                                       na.rm = TRUE), .groups = "drop")
  
  # New tech emissions for water heating: 
  # tech service need share -> divide by tech efficiency -> multiply EF
  adoption_wh <- adoption_tbl %>% 
    filter(year %in% years) %>% 
    mutate(service_demand = srv) %>% 
    select(climate_zone, year, DemandTechnology, YearSaturation)
  
  tech_fuel <- adoption_wh %>% 
    left_join(tech_fuel_map, by = "DemandTechnology") %>% 
    left_join(
      service_need_tbl %>% 
        filter(service_demand == srv), 
      by = c("climate_zone", "year")
    ) %>% 
    mutate(tech_service_need = YearSaturation * total_service_need) %>% 
    left_join(
      demand_tech_efficiency %>% 
        # Keep flexible if sheet doesn't have this col
        filter(`Demand Technology` == "Heat Pump Water Heater (50 gal)"), 
        by = c("DemandTechnology" = "Demand Technology", "year" = "year")
    ) %>% 
    mutate(tech_fuel_mmbtu = tech_service_need / efficiency_metric_value) %>% 
    left_join(emissions_table, by = "fuel_type") %>% 
    group_by(climate_zone, year) %>% 
    summarise(tech_emissions = sum(emissions_factor * tech_fuel_mmbtu, 
                                   na.rm = TRUE), .groups = "drop")
  
  # Displaced baseline emissions are proportional to adoption saturation
  delta <- baseline_wh %>% 
    left_join(adoption_wh %>% 
                select(climate_zone, year, YearSaturation), 
              by = c("climate_zone", "year")) %>% 
    mutate(displaced_emissions = baseline_emissions * YearSaturation) %>% 
    select(climate_zone, year, baseline_emissions, displaced_emissions) %>% 
    left_join(tech_fuel, by = c("climate_zone", "year")) %>% 
    mutate(delta_emissions = tech_emissions - displaced_emissions) %>% 
    group_by(year) %>% 
    summarise(
      delta_emissions = sum(delta_emissions, na.rm = TRUE), 
      tech_emissions = sum(tech_emissions, na.rm = TRUE), 
      displaced_emissions = sum(displaced_emissions, na.rm = TRUE), 
      .groups = "drop"
    )
  
  delta
}

# ---- Objective: minimize NPV of incremental tech costs ----
npv_objective_f <- function(x) {
  
  elig <- f_calc_eligible_customers(
    customer_stock = customer_stock, 
    stock_turnover = demand_input_tables$Stock_Turnover, 
    years = years
  )
  
  adoption <- f_calc_adoption(
    x = x, 
    years = years, 
    elig = elig,
    starting_tech_market_share = starting_tech_market_share, 
    tech_name = "Heat Pump Water Heater (50 gal)"
  )
  
  incr_cost <- adoption %>% 
    left_join(
      demand_tech_unit_cost, 
      by = c("DemandTechnology" = "Demand Type", "year" = "year")
      ) %>% 
    mutate(cost = new_adopters * unit_cost_real_2024USD) %>% 
    group_by(year) %>% 
    summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")
  
  npv_cost <- incr_cost %>% 
    left_join(disc_tbl, by = c("year" = "Year")) %>% 
    mutate(npv_cost = total_cost * disc) %>% 
    summarize(NPV = sum(npv_cost, na.rm = TRUE)) %>% 
    pull(NPV)
  
  npv_cost
}

# ---- Equality constraint: hit a 2030 emissions target ----
# Water heating-adjusted total
target_year <- 2030
# Interpret as total emissions in target_year
target_emissions_total <- 1480000

f_emissions_constraint <- function(x) {
  
  elig <- f_calc_eligible_customers(customer_stock, 
                                    demand_input_tables$Stock_Turnover, years)
  adoption <- f_calc_adoption(x, years, elig, 
                              starting_tech_market_share, 
                              "Heat Pump Water Heater (50 gal)")
  
  service_need_tbl <- mmbtu_output %>% 
    select(climate_zone, year, service_demand, total_service_need)
  
  delta_wh <- f_calc_water_heat_emissions_delta(
    adoption_tbl = adoption, 
    service_need_tbl = service_need_tbl, 
    demand_tech_efficiency = demand_tech_efficiency, 
    baseline_fuel_mix = baseline_fuel_mix, 
    baseline_efficiency = baseline_efficiency, 
    emissions_table = emissions_table
  )
  
  # Total emissions = reference total + delta (water heating only)
  total_adj <- ref_emissions %>% 
    left_join(delta_wh %>% 
                select(year, delta_emissions), by = "year") %>% 
    mutate(delta_emissions = replace_na(delta_emissions, 0), 
           emissions_adj = emissions + delta_emissions)
  
  e_target <- total_adj %>% 
    filter(year == target_year) %>% 
    pull(emissions_adj)
  
  e_target - target_emissions_total
}

# ---- Optimize ----
library(nloptr)

x0 <- c(hpwh_adoption_rate = 0.04)
lb <- c(0.00)
ub <- c(1.00)

opt_output <- nloptr::nloptr(
  x0 = x0, 
  eval_f = npv_objective_f, 
  eval_g_eq = f_emissions_constraint,
  lb = lb, 
  ub = ub, 
  opts = list(
    algorithm = "NLOPT_LN_COBYLA", 
    xtol_rel = 1.0e-8, 
    maxeval = 500
  )
)

# Need to meet the emissions target - simplified analysis for creating a supply 
# and demand
# ---> Assuming there's a demand specific target for emissions reduction

opt_output$solution 
opt_output$objective
