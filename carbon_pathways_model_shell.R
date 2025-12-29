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

# --- Decision variable(s) ---
# For now: single variable = annual saturation increase for one tech
# Starting guess
x0 <- c(0.25)
names(x0) <- "hpwh_annual_increase"

# Bounds: keep non-negative unless modeling backsliding
lb <- c(0.05)
ub <- c(0.80)

discount_rate <- 0.05
disc_tbl <- tibble(Year = years) %>% 
  mutate(disc = 1 / (1 + discount_rate)^(Year - min(Year)))

# ---- Helpers ----
clamp01 <- function(x) pmin(pmax(x, 0), 1)

# Map technologies to fuel types (make this a real input sheet later)
tech_fuel_map <- tibble::tribble(
  ~DemandTechnology, ~fuel_type, 
  "Heat Pump Water Heater (50 gal)", "Electricity"
)

# Build annual eligible adopters = replacements + positive net new
f.calc_eligible_customers <- function(customer_stock, stock_turnover, years) {
  
  stock_turnover <- stock_turnover %>% 
    # TODO: fix in workbook
    mutate(Stock = "Residential Households")
  
  hh <- customer_stock %>% 
    filter(Stock == "Residentital Households", year %in% years) %>% 
    rename(stock_count = stock_value) %>% 
    arrange(climate_zone, year) %>% 
    group_by(climate_zone) %>% 
    mutate(
      prev_stock = lag(stock_count), 
      net_new = stock_count - prev_stock, 
      net_new = dplyr::if_else(is.na(net_new), 0, net_new), 
      net_new_pos = pmax(net_new, 0)
    ) %>% 
    ungroup()
  
  elig <- hh %>% 
    left_join(stock_turnover %>% 
                select(Stock, Lifetime), by = "Stock") %>% 
    mutate(
      replacements = dplyr::if_else(is.na(prev_stock), 0, 
                                    prev_stock / Lifetime), 
      eligible_customers = replacements + net_new_pos
    ) %>% 
    select(climate_zone, year, stock_count, eligible_customers)
  
  elig
}

# Tech stock accounting: 
# tech_stock_t = 
#   min(stock, tech_stock_{t-1} + min(eligible * rate, stock-tech_stock_{t-1}))
f.calc_annual_adoption <- function(x, 
                                   years, 
                                   customer_stock, 
                                   stock_turnover, 
                                   starting_tech_market_share) {
  
  # For now: single decision variable = 
  #   steady-state adoption rate of eligible customers per year
  # (Interpretation: among eligible customers each year, 
  #   fraction that chooses the tech)
  adoption_rate <- clamp01(x[1])
  
  elig <- f.calc_eligible_customers(customer_stock, stock_turnover, years)
  
  # Starting tech stock in first year (per climate_zone) 
  # based on starting saturation % of stock
  # NOTE: starting_tech_market_share must include 
  # StartingSaturation_perc and Demand Technology + Service Demand
  start <- starting_tech_market_share %>% 
    filter(`Demand Technology` == "Heat Pump Water Heater (50 gal)") %>% 
    select(Sector, `Service Demand`, `Demand Technology`, 
           StartingSaturation_perc) %>% 
    distinct()
  
  # Assume start saturation applies to all climate zones initially 
  # (until provided CZ-specific saturation)
  start_by_cz <- tidyr::crossing(
    elig %>% 
      filter(year == min(years)) %>% 
      select(climate_zone, year, stock_count), 
    start
  ) %>% 
    mutate(
      tech_stock = stock_count * StartingSaturation_perc
    ) %>% 
    select(climate_zone, year, Sector, ServiceDemand = `Service Demand`, 
           DemandTechnology = `Demand Technology`, 
           stock_count, tech_stock)
  
  # Roll forward tech stock year by year
  out <- elig %>% 
    arrange(climate_zone, year) %>% 
    group_by(climate_zone) %>% 
    group_modify(~{
      cz <- .x
      
      # Initialize
      init_row <- start_by_cz %>% 
        filter(climate_zone == cz$climate_zone[1])
      tech_stock_prev <- init_row$tech_stock[1]
      sector <- init_row$Sector[1]
      service <- init_row$ServiceDemand[1]
      tech <- init_row$DemandTechnology[1]
      
      res <- cz %>% 
        mutate(
          Sector = sector, 
          ServiceDemand = service, 
          DemandTechnology = tech, 
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
    mutate(
      YearSaturation = clamp01(tech_stock / stock_count)
    )
  
  out
}

# Objective wrapper
eval_f <- function(x) {
  
  # Adoption path (returns tech_stock + YearSaturation + new_adopters)
  adoption <- f.calc_annual_adoption(
    x = x, 
    years = years, 
    customer_stock = customer_stock, 
    stock_turnover = demand_input_tables$Stock_Turnover, 
    starting_tech_market_share = starting_tech_market_share
  )
  
  # Join service need (MMBtu at service level)
  service_need <- mmbtu_output %>% 
    select(climate_zone, year, service_demand, total_service_need)
  
  # Fuel mapping for the tech (explicit fuel switching)
  adoption2 <- adoption %>% 
    left_join(tech_fuel_map, by = "DemandTechnology")

  # Tech service demand should match workbook naming
  # Here: assume HPWH affects "Water Heating"
  new_mmbtu <- adoption2 %>% 
    filter(ServiceDemand == "Water Heating") %>%  
    left_join(
      service_need %>% 
        filter(service_demand == "Water Heating"), 
      by = c("climate_zone", "year"), 
      relationship = "many-to-one"
      ) %>% 
    mutate(MMBTUNeed_Tech = YearSaturation * total_service_need) %>% 
    left_join(
      demand_tech_efficiency, 
      by = c("DemandTechnology" = "Demand Technology", "year" = "year"), 
      relationship = "many-to-one"
      ) %>% 
    mutate(
      total_fuel_consumption_mmbtu = MMBTUNeed_Tech / efficiency_metric_value
      ) %>% 
    select(climate_zone, year, ServiceDemand, 
           fuel_type, total_fuel_consumption_mmbtu)
  
  # Emissions (still placeholder factors)
  new_emissions <- new_mmbtu %>% 
    left_join(emissions_table, by = "fuel_type") %>% 
    mutate(emissions = emissions_factor * total_fuel_consumption_mmbtu) %>% 
    group_by(year) %>% 
    summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
  
  # Cost: incremental adopters * unit cost 
  # (clamp negative to 0; already non-negative by construction)
  incr_cost <- adoption2 %>% 
    left_join(
      demand_tech_unit_cost, 
      by = c("DemandTechnology" = "Demand Type", "year" = "year"), 
      relationship = "many-to-one"
      ) %>% 
    mutate(cost = new_adopters * unit_cost_real_2024USD) %>% 
    group_by(year) %>% 
    summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")
  
  # NPV
  npv_cost <- incr_cost %>% 
    left_join(disc_tbl, by = c("year" = "Year")) %>% 
    mutate(npv_cost = total_cost * disc) %>% 
    summarize(NPV = sum(npv_cost, na.rm = TRUE)) %>% 
    pull(NPV)
  
  # Current objective: minimize cost only
  npv_cost
  }

# Run optimization
library(nloptr)

opt_output <- nloptr::nloptr(
  x0 = x0, 
  eval_f = eval_f, 
  lb = lb, 
  ub = ub, 
  opts = list(
    algorithm = "NLOPT_GN_DIRECT", 
    xtol_rel = 1.0e-8, 
    maxeval = 200
  )
)

# Need to meet the emissions target - simplified analysis for creating a supply 
# and demand
# ---> Assuming there's a demand specific target for emissions reduction

opt_output$solution 
opt_output$objective