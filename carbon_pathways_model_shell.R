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
        value_col_name = ..3,
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
  keep(~"Year" %in% names(.x) | "year" %in% names(.x)) %>% 
  map(~{
    ycol <- if("year" %in% names(.x)) "year" else "Year"
    tibble(min_year = min(.x[[ycol]], na.rm = TRUE), 
           max_year = max(.x[[ycol]], na.rm = TRUE))
  }) %>% 
  bind_rows(.id = "sheet")

print(year_check)

# Ensure required years exist in key year-indexed sheets
key_year_sheets <- c("Baseline_Demand_Fuel_Mix", "Demand_Tech_Unit_Cost", 
                     "Demand_Tech_Efficiency", "Baseline_Efficiency", "Stock", 
                     "Demand_Scaling")
missing_years <- map(key_year_sheets, ~setdiff(years, unique(demand_input_tables[[.x]]$year))) %>% 
  set_names(key_year_sheets)

missing_years <- keep(missing_years, ~length(.x) > 0)
if (length(missing_years) > 0) {
  stop(glue::glue(paste0("Missing years detected in: {paste(names(missing_years, collapse = ', ')}")))
}

#### TODO: model steps ####
# 1) Build drivers from demand_input_tables$Stock
# 2) Build measures from demand_input_tables$Demand_Tech_Characteristics 
#    (+ supply tables later)
# 3) Build adoption trajectories from saturation + uptake function
# 4) Compute impacts (energy, cost, emissions)