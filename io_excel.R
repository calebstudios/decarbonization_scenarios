#/******************************************************************************
# PROGRAM NAME: io_excel.R
# PROJECT: Decarbonization Scenarios
# DESCRIPTION: Excel input/output helper functions
# DATE: 2025-12-22
# R-VERSION: R Version 4.5.2
#******************************************************************************/

# Function: read a "wide by year" sheet into long format
fn_read_year <- function(filename_in,
                         sheet_in,
                         yr_start_col,
                         value_col_name = "value",
                         year_min = 1900,
                         year_max = 2100) {
  data_in <- readxl::read_xlsx(path = filename_in, sheet = sheet_in)
  
  if (ncol(data_in) < yr_start_col) {
    stop(glue::glue(
      "Sheet '{sheet_in}' has {ncol(data_in)} columns, but yr_start_col = {yr_start_col}."
    ))
  }
  
  colnames_in <- names(data_in)
  colnames_yr <- colnames_in[yr_start_col:length(colnames_in)]
  
  # Validate year-like columns
  year_int <- suppressWarnings(as.integer(colnames_yr))
  if (any(is.na(year_int))) {
    bad <- colnames_yr[is.na(year_int)]
    stop(glue::glue(
      "Sheet '{sheet_in}': year columns must be numeric (e.g., 2025). Non-numeric: {paste(bad, collapse = ', ')}"
    ))
  }
  if (any(year_int < year_min | year_int > year_max)) {
    bad <- colnames_yr[year_int < year_min | year_int > year_max]
    stop(glue::glue(
      "Sheet '{sheet_in}': year columns outside [{year_min}, {year_max}]: {paste(bad, collapse = ', ')}"
    ))
  }
  
  data_out <- data_in %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(colnames_yr),
      names_to = "year",
      values_to = value_col_name
    ) %>%
    dplyr::mutate(year = as.integer(year))
  
  data_out
}

# Example: 
# df <- fn_read_year("Carbon_Pathways_Inputs_Sample.xlsx", 
#                    "Demand_Tech_Unit_Cost", 4, "unit_cost_real_2024USD")
