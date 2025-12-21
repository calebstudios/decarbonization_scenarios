library(tidyverse)
library(readxl)

# ---- Config ----
years <- 2025:2030
filename_in <- "Carbon_Pathways_Inputs_Sample.xlsx"

# ---- Helpers ----
fn_read_year <- function(filename_in, 
                         sheet_in, 
                         yr_start_col, 
                         value_col_name = "value") {
  data_in <- read_xlsx(path = filename_in, sheet = sheet_in)
  
  if (ncol(data_in) < yr_start_col) {
    stop(glue(
      "Sheet '{sheet_in}' has {ncol(data_in)} columns, but yr_start_col = {yr_start_col}."
    ))
  }
  
  colnames_in <- names(data_in)
  colnames_yr <- colnames_in[yr_start_col:length(colnames_in)]
  
  data_out <- data_in %>% 
    pivot_longer(
      cols = all_of(colnames_yr), 
      names_to = "Year", 
      values_to = value_col_name
      ) %>% 
    mutate(
      Year = suppressWarnings(as.integer(Year))
    )
  
  data_out
}

# ---- Measures (shell metadata) ----
measures <- tribble(
  ~measure_id, ~measure_name, ~sector, ~comp_group, ~tech_type, ~unit, 
  "ashp_rs", "Residential Air Source Heat Pump (space heating)", "Residential",	
  "Space heating", "Demand", "household",
  "hpwh_50", "Residential Heat Pump Water Heater (50 gal)", "Residential", 
  "Water heating", "Demand", "household", 
  "led_res", "Residential LED Lighting (general illumination)",	"Residential", 
  "Lighting", "Demand", "household", 
  "tstat_res", "Residential Smart Thermostat", "Residential", 
  "HVAC controls", "Demand", "household",
  "pv_util", "Utility-Scale Solar PV (one-axis tracking)", "Grid", 
  "Electric generation", "Supply", "MW", 
  "wind_lb", "Land-Based Wind", "Grid", 
  "Electric generation", "Supply", "MW", 
  "batt_4h", "Utility-Scale Li-ion Battery Storage (4-hour)", "Grid", 
  "Flexibility", "Storage", "MW"
)

# --- Read Input Params ----
input_params <- read_xlsx("Carbon_Pathways_Inputs_Sample.xlsx", 
                          sheet = "Input_Params") %>% 
  mutate(
    SheetName = as.character(SheetName), 
    YrStartCol = as.integer(YrStartCol), 
    ValueName = as.character(ValueName)
  )

# ---- Load all input sheets into a named list ----
inputs <- input_params %>% 
  mutate(
    data = pmap(
      list(SheetName, YrStartCol, ValueName), 
      ~fn_read_year(filename_in, ..1, ..2, value_col_name = ..3)
    )
  ) %>% 
  select(SheetName, data) %>% 
  deframe()

# Optional: normalize names (no spaces)
names(inputs) <- names(inputs) %>% 
  str_replace_all("\\s+", "_")

# ---- Drivers built from Stock sheet (replace hard-coded placeholders) ----
# Expecting Stock sheet structure: Stock, Climate Zone, Year columns ...
stock <- inputs[["Stock"]] %>% 
  rename(year = Year)

drivers <- stock %>% 
  mutate(
    sector = case_when(
      str_detect(Stock, regex("household", ignore_case = TRUE)) ~ "Residential", 
      # Fallback; refine when I add grid stock explicitly
      TRUE ~ "Grid"
    )
  ) %>% 
  group_by(year, sector) %>% 
  summarise(
    eligible_units = sum(stock_count_or_floor_area, na.rm = TRUE), 
    .groups = "drop"
  ) %>% 
  filter(year %in% years)

# ---- Placeholder targets ----
targets <- expand_grid(year = years, measure_id = measures$measure_id) %>% 
  mutate(target_adoption_share = NA_real_)

# ---- Shell output table ----
model_shell <- expand_grid(year = years, measure_id = measures$measure_id) %>% 
  left_join(measures, by = "measure_id") %>% 
  left_join(drivers, by = c("year", "sector")) %>% 
  left_join(targets, by = c("year", "measure_id")) %>% 
  mutate(
    adopted_units = eligible_units * target_adoption_share, 
    energy_impact_mmbtu = NA_real_, 
    cost_nominal = NA_real_
  )

glimpse(model_shell)
