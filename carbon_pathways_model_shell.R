library(tidyverse)

years <- 2025:2030

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

# Placeholder stock/activity drivers (replace with real data later)
drivers <- expand_grid(year = years, segment = c("Residential", "Grid")) %>% 
  mutate(
    # interpretation placeholders - swap these later
    eligible_units = case_when(
      # households
      segment == "Residential" ~ 1e6, 
      # MW base
      segment == "Grid" ~ 1e4 
    )
  )

# Placeholder targets: what % of eligible units adopt each year
targets <- expand_grid(year = years, measure_id = measures$measure_id) %>% 
  # fill later
  mutate(target_adoption_share = NA_real_)

# Shell output table (no math yet): one row per measure-year
model_shell <- expand_grid(year = years, measure_id = measures$measure_id) %>% 
  left_join(measures, by = "measure_id") %>% 
  left_join(drivers %>% 
              rename(sector = segment), by = c("year", "sector")) %>% 
  left_join(targets, by = c("year", "measure_id")) %>% 
  mutate(
    adopted_units = eligible_units * target_adoption_share, 
    energy_impact_mmbtu = NA_real_, 
    cost_nominal = NA_real_
  )

model_shell %>% glimpse
