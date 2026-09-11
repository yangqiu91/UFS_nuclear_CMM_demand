# install.packages('devtools')
# devtools::install_github('JGCRI/rgcam')
# devtools::install_github('JGCRI/gcamdata')
# install_github('JGCRI/rgcam', build_vignettes=TRUE)
# install.packages('tibble')
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages('ggplot2')
# install.packages('readxl')
# install.packages("writexl")
# install.packages("stringr")

library("devtools")
library("rgcam")
library("gcamdata")
library("tibble")
library('tidyr')
library('dplyr')
library("ggplot2")
library("stringr")
library("readxl")
library("writexl")

# PART I -- Data input ----

# define a few conversion factors

GW_to_KW_conversion <- 1e+6
TWh_to_KWh_conversion <- 1e+6

kg_to_kt_conversion <- 1e-6



GCAM_elec_new_cap_nuc_tech_USA <-
  read.csv("input_data/GCAM_elec_new_cap_nuc_tech_USA.csv") %>%
  as_tibble() %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units, new_capacity = value)

GCAM_elec_gen_nuc_tech_USA <-
  read.csv("input_data/GCAM_elec_gen_nuc_tech_USA.csv") %>%
  as_tibble() %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units, generation = value)

nuclear_tech_cmm_intensity <-
  read.csv("input_data/nuclear_tech_cmm_intensity.csv") %>%
  as_tibble() %>%
  pivot_longer(cols = 3:4, names_to = "demand_type", values_to = "value") %>%
  filter(!is.na(value)) %>%
  rename(material_intensity = value)



# PART II -- CMM demand calculation ----

## 2.1 calculate the material demand for technology installation. ----

GCAM_nuc_tech_USA_installation_cmm_demand <-
  # new capacity is currently five year total, we first annualize it.
  GCAM_elec_new_cap_nuc_tech_USA %>%
  arrange(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units) %>%
  group_by(region, scenario, Trade, ReactorCost, FuelCost, Technology, Units) %>%
  mutate(
    period_end_year = year,
    period_start_year = coalesce(lag(year), year - 5),
    year_gap = period_end_year - period_start_year,
    annual_new_capacity = new_capacity / year_gap
  ) %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units, annual_new_capacity) %>%
  # combine annual new capacity with the material intensity to calculate annual material demand
  left_join(nuclear_tech_cmm_intensity %>%
              filter(demand_type == "Installation_kg_kw"),
            by = c("Technology")) %>%
  mutate(annual_material_demand = annual_new_capacity * GW_to_KW_conversion * material_intensity * kg_to_kt_conversion,
         # unit of material demand as kt -- thousand metric ton
         Units = "kt",
         demand_type = "installation_demand") %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year,
         Material, demand_type, Units, annual_material_demand)


## 2.2 calculate the material demand for technology operation ----

GCAM_nuc_tech_USA_operation_cmm_demand <-

  GCAM_elec_gen_nuc_tech_USA %>%
  # combine annual generation with the material intensity to calculate annual material demand (operation)
  left_join(nuclear_tech_cmm_intensity %>%
              filter(demand_type == "Operation_kg_kwh"),
            by = c("Technology")) %>%

  # Here we took different approach to account for operational material demand based on refuling schedule of different reactor types
  mutate(

    annual_material_demand = if_else(grepl("HTGR", Technology), generation * )

    annual_material_demand = annual_new_capacity * GW_to_KW_conversion * material_intensity * kg_to_kt_conversion,
         # unit of material demand as kt -- thousand metric ton
         Units = "kt",
         demand_type = "installation_demand") %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year,
         Material, demand_type, Units, annual_material_demand)







