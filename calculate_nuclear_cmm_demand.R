install.packages('devtools')
devtools::install_github('JGCRI/rgcam')
devtools::install_github('JGCRI/gcamdata')
install_github('JGCRI/rgcam', build_vignettes=TRUE)
install_github('JGCRI/gcamdata', build_vignettes=TRUE)
install.packages('tibble')
install.packages("tidyr")
install.packages("dplyr")
install.packages('ggplot2')
install.packages('readxl')
install.packages("writexl")
install.packages("stringr")
install.packages("viridis")

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
library(viridis)

# PART I -- Data input ----

# define a few conversion factors

GW_to_KW_conversion <- 1e+6
TWh_to_KWh_conversion <- 1e+9
kg_to_kt_conversion <- 1e-6



GCAM_elec_new_cap_nuc_tech_USA <-
  read.csv("input_data/GCAM_nuc_cap_new_tech_2026-10-03.csv") %>%
  as_tibble() %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units, new_capacity = value)

GCAM_elec_gen_nuc_tech_USA <-
  read.csv("input_data/elec_gen_nuc_tech_USA_2026-10-03.csv") %>%
  as_tibble() %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year, Units, generation = value)

nuclear_tech_cmm_intensity <-
  read.csv("input_data/nuclear_tech_cmm_intensity.csv") %>%
  as_tibble() %>%
  pivot_longer(cols = 3:4, names_to = "demand_type", values_to = "value") %>%
  filter(!is.na(value)) %>%
  rename(material_intensity = value)

unique(GCAM_elec_new_cap_nuc_tech_USA$scenario)
unique(GCAM_elec_gen_nuc_tech_USA$scenario)

# The operational material intensity is calculated based on the reactor refueling schedule.
# The total operational material demand associated with each refueling event is divided by
# the reactor's annual electricity output to obtain the operational material intensity (kg/kWh).
#
# Because reactor types have different refueling schedules, an adjustment is needed to
# annualize the operational material intensity:
#
# - HTGR reactors are refueled annually, so no adjustment is needed and the calculated
#   operational material intensity is used directly.

# - Nuclear - Legacy and Nuclear - Large -PWR are refueled every 15 years, and the material
#   intensity data we use here levelized the total material demand needed for refueling to annual basis,
#   so no adjustment is needed and the calculated operational material intensity is used directly.
#
# - Other reactor types are refueled every 18 months. Dividing the material demand per
#   refueling event by annual electricity output would overestimate the annual material
#   demand because one refueling event corresponds to 1.5 years of operation. Therefore,
#   a factor of 2/3 is applied to the operational material intensity to account for the
#   18-month refueling cycle.

nuclear_tech_cmm_intensity_adjust <-
  nuclear_tech_cmm_intensity %>%
  filter(demand_type == "Operation_kg_kwh") %>%
  mutate(adjusted_material_intensity = if_else(!grepl("HTGR|Nuclear - Legacy|Nuclear - Large - PWR", Technology),
                                               material_intensity * (2/3),
                                               material_intensity)) %>%
  select(Technology, Material,  demand_type, material_intensity = adjusted_material_intensity) %>%
  rbind(nuclear_tech_cmm_intensity %>%
          filter(demand_type == "Installation_kg_kw"))

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
  left_join(nuclear_tech_cmm_intensity_adjust %>%
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
  left_join(nuclear_tech_cmm_intensity_adjust %>%
              filter(demand_type == "Operation_kg_kwh"),
            by = c("Technology")) %>%
  filter(!is.na(material_intensity)) %>%

  mutate(annual_material_demand = generation * TWh_to_KWh_conversion * material_intensity * kg_to_kt_conversion,
         # unit of material demand as kt -- thousand metric ton
         Units = "kt",
         demand_type = "operation_demand") %>%
  select(region, scenario, Trade, ReactorCost, FuelCost, Technology, year,
         Material, demand_type, Units, annual_material_demand)

# PART III -- Demand results output ----
GCAM_nuc_tech_USA_cmm_demand_all <-
  GCAM_nuc_tech_USA_installation_cmm_demand %>%
  rbind(GCAM_nuc_tech_USA_operation_cmm_demand)

write.csv(GCAM_nuc_tech_USA_cmm_demand_all, "outputs/GCAM_nuc_tech_USA_cmm_demand_all.csv")

unique(GCAM_nuc_tech_USA_cmm_demand_all$scenario)

# PART IV -- Visualization ----

line_color <-   c(
  "100% Domestic" = "black",

  "100% Domestic - Adv. Fuel" = "#0072B2",
  "100% Domestic - Adv. Reactor" = "#CC79A7",
  "100% Domestic - Adv. Nuc" = "#E69F00",
  "100% Domestic - Expensive U" = "#009E73"
)

scenario_include <-
  c(
    # "Current Trade",
    # "Limited Trade",
    "100% Domestic",

    # "Limited Trade - Adv. Reactor",
    # "Limited Trade - Adv. Fuel",
    # "Limited Trade - Adv. Nuc",

    "100% Domestic - Adv. Fuel",
    "100% Domestic - Adv. Reactor",
    "100% Domestic - Adv. Nuc",

    # "Unrestricted Trade",
    "100% Domestic - Expensive U"
  )


GCAM_nuc_tech_USA_cmm_demand_all_use <-
  GCAM_nuc_tech_USA_cmm_demand_all %>%
  group_by(region, scenario, Trade, ReactorCost, FuelCost, year, Material, Units) %>%
  summarise(annual_material_demand = sum(annual_material_demand)) %>%
  filter(scenario %in% scenario_include)

GCAM_nuc_tech_USA_cmm_demand_all_use %>% filter(year == 2100) %>% ungroup() %>%
  group_by(Material) %>%
  mutate(max_value = max(annual_material_demand),
         min_value = min(annual_material_demand))


material_demand_plot <-
  ggplot(
  GCAM_nuc_tech_USA_cmm_demand_all_use %>% filter(year >= 2025),
  aes(x = year, y = annual_material_demand, color = scenario, group = scenario)
) +
  geom_line(linewidth = 1.3) +
  facet_wrap(~ Material, scales = "free_y", ncol = 3) +
  scale_x_continuous(
    limits = c(2020, 2100),
    breaks = seq(2020, 2100, 20)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(values = line_color) +
  labs(x = "Year", y = "Annual material demand (kt)", color = "Scenario") +
  guides(color = guide_legend(nrow = 2)) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  "outputs/material_demand_plot.jpg",
  plot = material_demand_plot,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600,
  # compression = "lzw",
  bg = "white"
)



bar_plot_data <- GCAM_nuc_tech_USA_cmm_demand_all %>%
  filter(year %in% c(2030, 2050)) %>%
  mutate(
    year = factor(year),
    demand_type = recode(
      demand_type,
      installation_demand = "Installation",
      operation_demand = "Operation"
    )
  )%>%
  filter(scenario %in% scenario_include)

bar_plot_data$scenario <- factor(
  bar_plot_data$scenario,
  levels = c(
    "100% Domestic",
    "100% Domestic - Adv. Fuel",
    "100% Domestic - Adv. Reactor",
    "100% Domestic - Adv. Nuc",
    "100% Domestic - Expensive U"
  )
)

make_bar <- function(data, fill, fill_title, colors = NULL) {

  p <- ggplot(
    data,
    aes(
      x = year,
      y = annual_material_demand,
      fill = .data[[fill]]
    )
  ) +
    geom_col(
      width = 0.5,
      colour = "white",
      linewidth = 0.15
    ) +
    facet_grid(
      Material ~ scenario,
      scales = "free_y"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = "Year",
      y = "Annual material demand (kt)",
      fill = fill_title
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),
      strip.background = element_rect(
        fill = "white",
        colour = "black",
        linewidth = 0.5
      ),
      strip.text = element_text(face = "bold"),
      panel.spacing = grid::unit(0.8, "lines")
    )

  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors)
  }

  return(p)
}


## Figure 1: Installation versus operation demand

fig1_data <- bar_plot_data %>%
  group_by(Material, scenario, year, demand_type) %>%
  summarise(annual_material_demand = sum(annual_material_demand, na.rm = TRUE),
            .groups = "drop")

fig1 <- make_bar(
  fig1_data,
  "demand_type",
  "Demand type",
  c("Installation" = "#0072B2", "Operation" = "#E69F00")
)

fig1


## Figure 2: Demand by reactor technology
technology_colors <- c(
  "Nuclear - Large - PWR"       = "#0072B2",  # blue
  "Nuclear - SMR - BWR"         = "#56B4E9",  # light blue
  "Nuclear - SMR - HTGR"        = "#009E73",  # green
  "Nuclear - SMR - MSR"         = "#CC79A7",  # purple
  "Nuclear - SMR - SFR"         = "#D55E00",  # vermilion
  "Nuclear - Legacy"            = "#6C6C6C",  # gray
  "Nuclear - SMR - PWR modular" = "#C49A00"   # dark yellow
)

fig2_data <- bar_plot_data %>%
  group_by(Material, scenario, year, Technology) %>%
  summarise(
    annual_material_demand = sum(
      annual_material_demand,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Technology = factor(
      Technology,
      levels = names(technology_colors)
    )
  )

fig2 <- make_bar(
  data = fig2_data,
  fill = "Technology",
  fill_title = "Reactor technology",
  colors = technology_colors
)

fig2


ggsave(
  "outputs/US_nuclear_CMM_demand_by_type.jpg",
  plot = fig1,
  width = 11,
  height = 9,
  units = "in",
  dpi = 600,
  # compression = "lzw",
  bg = "white"
)

ggsave(
  "outputs/US_nuclear_CMM_demand_by_tech.jpg",
  plot = fig2,
  width = 11,
  height = 9,
  units = "in",
  dpi = 600,
  # compression = "lzw",
  bg = "white"
)


## sensitivity figur on Hf demand

GCAM_nuc_tech_USA_cmm_demand_hf_sensitivity <-
  GCAM_nuc_tech_USA_cmm_demand_all %>% filter(scenario == "100% Domestic",
                                              Material == "Hafnium") %>%
  rbind(GCAM_nuc_tech_USA_cmm_demand_all %>% filter(scenario == "100% Domestic",
                                                    Material == "Hafnium",
                                                    Technology == "Nuclear - Legacy") %>%
          mutate(scenario = "100% Domestic (AP1000)")) %>%
  group_by(region, scenario, year, Material, Units) %>%
  summarise(annual_material_demand = sum(annual_material_demand))


hf_sensitivity_plot <-
  ggplot(
    GCAM_nuc_tech_USA_cmm_demand_hf_sensitivity %>% filter(year >= 2025),
    aes(x = year, y = annual_material_demand, color = scenario, group = scenario)
  ) +
  geom_line(linewidth = 1.3) +
  facet_wrap(~ Material, scales = "free_y", ncol = 3) +
  scale_x_continuous(
    limits = c(2020, 2100),
    breaks = seq(2020, 2100, 20)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(values = c(
    "100% Domestic" = "black",

    "100% Domestic (AP1000)" = "#0072B2")) +
  labs(x = "Year", y = "Annual material demand (kt)", color = "Scenario") +
  guides(color = guide_legend(nrow = 2)) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  "outputs/hf_sensitivity_plot.jpg",
  plot = hf_sensitivity_plot,
  width = 4,
  height = 4.5,
  units = "in",
  dpi = 600,
  # compression = "lzw",
  bg = "white"
)






