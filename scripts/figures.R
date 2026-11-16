# Grafiki 
library(cowplot)
library(ggplot2)

fig_temp_dev <- ggplot(
  final_model_data,
  aes(
    x = mean_temperature,
    y = development_rate
  )
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black",
    fill = "grey80",
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.3,
    shape = 16
  ) +
  geom_point(
    data = final_model_data %>%
      filter(
        Year_plus_Site %in% c(
          "2021Ķekava",
          "2025Eleja"
        )
      ),
    size = 3.8,
    shape = 1,
    stroke = 0.8
  ) +
  geom_text(
    data = final_model_data %>%
      filter(Year_plus_Site == "2021Ķekava"),
    aes(label = "2021 Ķekava"),
    nudge_x = 0,
    nudge_y = -0.0025,
    hjust = 1,
    size = 3.4
  ) +
  geom_text(
    data = final_model_data %>%
      filter(Year_plus_Site == "2025Eleja"),
    aes(label = "2025 Eleja"),
    nudge_x = 0.25,
    nudge_y = 0.0012,
    hjust = 0,
    size = 3.4
  ) +
  labs(
    x = "Mean air temperature during generation interval (°C)",
    y = expression("Development rate (day"^{-1}*")")
  ) +
  theme_half_open()

fig_temp_dev

ggsave(
  filename = "figures/temperature_development_rate_plot.png",
  plot = fig_temp_dev,
  width = 7,
  height = 3,
  units = "in",
  dpi = 300
)




# shemas grafiks


library(DiagrammeR)

workflow_fig <- grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = LR,
    bgcolor = white
  ]

  node [
    shape = box,
    style = rounded,
    fontname = Helvetica,
    fontsize = 11,
    color = black,
    penwidth = 1.2,
    margin = 0.12
  ]

  edge [
    color = black,
    penwidth = 1.0,
    arrowsize = 0.7
  ]

  initial [
    label = 'Initial site-year series\\nn = 37'
  ]

  peaks [
    label = 'With candidate peaks\\nn = 36'
  ]

  pairs [
    label = 'With candidate intervals\\nn = 34 series\\n94 intervals'
  ]

  test [
    label = 'Held-out test series\\nn = 6'
  ]

  train [
    label = 'Training series\\nn = 28'
  ]

  test_acc [
    label = 'Accepted test series\\nn = 3'
  ]

  train_acc [
    label = 'Accepted training series\\nn = 22'
  ]

  final [
    label = 'Intervals in final model\\nn = 25'
  ]

  initial -> peaks
  peaks -> pairs

  pairs -> test
  pairs -> train

  test -> test_acc
  train -> train_acc

  test_acc -> final
  train_acc -> final
}
")

workflow_fig

library(DiagrammeRsvg)
library(rsvg)

dir.create("figures", showWarnings = FALSE)

svg_txt <- export_svg(workflow_fig)

writeLines(
  svg_txt,
  "figures/workflow_figure.svg"
)

rsvg_png(
  charToRaw(svg_txt),
  file = "figures/workflow_figure.png",
  width = 2200
)





# kartes attēls (latvija eiropā, vietas Latvijā, ortofoto ar vietu)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

europe <- world %>%
  filter(continent == "Europe")

latvia <- world %>%
  filter(admin == "Latvia")

fig1A <- ggplot() +
  geom_sf(
    data = europe,
    fill = "grey75",
    colour = NA
  ) +
  geom_sf(
    data = latvia,
    fill = "black",
    colour = "black",
    linewidth = 0.3
  ) +
  coord_sf(
    xlim = c(-10, 45),
    ylim = c(35, 72),
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  scale_x_continuous(
    breaks = seq(-10, 40, by = 10),
    labels = function(x) {
      ifelse(
        x < 0,
        paste0(abs(x), "°W"),
        ifelse(x > 0, paste0(x, "°E"), "0°")
      )
    }
  ) +
  scale_y_continuous(
    breaks = seq(35, 70, by = 5),
    labels = function(y) paste0(y, "°N")
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_line(
      colour = "grey90",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "grey40",
      fill = NA,
      linewidth = 0.7
    ),
    
    axis.title = element_blank(),
    
    axis.text = element_text(
      colour = "grey50",
      size = 9
    ),
    
    axis.ticks = element_line(
      colour = "grey40",
      linewidth = 0.4
    ),
    
    axis.ticks.length = unit(2, "mm"),
    
    plot.margin = margin(5, 5, 5, 5)
  )

fig1A

ggsave(
  "figures/Figure1A_Latvia_in_Europe.png",
  plot = fig1A,
  width = 2.5,
  height = 2.5,
  units = "in",
  dpi = 600
)

# punkti lativjā


# =========================
# Figure 1B: coordinate data
# =========================

library(readxl)
library(dplyr)

# 1. Read coordinate table
coords <- read_excel(
  "data/raw/Koordinātu_tabula.xlsx"
)

# 2. Clean coordinate columns and station names
coords <- coords %>%
  mutate(
    across(
      c(N_Site, E_Site, N_Meteo, E_Meteo),
      ~ as.numeric(gsub(",", ".", .x))
    ),
    
    # Convert text "NA" to real missing values
    Meteo_Station_Name = na_if(Meteo_Station_Name, "NA"),
    
    # Remove inconsistent space after "LVGMC-"
    Meteo_Station_Name = gsub(
      "LVGMC- ",
      "LVGMC-",
      Meteo_Station_Name
    )
  )

# 3. Check that coordinates are numeric
str(coords)

# 4. Create one representative point for each sampling site
#    Coordinates are averaged across years because field positions
#    within the same locality vary slightly among years.
sampling_sites <- coords %>%
  group_by(Parish) %>%
  summarise(
    N = mean(N_Site, na.rm = TRUE),
    E = mean(E_Site, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Create table of unique meteorological stations
meteo_stations <- coords %>%
  filter(!is.na(Meteo_Station_Name)) %>%
  distinct(
    Meteo_Station_Name,
    N_Meteo,
    E_Meteo
  )

# 6. Inspect resulting tables
sampling_sites
meteo_stations

coords <- coords %>%
  mutate(
    across(
      c(N_Site, E_Site, N_Meteo, E_Meteo),
      ~ as.numeric(na_if(gsub(",", ".", .x), "NA"))
    ),
    Meteo_Station_Name = na_if(Meteo_Station_Name, "NA"),
    Meteo_Station_Name = gsub(
      "LVGMC- ",
      "LVGMC-",
      Meteo_Station_Name
    )
  )



library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Latvia polygon
latvia_map <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(admin == "Latvia")

# Convert sampling sites to sf
sampling_sites_sf <- st_as_sf(
  sampling_sites,
  coords = c("E", "N"),
  crs = 4326
)

# Convert meteorological stations to sf
meteo_stations_sf <- st_as_sf(
  meteo_stations,
  coords = c("E_Meteo", "N_Meteo"),
  crs = 4326
)

library(ggrepel)

fig1B <- ggplot() +
  geom_sf(
    data = latvia_map,
    fill = "grey90",
    colour = "grey45",
    linewidth = 1
  ) +
  
  # Sampling sites
  geom_sf(
    data = sampling_sites_sf,
    aes(shape = "Sampling site"),
    fill = "grey80",
    colour = "black",
    size = 2.4,
    stroke = 0.4
  ) +
  
  # Meteorological stations
  geom_sf(
    data = meteo_stations_sf,
    aes(shape = "Meteorological station"),
    colour = "grey20",
    size = 2.8
  ) +
  
  # Sampling site labels
  geom_text_repel(
    data = cbind(
      sampling_sites,
      st_coordinates(sampling_sites_sf)
    ),
    aes(
      x = X,
      y = Y,
      label = Parish
    ),
    size = 3,
    box.padding = 0.35,
    point.padding = 0.5,
    min.segment.length = Inf,
    max.overlaps = Inf
  ) +
  
  scale_shape_manual(
    values = c(
      "Sampling site" = 21,
      "Meteorological station" = 17
    ),
    name = NULL
  ) +
  
  coord_sf(
    xlim = c(20.8, 28.3),
    ylim = c(55.5, 58.2),
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  
  scale_x_continuous(
    breaks = seq(21, 28, by = 1),
    labels = function(x) paste0(x, "°E")
  ) +
  
  scale_y_continuous(
    breaks = seq(56, 58, by = 0.5),
    labels = function(y) paste0(y, "°N")
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major = element_line(
      colour = "grey90",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "grey45",
      fill = NA,
      linewidth = 0.7
    ),
    
    axis.title = element_blank(),
    
    axis.text = element_text(
      colour = "grey50",
      size = 10
    ),
    
    axis.ticks = element_line(
      colour = "grey45",
      linewidth = 0.4
    ),
    
    axis.ticks.length = unit(2, "mm"),
    
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    
    plot.margin = margin(5, 5, 5, 5)
  )

fig1B
ggsave(
  "figures/Figure1B_sampling_sites.png",
  plot = fig1B,
  width = 4.5,
  height = 3.2,
  units = "in",
  dpi = 600
)



#===========================Supplementary figs

#Figure S1 — bootstrap distributions.
boot_results <- readRDS(
  "data/processed/final_generation_model_bootstrap.rds"
)

library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
boot_results <- readRDS(
  "data/processed/final_generation_model_bootstrap.rds"
)

final_model_parameters <- readRDS(
  "data/processed/final_generation_model_parameters.rds"
)

# Prepare plotting data
boot_plot_data <- bind_rows(
  tibble(
    panel = "A. Lower developmental threshold, Tbase (°C)",
    value = boot_results$Tbase
  ),
  tibble(
    panel = "B. Thermal requirement, Ksel (degree-days)",
    value = boot_results$K
  )
)

# Reference lines: final estimate, bootstrap median, 95% CI
ref_lines <- tibble(
  panel = c(
    "A. Lower developmental threshold, Tbase (°C)",
    "B. Thermal requirement, Ksel (degree-days)"
  ),
  estimate = c(
    final_model_parameters$Tbase,
    final_model_parameters$K
  ),
  boot_median = c(
    final_model_parameters$Tbase_boot_median,
    final_model_parameters$K_boot_median
  ),
  lower = c(
    as.numeric(final_model_parameters$Tbase_boot_lower),
    as.numeric(final_model_parameters$K_boot_lower)
  ),
  upper = c(
    as.numeric(final_model_parameters$Tbase_boot_upper),
    as.numeric(final_model_parameters$K_boot_upper)
  )
)

# Plot
figS1 <- ggplot(
  boot_plot_data,
  aes(x = value)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "grey80",
    colour = "grey35",
    linewidth = 0.3
  ) +
  geom_density(
    linewidth = 0.7,
    colour = "black"
  ) +
  
  # Final model estimate
  geom_vline(
    data = ref_lines,
    aes(xintercept = estimate),
    linewidth = 0.8,
    colour = "black"
  ) +
  
  # 95% bootstrap interval
  geom_vline(
    data = ref_lines,
    aes(xintercept = lower),
    linetype = "dashed",
    linewidth = 0.7,
    colour = "grey20"
  ) +
  geom_vline(
    data = ref_lines,
    aes(xintercept = upper),
    linetype = "dashed",
    linewidth = 0.7,
    colour = "grey20"
  ) +
  
  facet_wrap(
    ~ panel,
    scales = "free",
    ncol = 1
  ) +
  
  labs(
    x = NULL,
    y = "Density"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(
      fill = "white",
      colour = "grey40"
    ),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(
      colour = "black",
      size = 9
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

figS1


ggsave(
  filename = "figures/Figure_S1_bootstrap_distributions.png",
  plot = figS1,
  width = 6.5,
  height = 7.5,
  units = "in",
  dpi = 600
)



# Figure S2 — regression influence diagnostics

library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
final_generation_model <- lm(
  development_rate ~ mean_temperature,
  data = final_model_data
)

# Influence diagnostics
influence_data <- final_model_data %>%
  mutate(
    leverage = hatvalues(final_generation_model),
    studentized_residual = rstudent(final_generation_model),
    cooks_distance = cooks.distance(final_generation_model),
    observation = row_number()
  )

# -------------------------
# Panel A
# -------------------------


figS2A <- ggplot(
  influence_data,
  aes(
    x = leverage,
    y = studentized_residual
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = c(-2, 2),
    linetype = "dashed",
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_point(
    size = 2.2,
    colour = "black"
  ) +
  geom_text_repel(
    data = influence_data %>%
      filter(
        Year_plus_Site %in%
          c("2021Kekavas", "2025Elejas")
      ),
    aes(
      label = case_when(
        Year_plus_Site == "2021Kekavas" ~ "2021 Ķekava",
        Year_plus_Site == "2025Elejas" ~ "2025 Eleja"
      )
    ),
    size = 3.2,
    box.padding = 0.4,
    point.padding = 0.3,
    min.segment.length = Inf
  ) +
  labs(
    x = "Leverage",
    y = "Studentized residual",
    title = "A"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

# -------------------------
# Panel B
# -------------------------

figS2B <- ggplot(
  influence_data,
  aes(
    x = observation,
    y = cooks_distance
  )
) +
  geom_segment(
    aes(
      xend = observation,
      y = 0,
      yend = cooks_distance
    ),
    linewidth = 0.5,
    colour = "grey40"
  ) +
  geom_point(
    size = 2,
    colour = "black"
  ) +
  geom_text_repel(
    data = influence_data %>%
      filter(
        Year_plus_Site %in%
          c("2021Kekavas", "2025Elejas")
      ),
    aes(
      label = case_when(
        Year_plus_Site == "2021Kekavas" ~ "2021 Ķekava",
        Year_plus_Site == "2025Elejas" ~ "2025 Eleja"
      )
    ),
    size = 3.2,
    box.padding = 0.4,
    point.padding = 0.3,
    min.segment.length = Inf
  ) +
  labs(
    x = "Observation",
    y = "Cook's distance",
    title = "B"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

# Combine panels
figS2 <- figS2A / figS2B

figS2

ggsave(
  "figures/Figure_S2_influence_diagnostics.png",
  plot = figS2,
  width = 6.5,
  height = 7,
  units = "in",
  dpi = 600
)

