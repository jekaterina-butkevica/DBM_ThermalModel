# ============================================================
# Supplementary tables
# Table S1: Sampling sites and meteorological stations by year
# ============================================================

library(readxl)
library(dplyr)
library(flextable)
library(officer)

# ------------------------------------------------------------
# 1. Read coordinate data
# ------------------------------------------------------------

coords <- read_excel(
  "data/raw/Koordinātu_tabula.xlsx"
)

# ------------------------------------------------------------
# 2. Clean coordinate data
# ------------------------------------------------------------

coords <- coords %>%
  mutate(
    across(
      c(N_Site, E_Site, N_Meteo, E_Meteo),
      ~ as.numeric(na_if(gsub(",", ".", .x), "NA"))
    ),
    
    Meteo_Station_Name = na_if(Meteo_Station_Name, "NA"),
    
    # Standardize spacing in station names
    Meteo_Station_Name = gsub(
      "LEGMC- ",
      "LEGMC-",
      Meteo_Station_Name
    )
  )

# ------------------------------------------------------------
# 3. Keep only site-year combinations with actual sampling data
# ------------------------------------------------------------

table_s1 <- coords %>%
  filter(
    !is.na(N_Site),
    !is.na(E_Site)
  ) %>%
  arrange(
    Year,
    Parish
  ) %>%
  transmute(
    `Year` = Year,
    `Sampling site` = Parish,
    `Site latitude (°N)` = N_Site,
    `Site longitude (°E)` = E_Site,
    `Meteorological station` = Meteo_Station_Name,
    `Station latitude (°N)` = N_Meteo,
    `Station longitude (°E)` = E_Meteo
  )

# ------------------------------------------------------------
# 4. Round coordinates for presentation
# ------------------------------------------------------------

table_s1 <- table_s1 %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

table_s1
# ------------------------------------------------------------
# 5. Format Table S1
# ------------------------------------------------------------

ft_s1 <- flextable(table_s1)

ft_s1 <- border_remove(ft_s1)

main_border <- fp_border(
  color = "black",
  width = 1
)

ft_s1 <- hline_top(
  ft_s1,
  border = main_border,
  part = "header"
)

ft_s1 <- hline(
  ft_s1,
  i = 1,
  border = main_border,
  part = "header"
)

ft_s1 <- hline_bottom(
  ft_s1,
  border = main_border,
  part = "body"
)

ft_s1 <- bold(
  ft_s1,
  part = "header"
)

ft_s1 <- align(
  ft_s1,
  align = "center",
  part = "header"
)

# Text columns left aligned
ft_s1 <- align(
  ft_s1,
  j = c(
    "Sampling site",
    "Meteorological station"
  ),
  align = "left",
  part = "body"
)

# Numeric columns centred
ft_s1 <- align(
  ft_s1,
  j = c(
    "Year",
    "Site latitude (°N)",
    "Site longitude (°E)",
    "Station latitude (°N)",
    "Station longitude (°E)"
  ),
  align = "center",
  part = "body"
)

ft_s1 <- valign(
  ft_s1,
  valign = "center",
  part = "all"
)

ft_s1 <- fontsize(
  ft_s1,
  size = 9,
  part = "all"
)

ft_s1 <- fontsize(
  ft_s1,
  size = 9.5,
  part = "header"
)

ft_s1 <- padding(
  ft_s1,
  padding.top = 3,
  padding.bottom = 3,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

ft_s1 <- autofit(ft_s1)

ft_s1

# ------------------------------------------------------------
# 8. Save Table S1
# ------------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE
)

save_as_docx(
  "Table S1" = ft_s1,
  path = "tables/Table_S1_sampling_sites_meteorological_stations.docx"
)




###### Otrā tabula
# ============================================================
# Table S2: Interval-selection outcomes
# ============================================================

library(dplyr)
library(flextable)
library(officer)

# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------

train_validation <- readRDS(
  "data/processed/train_pair_validation.rds"
)

test_validation <- readRDS(
  "data/processed/test_pair_validation.rds"
)

train_test_split <- readRDS(
  "data/processed/train_test_split.rds"
)

candidate_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
)

pair_selection_parameters <- readRDS(
  "data/processed/pair_selection_parameters.rds"
)



validation_all <- bind_rows(
  train_validation %>%
    mutate(
      Dataset = "Training"
    ),
  
  test_validation %>%
    mutate(
      Dataset = "Held-out test"
    )
)

dim(validation_all)


pair_dates <- candidate_pairs %>%
  mutate(
    pair_id = paste(
      peak_1,
      peak_2,
      sep = "-"
    )
  ) %>%
  select(
    Year_plus_Site,
    pair_id,
    peak_1_date,
    peak_2_date,
    generation_days
  ) %>%
  distinct()



table_s2_data <- validation_all %>%
  left_join(
    train_test_split %>%
      select(
        Year_plus_Site,
        Year,
        Site
      ),
    by = "Year_plus_Site"
  ) %>%
  left_join(
    pair_dates,
    by = c(
      "Year_plus_Site",
      "modal_pair" = "pair_id"
    )
  )

table_s2_data %>%
  select(
    Year_plus_Site,
    Dataset,
    peak_1_date,
    peak_2_date,
    generation_days,
    modal_pair_fraction,
    median_error,
    reliable_pair,
    stable_pair,
    accepted_pair
  ) %>%
  print(n = Inf)



table_s2 <- table_s2_data %>%
  arrange(
    Year,
    Site
  ) %>%
  transmute(
    `Site–year` = Year_plus_Site,
    
    `Dataset` = Dataset,
    
    `First peak` =
      format(
        as.Date(peak_1_date),
        "%Y-%m-%d"
      ),
    
    `Second peak` =
      format(
        as.Date(peak_2_date),
        "%Y-%m-%d"
      ),
    
    `Generation duration (days)` =
      round(
        generation_days,
        1
      ),
    
    `Selection stability` =
      sprintf(
        "%.0f%%",
        100 * modal_pair_fraction
      ),
    
    `Median thermal deviation` =
      sprintf(
        "%.1f%%",
        100 * median_error
      ),
    
    `Thermal criterion` =
      if_else(
        reliable_pair,
        "Pass",
        "Fail"
      ),
    
    `Stability criterion` =
      if_else(
        stable_pair,
        "Pass",
        "Fail"
      ),
    
    `Accepted` =
      if_else(
        accepted_pair,
        "Yes",
        "No"
      )
  )

table_s2

# noformejums
ft_s2 <- flextable(
  table_s2
)

ft_s2 <- border_remove(
  ft_s2
)

main_border <- fp_border(
  color = "black",
  width = 1
)

# Top border
ft_s2 <- hline_top(
  ft_s2,
  border = main_border,
  part = "header"
)

# Line below header
ft_s2 <- hline(
  ft_s2,
  i = 1,
  border = main_border,
  part = "header"
)

# Bottom border
ft_s2 <- hline_bottom(
  ft_s2,
  border = main_border,
  part = "body"
)

# Header
ft_s2 <- bold(
  ft_s2,
  part = "header"
)

ft_s2 <- align(
  ft_s2,
  align = "center",
  part = "header"
)

# First two columns left
ft_s2 <- align(
  ft_s2,
  j = c(
    "Site–year",
    "Dataset"
  ),
  align = "left",
  part = "body"
)

# Everything else centred
ft_s2 <- align(
  ft_s2,
  j = 3:ncol(table_s2),
  align = "center",
  part = "body"
)

ft_s2 <- valign(
  ft_s2,
  valign = "center",
  part = "all"
)

ft_s2 <- fontsize(
  ft_s2,
  size = 9,
  part = "all"
)

ft_s2 <- fontsize(
  ft_s2,
  size = 9.5,
  part = "header"
)

ft_s2 <- padding(
  ft_s2,
  padding.top = 3,
  padding.bottom = 3,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

ft_s2 <- autofit(
  ft_s2
)

ft_s2

error_threshold <- round(
  pair_selection_parameters$error_upper_fence,
  3
)

stability_threshold <- round(
  pair_selection_parameters$stability_lower_fence,
  3
)

table_note <- paste0(
  "Selection stability is the proportion of the 13 candidate ",
  "Tbase scenarios (0–12 °C) in which the modal peak pair was selected. ",
  "Median thermal deviation is the median relative deviation from the ",
  "training-derived thermal requirement across those scenarios. ",
  "Pairs passed the thermal criterion when median thermal deviation ≤ ",
  error_threshold,
  " and the stability criterion when selection stability ≥ ",
  stability_threshold,
  ". A pair was accepted only when both criteria were met."
)

ft_s2 <- add_footer_lines(
  ft_s2,
  values = table_note
)

ft_s2 <- fontsize(
  ft_s2,
  size = 8,
  part = "footer"
)

ft_s2 <- italic(
  ft_s2,
  part = "footer"
)

ft_s2

# saglabat
save_as_docx(
  "Table S2" = ft_s2,
  path = "tables/Table_S2_interval_selection_outcomes.docx"
)

