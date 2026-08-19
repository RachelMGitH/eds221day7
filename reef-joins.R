library(tidyverse)

moorea_coral <- read_csv(
  "reefdata/moorea_coral.csv",
  na = c("", "NA", "ND") # This vector tells read_csv() which values to interpret as missing data
)

moorea_fish <- read_csv(
  "reefdata/moorea_fish.csv",
  na = c("", "NA", "ND")
)

glimpse(moorea_coral)

glimpse(moorea_fish)

# Exercise 1: Wrangle the coral data
# Create a vector called non_coral containing the five non-coral category
# labels: "Sand", "CTB", "Macroalgae", "Non-coralline Crustose Algae", and "Unknown or Other".

non_coral <- c(
  "Sand",
  "CTB",
  "Macroalgae",
  "Non-coralline Crustose Algae",
  "Unknown or Other"
)

# Filter moorea_coral to exclude any row whose Taxonomy_Substrate_or_Functional_Group
# is in non_coral, and to keep only rows where Depth is less than 17.

corals_only <- moorea_coral |>
  filter(
    !Taxonomy_Substrate_or_Functional_Group %in% non_coral,
    Depth < 17
  ) |>
  #Use mutate(), str_sub(), and as.numeric() to pull the four-digit year
  # out of Date (which is formatted "YYYY-MM") into a new column called Year.
  mutate(Year = as.numeric(str_sub(Date, start = 1, end = 4)))

# Summarize at the quadrat level first, then at the whole transect level

coral_summary <- corals_only |>
  # Quadrat-level percent cover
  summarize(
    quadrat_cover = sum(Percent_Cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth, Quad40)
  ) |>
  # Transect-level percent cover
  summarize(
    mean_coral_cover = mean(quadrat_cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth)
  ) |>
  arrange(Year, Site, Depth)

#Wrangle the fish data
# Filter to primary consumers
primary_consumers <- moorea_fish |>
  filter(Coarse_Trophic == "Primary Consumer")

# The total biomass per transect
fish_summary <- primary_consumers |>
  summarize(
    total_biomass = sum(Biomass, na.rm = TRUE),
    .by = c(Site, Habitat, Year)
  ) |>
  arrange(
    Year,
    Site,
    Habitat
  )

# Join the summaries
# USe inner_join, combining the two data frames by site, habitat and year
reef_joined <- coral_summary |>
  inner_join(fish_summary, by = join_by(Site, Habitat, Year))


# Reshape
reef_joined_wide <- reef_joined |>
  select(Site, Habitat, Year, mean_coral_cover) |>
  # Pivot wider to spread out the habitats
  pivot_wider(names_from = Habitat, values_from = mean_coral_cover) |>
  # add a column with the difference between the habitats
  mutate(coral_cover_difference = Forereef - Fringing)
# Make a histogram
ggplot(
  data = reef_joined_wide,
  mapping = aes(x = coral_cover_difference)
) +
  geom_histogram()
