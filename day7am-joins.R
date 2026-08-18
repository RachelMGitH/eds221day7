library(tidyverse)

portal_base_url <- "https://raw.githubusercontent.com/weecology/portal-teachingdb/master/"

# Download the data
dir.create("data")

download.file(
  paste0(portal_base_url, "surveys.csv"),
  destfile = "data/surveys.csv"
)

download.file(
  paste0(portal_base_url, "species.csv"),
  destfile = "data/species.csv"
)

download.file(
  paste0(portal_base_url, "plots.csv"),
  destfile = "data/plots.csv"
)

# Read the data
surveys <- read_csv("data/surveys.csv")
species <- read_csv("data/species.csv")
plots <- read_csv("data/plots.csv")

# Keys -------------------------------------------------------------------

# A key is a primary key if it uniquely identifies rows
# in its table
surveys |>
  count(record_id) |>
  filter(n > 1)

# A foreign key connects to another table. It doesn't have to be unique.
surveys |>
  count(species_id) |>
  filter(n > 1)


# Joins ------------------------------------------------------------------

surveys |>
  inner_join(
    species,
    # by specifies the foreign key
    by = join_by(species_id)
  ) |>
  glimpse()


# Joins in analyses ------------------------------------------------------

survey_plots_90s <- surveys |>
  # Keep just the 90s
  filter(year >= 1990 & year < 2000) |>
  #Join with plot data
  inner_join(plots, by = join_by(plot_id))

# Make a bar plot to see the distribution of plot types
ggplot(
  data = survey_plots_90s,
  mapping = aes(
    x = plot_type
  )
) +
  geom_bar() +
  theme_classic()

# Did the krat exclosures catch any krats?
# Genus = Dipodomys
survey_plots_90s |>
  # joins 90s surveys to species
  inner_join(species, by = join_by(species_id)) |>
  #Filter to the krat exclosures
  filter(
    plot_type == "Short-term Krat Exclosure" |
      plot_type == "Long-term Krat Exclosure"
  ) |>
  # Create a column indicating if it's a krat
  mutate(is_krat = genus == "Dipodomys") |>
  # count 'em up
  count(is_krat)
