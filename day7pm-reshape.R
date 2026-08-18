library(tidyverse)

surveys <- read_csv("data/surveys.csv")
species <- read_csv("data/species.csv")
plots <- read_csv("data/plots.csv")


# pivot_wider() ----------------------------------------------------------

# Summarize our surveys to get the mean weight by sex
weight_by_sex <- surveys |>
  filter(!is.na(sex)) |>
  summarize(
    mean_weight = mean(weight, na.rm = TRUE),
    .by = c(species_id, sex)
  )

# Pivot wider to see the mean weight by sex in columns
weight_by_sex_wider <- weight_by_sex |>
  pivot_wider(
    # Where do I get the _names_ of the new columns from?
    names_from = sex,
    # Where do I get the _values_ of the new columns from?
    values_from = mean_weight
  )


# Pivot wider is useful when one column contains multiple sets of values
# that you want to use with each other

# Another example
taxa_by_year <- surveys |>
  # Choose a subset of years for convenience
  filter(year >= 1999) |>
  # This join will add the taxa column from species to my survey's subset
  inner_join(
    select(species, species_id, taxa),
    join_by(species_id)
  ) |>
  count(year, taxa)
taxa_by_year

# Pivot this wider, so we have columns for Birds and Rodents
taxa_wider <- taxa_by_year |>
  pivot_wider(
    names_from = taxa,
    values_from = n
  )
taxa_wider


# pivot_longer() ---------------------------------------------------------

taxa_wider
# Pivot back to long format
taxa_wider |>
  pivot_longer(
    # what columns are we pivoting?
    cols = !year,
    # Where are the names going?
    names_to = "taxa",
    # where are the values going?
    values_to = "n"
  )

# pivot_wider() and pivot_longer() are inverses of each other
