library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(PHEindicatormethods)

# Load postcode data and calculate IMD for Birmingham LSOAs
lsoa_imds <- read.csv(
  "data/West Midlands postcodes.csv"
  ) %>%
  clean_names() %>%
  filter(
    district == "Birmingham"
  ) %>%
  rename(
    lsoa21 = "lsoa21_code",
    imd_rank = index_of_multiple_deprivation
  ) %>%
  group_by(lsoa21) %>%
  summarise(
    # This is just the IMD rank since all 
    # postcodes in an LSOA have the same rank
    imd_rank = mean(imd_rank)
    ) %>%
  mutate(
    imd_quintile =  floor(5 * imd_rank/32844 + 1)
  ) 

# Load registered populations
under4_pops <- read_excel("data/BSol-registered-pop-under4-June26.xlsx") %>%
  clean_names()

# Load HSV activity
activity <- read_excel("data/activity-2526-data-processed.xlsx") %>%
  select(lsoa21_code, issued_total_2526)

# Calculate vitamin issue rate per 1000 children under 4 in the quintile
imd_vits_data <- lsoa_imds %>%
  left_join(
    under4_pops,
    by = join_by("lsoa21" == "lsoa_2021")
  ) %>%
  # join vitamin activity data
  left_join(
    activity,
    by = join_by("lsoa21" == "lsoa21_code")
  ) %>%
  mutate(
    # impute zeros
    observations = ifelse(is.na(observations), 0, observations),
    issued_total_2526 = ifelse(is.na(issued_total_2526), 0, issued_total_2526),
  ) %>%
  group_by(
    imd_quintile
  ) %>%
  summarise(
    under4_pop = sum(observations),
    issued_total_2526 = sum(issued_total_2526)
  ) %>%
  # Calculate rate with errors
  phe_rate(
    x = issued_total_2526,
    n = under4_pop,
    multiplier = 1000
  )

# Save issue rate data
write_xlsx(imd_vits_data, "output/vitamins-by-imd-2526.xlsx")

# Plot issue rate
plt <- ggplot(
  imd_vits_data, aes(x = imd_quintile, y = value)
) +
  geom_col(fill = "#699AC2") +
  geom_errorbar(
    aes(ymin = lowercl, ymax = uppercl),
    width = 0.3) +
  theme_minimal() +
  labs(
    title = stringr::str_wrap(
      "Healthy Start Vitamins Issued per 1000 Children Under 4 Years Registered to a GP (2025/26)",
      70
    ),
    y = "",
    x = "IMD Quintile"
  ) +
  scale_x_continuous(
    breaks = 1:5, 
    labels = c("1\n(Most Deprived)", "2", "3", "4", "5\n(Least Deprived)")
  ) +
  scale_y_continuous(
    limits = c(0, 500),
    expand = c(0, 0)
  )

plt

# Save plot
ggsave("output/vitamins-by-imd-2526.png", width = 6, height = 4)