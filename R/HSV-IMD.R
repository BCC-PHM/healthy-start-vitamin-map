library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(writexl)
library(viridis)
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
  ) %>%
  select(
    lsoa21, imd_rank, imd_quintile
  )

# Load registered populations
under4_pops <- read_excel("data/BSol-registered-pop-under4-June26.xlsx") %>%
  clean_names()

# Load HSV activity
activity <- read_excel("data/activity-2526-data-processed.xlsx") %>%
  select(name, lsoa21_code, issued_total_2526)

# Calculate vitamin issue rate per 1000 children under 4 in the quintile
vits_data_with_imd <- lsoa_imds %>%
  left_join(
    under4_pops,
    by = join_by("lsoa21" == "lsoa_2021")
  ) %>%
  # join vitamin activity data
  left_join(
    # Sum activity by LSOA
    activity %>%
      group_by(lsoa21_code) %>%
      summarise(
        issued_total_2526 = sum(issued_total_2526),
        sites = paste(name, collapse = " | "),
        num_sites = n()
      ),
    by = join_by("lsoa21" == "lsoa21_code")
  ) %>%
  mutate(
    # impute zeros
    observations = ifelse(is.na(observations), 0, observations),
    issued_total_2526 = ifelse(is.na(issued_total_2526), 0, issued_total_2526),
  )
  

imd_vits_data <- vits_data_with_imd %>%
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
      "Healthy Start Vitamins Issued by LSOA per 1000 Children Under 4 Years Registered to a GP (2025/26)",
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
    limits = c(0, 600),
    expand = c(0, 0)
  )

plt

# Save plot
ggsave("output/vitamins-by-imd-2526.png", width = 6, height = 4)

###############################################################################
#                        Scatter plot of LSOAs                                #
###############################################################################

plt2 <- vits_data_with_imd %>%
  # Calculate rate with errors
  mutate(
    value = 1000*issued_total_2526 / observations,
    num_sites = factor(ifelse(is.na(num_sites), 0, num_sites))
    ) %>%
  ggplot(aes(x = imd_rank, y = issued_total_2526, color = num_sites)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(3284.4, 32844*0.9, 32844/5), 
    labels = c("1\n(Most Deprived)", "2", "3", "4", "5\n(Least Deprived)"),
    limits = c(0, 32844),
    expand = c(0, 0)
  ) +
  geom_vline(xintercept = seq(32844*0.2, 32844*0.8, 32844*0.2),
             linetype = "dashed") +
  scale_color_manual(
    values = c("black", inferno(4, begin = 0.35, end = 0.9))
  ) +
  labs(
    title = stringr::str_wrap(
      "Healthy Start Vitamins Issued by LSOA (2025/26)",
      70
    ),
    y = "",
    x = "IMD Quintile",
    color = "Number of Sites"
  ) 
plt2
ggsave("output/vitamins-by-imd-scatter-count.png",
       plot = plt2, width = 6, height = 4)

interactive_plot <- ggiraph::girafe(ggobj = plt2)
htmltools::save_html(interactive_plot, "output/vitamins-by-imd-scatter-count.html")