library(readxl)
library(dplyr)
library(writexl)
library(janitor)
library("BSol.mapR")
library(viridis)

ActivityData <- read_excel("data/HSV Data 2025-26.xlsx") %>%
  clean_names() %>%
  select(c("name", "postcode", "category", 
           contains("order"), contains("issued"))
  ) %>%
  mutate(
    postcode_simplified = toupper(gsub(" ", "", postcode))
  ) %>%
  left_join(
    #load WestMids geography spreadsheet
    WestMidsData <- read.csv("data/West Midlands postcodes.csv",
                             check.names = F) %>%
      clean_names() %>%
      rename(postcode_correct = postcode) %>%
      select("postcode_correct", "lsoa21_code", "ward") %>%
      mutate(
        postcode_simplified = gsub(" ", "", postcode_correct)
      ),
    by = join_by("postcode_simplified")
  ) %>%
  mutate(
    issued_total_2526 = as.numeric(issued_q1) + 
      as.numeric(issued_q2) +
      as.numeric(issued_q3) +
      as.numeric(issued_q4)
  ) %>%
  select(-postcode) %>%
  rename(postcode = postcode_correct) %>%
  select(
    name, lsoa21_code, postcode, ward, issued_total_2526
  ) 

write_xlsx(ActivityData, "data/activity-2526-data-processed.xlsx")

### plot distribution of vitamins by ward ###############

ward_issued <- ActivityData %>%
  rename(Ward = ward) %>%
  group_by(Ward) %>%
  summarise(
    issued_total_2526 = sum(issued_total_2526)
  )

###############################################################################
#                      Plot basic map of HSV issued                           #
###############################################################################

map1 <- plot_map(
  ward_issued,
  value_header = "issued_total_2526",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Number of Vitamins Issued from Sites by Site Ward (25/26)",
  fill_missing = 0,
  style = "cont",
  breaks = seq(0, 2500, 500),
  textNA = NA,
  palette = c("#FFFFFF", magma(50, direction = -1))
  )

save_map(map1, "output/vitamins-issued-ward-2526.html")

save_map(map1, "output/vitamins-issued-ward-2526.png")

###############################################################################
#                              Add HSV Sites                                  #
###############################################################################

map2 <- add_points(
  map1,
  ActivityData %>%
    rename(Postcode = postcode)
)

save_map(map2, "output/vitamins-issued-ward-2526-with-sites.html")

save_map(map2, "output/vitamins-issued-ward-2526-with-sites.png")