library(readxl)
library(dplyr)
library(writexl)
library(janitor)
library("BSol.mapR")


#####################################

ActivityData <- read_excel("data/HSV Data 2025-26.xlsx") %>%
  clean_names() %>%
  select(c("name", "postcode", "category", 
           contains("order"), contains("issued"))
  ) %>%
  mutate(
    postcode = toupper(gsub(" ", "", postcode))
  ) %>%
  left_join(
    #load WestMids geography spreadsheet
    WestMidsData <- read.csv("data/West Midlands postcodes.csv",
                             check.names = F) %>%
      clean_names() %>%
      select("postcode", "lsoa21_code", "ward") %>%
      mutate(
        postcode = gsub(" ", "", postcode)
      ),
    by = join_by("postcode")
  ) %>%
  mutate(
    issued_total_2526 = as.numeric(issued_q1) + 
      as.numeric(issued_q2) +
      as.numeric(issued_q3) +
      as.numeric(issued_q4)
  ) %>%
  select(
    name, lsoa21_code, ward, issued_total_2526
  ) 

write_xlsx(ActivityData, "data/activity-2526-data-processed.xlsx")

### plot distribution of vitamins by ward ###############

ward_issued <- ActivityData %>%
  rename(Ward = ward) %>%
  group_by(Ward) %>%
  summarise(
    issued_total_2526 = sum(issued_total_2526)
  )

map <- plot_map(
  ward_issued,
  value_header = "issued_total_2526",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Number of Vitamins Issued from Sites, by Site Ward (25/26)",
  fill_missing = 0,
  style = "cont",
  breaks = seq(0, 2500, 500),
  textNA = NA,
  palette = "brewer.blues"
  )

save_map(map, "output/vitamins-issued-ward-2526.html")

save_map(map, "output/vitamins-issued-ward-2526.png")

