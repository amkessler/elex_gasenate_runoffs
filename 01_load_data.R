library(tidyverse)
library(lubridate)
library(janitor)


#for filtering by selected date below, choose your date here
chosen_date <- "2020-09-29"



# ACTBLUE FILINGS ####

# contribs

#import zipped file 
actblue_contribs_all <- read_csv("raw_data/actblue_oct_monthly_sa1464847.csv.zip", col_types = cols(.default = "c"))

#format columns and add name column
actblue_contribs_all <- actblue_contribs_all %>% 
  mutate(
    amount = as.numeric(amount),
    aggregate_amount = as.numeric(aggregate_amount),
    date = ymd(date),
    cmte_name = "ACTBLUE"
  ) %>% 
  select(cmte_name, everything())

glimpse(actblue_contribs_all)

#filter for only records after our specified date
actblue_contribs_selectdates <- actblue_contribs_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(actblue_contribs_selectdates, "processed_data/actblue_contribs_selectdates.rds")
