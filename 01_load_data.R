library(tidyverse)
library(lubridate)
library(janitor)


#for filtering by selected date below, choose your date here
chosen_date <- "2020-09-27"



# ACTBLUE FILINGS ####

# *contribs*

#import zipped file 
actblue_contribs_all <- read_csv("raw_data/actblue_oct_monthly_sa1464847.csv.zip", col_types = cols(.default = "c"))

glimpse(actblue_contribs_all)

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
#save all records versions in case needed
saveRDS(actblue_contribs_all, "processed_data/actblue_contribs_all.rds")


# *expenditures*

#import zipped file 
actblue_expends_all <- read_csv("raw_data/actblue_oct_monthly_sb1464847.csv.zip", col_types = cols(.default = "c"))

#format columns and add name column
actblue_expends_all <- actblue_expends_all %>% 
  mutate(
    amount = as.numeric(amount),
    aggregate_amount = as.numeric(aggregate_amount),
    date = ymd(date),
    cmte_name = "ACTBLUE"
  ) %>% 
  select(cmte_name, everything())

glimpse(actblue_expends_all)

#filter for only records after our specified date
actblue_expends_selectdates <- actblue_expends_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(actblue_expends_selectdates, "processed_data/actblue_expends_selectdates.rds")
#save all records versions in case needed
saveRDS(actblue_expends_all, "processed_data/actblue_expends_all.rds")


# WINRED FILINGS ####

# *contribs*

#import zipped file 
winred_contribs_all <- read_csv("raw_data/winred_oct_quarterly_sa1448756.csv.zip", col_types = cols(.default = "c"))

#format columns and add name column
winred_contribs_all <- winred_contribs_all %>% 
  mutate(
    amount = as.numeric(amount),
    aggregate_amount = as.numeric(aggregate_amount),
    date = ymd(date),
    cmte_name = "WINRED"
  ) %>% 
  select(cmte_name, everything())

glimpse(winred_contribs_all)

#filter for only records after our specified date
winred_contribs_selectdates <- winred_contribs_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(winred_contribs_selectdates, "processed_data/winred_contribs_selectdates.rds")
#save all records versions in case needed
saveRDS(winred_contribs_all, "processed_data/winred_contribs_all.rds")


# *expenditures*

#import zipped file 
winred_expends_all <- read_csv("raw_data/winred_oct_quarterly_sb1448756.csv.zip", col_types = cols(.default = "c"))

#format columns and add name column
winred_expends_all <- winred_expends_all %>% 
  mutate(
    amount = as.numeric(amount),
    aggregate_amount = as.numeric(aggregate_amount),
    date = ymd(date),
    cmte_name = "WINRED"
  ) %>% 
  select(cmte_name, everything())

glimpse(winred_expends_all)

#filter for only records after our specified date
winred_expends_selectdates <- winred_expends_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(winred_expends_selectdates, "processed_data/winred_expends_selectdates.rds")
#save all records versions in case needed
saveRDS(winred_expends_all, "processed_data/winred_expends_all.rds")



# SENATE LEADERSHIP FUND FILINGS ####

# *contribs*

#import zipped file 
senateleadershipfund_contribs_all <- read_csv("raw_data/senateleadershipfund_oct_monthly_sa.csv", col_types = cols(.default = "c"))

#format columns and add name column
senateleadershipfund_contribs_all <- senateleadershipfund_contribs_all %>% 
  mutate(
    amount = as.numeric(amount),
    aggregate_amount = as.numeric(aggregate_amount),
    date = ymd(date),
    cmte_name = "SENATE LEADERSHIP FUND"
  ) %>% 
  select(cmte_name, everything())

glimpse(senateleadershipfund_contribs_all)

#filter for only records after our specified date
senateleadershipfund_contribs_selectdates <- senateleadershipfund_contribs_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(senateleadershipfund_contribs_selectdates, "processed_data/senateleadershipfund_contribs_selectdates.rds")


# *expenditures*

#import zipped file 
senateleadershipfund_expends_all <- read_csv("raw_data/senateleadershipfund_oct_monthly_sb.csv", col_types = cols(.default = "c"))

#format columns and add name column
senateleadershipfund_expends_all <- senateleadershipfund_expends_all %>% 
  mutate(
    amount = as.numeric(amount),
    date = ymd(date),
    cmte_name = "SENATE LEADERSHIP FUND"
  ) %>% 
  select(cmte_name, everything())

glimpse(senateleadershipfund_expends_all)

#filter for only records after our specified date
senateleadershipfund_expends_selectdates <- senateleadershipfund_expends_all %>% 
  filter(date > chosen_date)

#save for next steps
saveRDS(senateleadershipfund_expends_selectdates, "processed_data/senateleadershipfund_expends_selectdates.rds")

