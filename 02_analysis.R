library(tidyverse)
library(lubridate)
library(janitor)

#load data files created in step 01
actblue_contribs <- readRDS("processed_data/actblue_contribs_selectdates.rds")
# actblue_expends <- readRDS("processed_data/actblue_expends_selectdates.rds")
winred_contribs <- readRDS("processed_data/winred_contribs_selectdates.rds")
# winred_expends <- readRDS("processed_data/winred_expends_selectdates.rds")


#pull out just contribs to the georgia runoff candidates

#use ossoff and warnock variations to filter actblue
#and create new columns to standardize
actblue_contribs_ga_sencands <- actblue_contribs %>% 
  filter(
    str_detect(memo_text, "JON OSSOFF FOR SENATE") |
    str_detect(memo_text, "OSSOFF VICTORY FUND") |
    str_detect(memo_text, "WARNOCK FOR GEORGIA") |
    str_detect(memo_text, "WARNOCK VICTORY FUND")
  ) %>% 
  mutate(
    ga_candidate = case_when(
      str_detect(memo_text, "JON OSSOFF FOR SENATE") ~ "OSSOFF",
      str_detect(memo_text, "OSSOFF VICTORY FUND") ~ "OSSOFF",
      str_detect(memo_text, "WARNOCK FOR GEORGIA") ~ "WARNOCK",
      str_detect(memo_text, "WARNOCK VICTORY FUND") ~ "WARNOCK"
    ),
    ga_party = case_when(
      ga_candidate == "OSSOFF" ~ "D",
      ga_candidate == "WARNOCK" ~ "D"
    )
  ) %>% 
  select(ga_party, ga_candidate, everything())


#use perdue loeffler variations to filter winred
winred_contribs_ga_sencands <- winred_contribs %>% 
  filter(
    str_detect(memo_text, "PERDUE FOR SENATE") |
      str_detect(memo_text, "PERDUE VICTORY INC") |
      str_detect(memo_text, "FRIENDS OF DAVID PERDUE") |
      str_detect(memo_text, "GEORGIANS FOR KELLY LOEFFLER")
  )



glimpse(winred_contribs_all)

winred_contribs_all %>% 
  count(memo_text) %>% 
  filter(str_detect(memo_text, "LOEFFLER"))
