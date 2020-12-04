library(tidyverse)
library(lubridate)
library(janitor)


#for filtering by selected date below, choose your date here
chosen_date <- "2020-11-03"

#from raw fec reports
actblue_fecraw_contribs_all <- read_csv("raw_data/fecfiles/actblue_1481637-schedule-a.csv", col_types = cols(.default = "c"))

actblue_fecraw_contribs_all <- actblue_fecraw_contribs_all %>% 
  mutate(
    contribution_date = ymd(contribution_date),
    contribution_amount = as.numeric(contribution_amount),
    contribution_aggregate = as.numeric(contribution_aggregate),
    cmte_name = "ACTBLUE"
  ) %>% 
  select(cmte_name, everything())


glimpse(actblue_fecraw_contribs_all)

#filter for only records after our specified date
actblue_fecraw_contribs_postelex <- actblue_fecraw_contribs_all %>% 
  filter(contribution_date > chosen_date)


#save versions 
saveRDS(actblue_fecraw_contribs_all, "processed_data/actblue_fecraw_contribs_all.rds")
saveRDS(actblue_fecraw_contribs_postelex, "processed_data/actblue_fecraw_contribs_postelex.rds")

actblue_fecraw_contribs_postelex_GASEN %>% 
  head(1000) %>% 
  View()



#use ossoff and warnock variations to filter actblue
#and create new columns to standardize
actblue_fecraw_contribs_postelex_GASEN <- actblue_fecraw_contribs_postelex %>% 
  filter(
    str_detect(memo_text_description, "JON OSSOFF FOR SENATE") |
      str_detect(memo_text_description, "OSSOFF VICTORY FUND") |
      str_detect(memo_text_description, "WARNOCK FOR GEORGIA") |
      str_detect(memo_text_description, "WARNOCK VICTORY FUND")
  ) %>% 
  mutate(
    ga_candidate = case_when(
      str_detect(memo_text_description, "JON OSSOFF FOR SENATE") ~ "OSSOFF",
      str_detect(memo_text_description, "OSSOFF VICTORY FUND") ~ "OSSOFF",
      str_detect(memo_text_description, "WARNOCK FOR GEORGIA") ~ "WARNOCK",
      str_detect(memo_text_description, "WARNOCK VICTORY FUND") ~ "WARNOCK"
    ),
    ga_party = case_when(
      ga_candidate == "OSSOFF" ~ "D",
      ga_candidate == "WARNOCK" ~ "D"
    ),
    in_out_state = if_else(contributor_state == "GA", "IN", "OUT")
  ) %>% 
  select(ga_party, ga_candidate, everything())

#save for next steps
saveRDS(actblue_fecraw_contribs_postelex_GASEN, "processed_data/actblue_fecraw_contribs_postelex_GASEN.rds")
#save for sharing with others
write_csv(actblue_fecraw_contribs_postelex_GASEN, "processed_data/actblue_fecraw_contribs_postelex_GASEN.csv")





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




#### PROCESSING WORK ON ISOLATING GEORGIA CANDIDATES #####

#isolating and processing contribs to the georgia runoff candidates ####

#use ossoff and warnock variations to filter actblue
#and create new columns to standardize
actblue_contribs_ga_sencands <- actblue_contribs_selectdates %>% 
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
    ),
    in_out_state = if_else(state == "GA", "IN", "OUT")
  ) %>% 
  select(ga_party, ga_candidate, everything())

#save for next steps
saveRDS(actblue_contribs_ga_sencands, "output/actblue_contribs_ga_sencands.rds")
#save for sharing with others
write_csv(actblue_contribs_ga_sencands, "output/actblue_contribs_ga_sencands.csv")



#do the same for perdue loeffler variations from winred
winred_contribs_ga_sencands <- winred_contribs_selectdates %>% 
  filter(
    str_detect(memo_text, "PERDUE FOR SENATE") |
      str_detect(memo_text, "PERDUE VICTORY INC") |
      str_detect(memo_text, "FRIENDS OF DAVID PERDUE") |
      str_detect(memo_text, "GEORGIANS FOR KELLY LOEFFLER")
  ) %>% 
  mutate(
    ga_candidate = case_when(
      str_detect(memo_text, "PERDUE FOR SENATE") ~ "PERDUE",
      str_detect(memo_text, "PERDUE VICTORY INC") ~ "PERDUE",
      str_detect(memo_text, "FRIENDS OF DAVID PERDUE") ~ "PERDUE",
      str_detect(memo_text, "GEORGIANS FOR KELLY LOEFFLER") ~ "LOEFFLER"
    ),
    ga_party = case_when(
      ga_candidate == "PERDUE" ~ "R",
      ga_candidate == "LOEFFLER" ~ "R"
    ),
    in_out_state = if_else(state == "GA", "IN", "OUT")
  ) %>% 
  select(ga_party, ga_candidate, everything())


#save for next steps
saveRDS(winred_contribs_ga_sencands, "output/winred_contribs_ga_sencands.rds")
#save for sharing with others
write_csv(winred_contribs_ga_sencands, "output/winred_contribs_ga_sencands.csv")






# SENATE LEADERSHIP FUND FILINGS ####

# *contribs*

#import zipped file
senateleadershipfund_contribs_all <- read_csv("raw_data/itemizerfiles/senateleadershipfund_sa.csv", col_types = cols(.default = "c"))

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
senateleadershipfund_expends_all <- read_csv("raw_data/itemizerfiles/senateleadershipfund_sb.csv", col_types = cols(.default = "c"))

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



# 
# # SMP (SEN. MAJORITY PAC) FILINGS ####
# 
# # *contribs*
# 
# #import zipped file
# smp_contribs_all <- read_csv("raw_data/itemizerfiles/smp_sa.csv", col_types = cols(.default = "c"))
# 
# #format columns and add name column
# smp_contribs_all <- smp_contribs_all %>%
#   mutate(
#     amount = as.numeric(amount),
#     aggregate_amount = as.numeric(aggregate_amount),
#     date = ymd(date),
#     cmte_name = "SMP"
#   ) %>%
#   select(cmte_name, everything())
# 
# glimpse(smp_contribs_all)
# 
# #filter for only records after our specified date
# smp_contribs_selectdates <- smp_contribs_all %>%
#   filter(date > chosen_date)
# 
# #save for next steps
# saveRDS(smp_contribs_selectdates, "processed_data/smp_contribs_selectdates.rds")
# 
# 
# # *expenditures*
# 
# #import zipped file
# smp_expends_all <- read_csv("raw_data/itemizerfiles/smp_sb.csv", col_types = cols(.default = "c"))
# 
# #format columns and add name column
# smp_expends_all <- smp_expends_all %>%
#   mutate(
#     amount = as.numeric(amount),
#     date = ymd(date),
#     cmte_name = "SMP"
#   ) %>%
#   select(cmte_name, everything())
# 
# glimpse(smp_expends_all)
# 
# #filter for only records after our specified date
# smp_expends_selectdates <- smp_expends_all %>%
#   filter(date > chosen_date)
# 
# #save for next steps
# saveRDS(smp_expends_selectdates, "processed_data/smp_expends_selectdates.rds")
# 
# smp_contribs_selectdates 
