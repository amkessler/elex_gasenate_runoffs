# package details here https://stephenholzman.github.io/tidyusafec/articles/intro.html
# to install for the first time:
# devtools::install_github("stephenholzman/tidyusafec")

library(tidyverse)
library(lubridate)
library(janitor)
library(tidyusafec)
options(scipen = 999)

# CMTES of INTEREST:
# C00571703 - SENATE LEADERSHIP FUND
# C00484642 - SMP (Senate Majority PAC)


#save api key for use here
save_datagov_apikey(key = Sys.getenv("FECAPIKEY"))


#get committee reports - starting with one single committee
testcmte <- get_committee_reports(committee_id = "C00571703", 
                                  cycle = 2020)


#get CONTRIBS - starting with one single committee
testcmte_contribs <- get_itemized_contributions(committee_id = "C00571703", 
                                                # min_amount = 2000,
                                                # is_individual = TRUE,
                                                min_date = "2020-11-03")


glimpse(testcmte_contribs)




