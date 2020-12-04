# package details here https://stephenholzman.github.io/tidyusafec/articles/intro.html
# to install for the first time:
# devtools::install_github("stephenholzman/tidyusafec")

library(tidyverse)
library(lubridate)
library(janitor)
library(tidyusafec)
options(scipen = 999)

#save api key for use here
save_datagov_apikey(key = Sys.getenv("FECAPIKEY"))


#get committee reports - starting with one single committee
testcmte <- get_committee_reports(committee_id = "C00492454", 
                                  cycle = 2020)


#get CONTRIBS - starting with one single committee
testcmte_contribs <- get_itemized_contributions(committee_id = "C00492454", 
                                                min_amount = 2000,
                                                is_individual = TRUE,
                                                min_date = "2020-06-30")


glimpse(testcmte_contribs)




