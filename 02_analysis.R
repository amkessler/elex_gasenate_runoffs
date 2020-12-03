library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)
library(widgetframe)
library(scales)
library(zoo)
options(dplyr.summarise.inform = FALSE)


#load data files created in step 01
actblue_contribs_selectdates <- readRDS("processed_data/actblue_contribs_selectdates.rds")
# actblue_expends <- readRDS("processed_data/actblue_expends_selectdates.rds")
winred_contribs_selectdates <- readRDS("processed_data/winred_contribs_selectdates.rds")
# winred_expends <- readRDS("processed_data/winred_expends_selectdates.rds")


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



### IN-STATE VS. OUT-OUT-STATE ####

#dem overall totals
dem_inout <- actblue_contribs_ga_sencands %>% 
  group_by(in_out_state) %>% 
  summarise(total_dollars = sum(amount)) %>% 
  ungroup()

dem_inout

#dem candidate totals
dem_inout_bycand <- actblue_contribs_ga_sencands %>% 
  group_by(ga_candidate, in_out_state) %>% 
  summarise(total_dollars = sum(amount)) %>% 
  ungroup()

dem_inout_bycand


## charts

#reorder factor to allow for descending bars
dem10 <- dem10 %>%
  mutate(zipname = fct_reorder(zipname, demtotal)) 

#chart it out
d <- ggplot(dem10, aes(zipname, demtotal)) + geom_col(fill = "darkblue") + coord_flip() +
  theme_minimal()

d

#add extra elements to the chart and convert to ggplotly
d2 <- d + labs(title="Top DNC/DCCC zip codes",
               # subtitle = "A subtitle",
               caption = "Source: FEC",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=dollar) 

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(dd), 'demtopzip_plt.html')
htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'demtopzip_plt_nm.html')

#save as RDS object
saveRDS(dd_nomenu, "demtopzip_plt_nm.rds")







