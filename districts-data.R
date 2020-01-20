# This script reads in the data
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, reshape2, tidylog, splines, pbapply, here, brms)
d <- read_csv(here::here("data","districts.csv"),
              col_types = list("state_id"=col_character(),"state_name"=col_character(),"district_id"=col_character(),
                               "district_name"=col_character(),"new_district"=col_character(),"new_state"=col_character(),
                               "household"=col_character(),"cluster"=col_character())) %>% 
  select(-c('state_id', 'state_name', 'district_id', 'district_name')) %>% 
  rename(district.old=district, state.old=state) %>% 
  filter(!is.na(caseid)) %>%  # FILTER: removed 23 observations missing "caseid" (and all outcomes)
  mutate(district=ifelse(!is.na(new_district),tolower(new_district),tolower(district.old)),
         new=ifelse(!is.na(new_district),1,0),
         state=ifelse(!is.na(new_state),tolower(new_state),tolower(state.old)),
         state=ifelse(state=="arunanchal pradesh" | state=="arunanchal","arunachal pradesh",state),
         state=ifelse(state=="nct of delhi", "delhi",state))
save(d, file=here("data","districts.Rda"))
write_csv(d, path = here("data","districts-121819.csv"))
