

# recommended script for the entire workflow for processing Survey123 data

# setup: run this chunk for every step ---- 
library(tidyverse)
library(lubridate)
library(here)
library(devtools)

source(here("code/survey123_utility_functions.r"))

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")

zyear = 2020
#
# step 1, convert Survey123 data to more-usable format ----
source(here("code/step1_wrangle_survey123.r"))

wrangled_s123 <- read_s123(zyear, add.test.data = TRUE) %>% 
  filter(useforsummary == "y") %>% 
  fix_s123_names() %>% 
  fix_s123_date_fields() %>% 
  add_multiple_survey_num() %>% 
  wrangle_s123()
  
wrangled_s123 %>% saveRDS(here(paste("data/wrangled/wrangled_s123", zyear, sep = "_")))
 
# step 1.1, check for any data issues, and data summary helpers to prepare for manual screening ----
wrangled_s123 <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
#
# which species have nested in each colony? ---- 
check_nesting_history(2020, c(53), screened.s123 = FALSE) %>% # from survey123_utility_functions.R
  pivot_wider(id_cols = c(code, site.name, year), values_from = total.nests, names_from = species) %>% 
  view()

# check start and end date match ----
# if nrow = 0, no problem
wrangled_s123$dates %>% 
  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
  nrow()
  

# check for multiple surveys at same site and same date ----
wrangled_s123$dates %>% filter(num.surveys.this.date > 1) %>% view()

# check observer X date X colony   ----
check_expected_observers(zyear) %>% # from survey123_utility_functions.R
  filter(only.unexpected.observer == TRUE) %>% 
  view()



# step 2, output wrangled Survey123 data into Season Summary sheets. ----

# list of all species in each colony this year. 
colony_spp_need_sheet <- get_colony_spp_need_sheet(zyear)

# create season summary sheet for single colony X species
render_season_summary(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = 2020, zcode = 599, zspp = "GREG")

# create season summary sheet for all colony X species that don't yet have a sheet
# if testing, can further subset colony_spp_need_sheet, here just doing the test data
# colony_spp_need_sheet <- colony_spp_need_sheet %>% filter(code == 599)

system.time(pmap(.l = list(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary))
# creating the 6 test data sheets takes ~30 seconds


# step 3, NO CODE - STOP AND MANUALLY SCREEN SEASON SUMMARY SHEETS. ---- 

# step 4, extract tables from screened Season Summary Sheets ----
source_url("https://github.com/scottfjennings/Survey123_to_HEPDATA/blob/main/code/step4_extract_screened_season_summary.R")


# note, running the code below will overwrite any previous version of screened_s123, it will not append new records to the previous version. This generally shouldn't be a problem (I can't think of a realistic scenario), but it is something to be aware of.  

screened_s123 <- list(screen.log = map2_df(zyear, screened_seas_summ_files, get_screening_log),
                      observers.effort = map2_df(zyear, screened_seas_summ_files, get_observers_effort),
                      nests = map2_df(zyear, screened_seas_summ_files, get_total_nest_table),
                      stages = map2_df(zyear, screened_seas_summ_files, get_nest_stage_rop_table),
                      brood.sizes = map2_df(zyear, screened_seas_summ_files, get_stage4brd),
                      predators = map2_df(zyear, screened_seas_summ_files, get_predators) %>%  distinct(),
                      disturbance = map2_df(zyear, screened_seas_summ_files, get_disturbance) %>%  distinct(),
                      notes = map2_df(zyear, screened_seas_summ_files, get_notes) %>% distinct())  

  
saveRDS(screened_s123, here(paste("data/screened/screened_s123_", zyear, sep = "")))
 
# step 5 generate screening change log ----
source(here("code/step5_make_screening_change_log.R"))


track_changes_s123 <- list(screen.log = readRDS(here(paste("data/screened/screened_s123_", zyear, sep = "")))[["screen.log"]],
                           observers.effort = make_track_change_tables(zyear, "observers.effort"),
                           nests = make_track_change_tables(zyear, "nests") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           stages = make_track_change_tables(zyear, "stages") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           brood.sizes = make_track_change_tables(zyear, "brood.sizes") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           predators = make_track_change_tables(zyear, "predators") %>% 
                             arrange(code, species, predator.species),
                           disturbance = make_track_change_tables(zyear, "disturbance") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           notes = make_track_change_tables(zyear, "notes") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date)
)


track_changes_s123$nests %>% 
  filter(grepl("changed", changelog)) %>% view()



saveRDS(track_changes_s123, here(paste("data/track_changes/track_changes_s123_", zyear, sep = ""))) 
  
  
  
  
# step 6 convert to HEPDATA ----
source(here("code/step6_screened_to_HEPDATA.R"))


HEPDATA <- screened_to_HEPDATA(zyear)
