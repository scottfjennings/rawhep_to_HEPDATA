

# entire workflow for processing Survey123 data
library(tidyverse)
library(here)

source(here("code/survey123_utility_functions.r"))

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")

zyear = 2020
#
# step 1, convert Survey123 data to more-usable format ----
# this step uses functions from survey123_wrangle.R
source(here("code/step1_wrangle_survey123.r"))

# add table with "expected" number of species (query HEPDATA)
# change "code" to "site.code"
wrangled_s123 <- read_s123(zyear, add.test.data = TRUE) %>% 
  filter(useforsummary == "y") %>% # 
  fix_s123_names() %>% 
  fix_s123_date_fields() %>% 
  add_multiple_survey_num() %>% 
  wrangle_s123()
  


wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
 
# step 1.1, check for any data issues, and data summary helpers to prepare for manual screening ----

# the functions called in this section load the appropriate wrangled_s123,
# other checks require loading wrangled_s123 directly
wrangled_s123 <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
#
# 1.1.1 which species have nested in each colony? ---- 
# for 1, a few, or all colonies, check which species have nested there and peak active nests for each year.
check_nesting_history(2020, c(53), screened.s123 = FALSE) %>% # from survey123_utility_functions.R
  pivot_wider(id_cols = c(code, site.name, year), values_from = total.nests, names_from = species) %>% 
  view()

#
 
# 1.1.2 check start and end date match ----
# if nrow = 0, no problem
wrangled_s123$dates %>% 
  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
  nrow()
  
# 1.1.3 check for any surveys entered as happening at night, not yet developed, needed? ----

# 1.1.4 check for multiple surveys at same site and same date ----
# such visits are fine and consistent with the field protocol, and the screening code should handle these multiple surveys fine, but nevertheless good to be aware of before proceeding with screening so you can check to make sure all the data made it through screening appropriately

wrangled_s123$dates %>% filter(num.surveys.this.date > 1) %>% view()

# add check for correct data types, values within expected ranges

# 1.1.5 check observer X date X colony   ----
check_expected_observers(zyear) %>% # from survey123_utility_functions.R
  filter(only.unexpected.observer == TRUE) %>% 
  view()


# check for time span between surveys, not yet developed, needed? ----
 
# step 2, output wrangled Survey123 data into Season Summary sheets. ----
# create a sheet for each instance where a species nested in a colony

# list of all species in each colony this year. use this list to make season summary sheets only for species that actually nested at each colony
colony_spp <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))$nests %>% 
  filter(total.nests > 0) %>% 
  distinct(code, species) %>% 
  mutate(year = zyear) %>% 
  arrange(code, species)

# remove colony X species that already have sheet made
if(file.exists(here(paste("season_summary_forms/sheet_creation_logs/sheet_creation_log_", zyear, sep = "")))) {
  colony_spp_need_sheet <- colony_spp %>% 
  anti_join(readRDS(here(paste("season_summary_forms/sheet_creation_logs/sheet_creation_log_", zyear, sep = ""))))
} else {
  colony_spp_need_sheet <- colony_spp
}

# create season summary sheet for single colony X species
render_season_summary(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = 2020, zcode = 599, zspp = "GREG")

# create season summary sheet for all colony X species that don't yet have a sheet
# if testing, can further subset colony_spp_need_sheet, here just doing the test data
# colony_spp_need_sheet <- colony_spp_need_sheet %>% filter(code == 599)

system.time(pmap(.l = list(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary))
# creating the 6 test data sheets takes ~30 seconds


# step 3, NO CODE - MANUALLY SCREEN SEASON SUMMARY SHEETS. ---- 
  # when a sheet has been screened the first time, change its name from [code]_[year]_[species].docx to [code]_[year]_[species]_screened1.docx
  # if a sheet is screened a second time, change its name from [code]_[year]_[species]_screened1.docx to [code]_[year]_[species]_screened2.docx
  # files must be .docx. .doc will not work without downloading LibreOffice software
# step 3.1, check season summary sheets needing to be screened ---- 
  # this mostly won't be needed, since it will be easy to visually id unscreened sheets in the folder. if the above renaming is NOT done, then this code will not perform as designed/expected
  grep('screened', list.files(here(paste("season_summary_forms/", zyear, "/", sep = ""))), invert=TRUE, value=TRUE)
  
  
   
# step 4, extract tables from screened Season Summary Sheets ----
  # uses functions in extract_screened_season_summary.R
source(here("code/step4_extract_screened_season_summary.r"))

  screened_seas_summ_files <- list.files(paste("season_summary_forms/", zyear, "/", sep = ""), pattern = "screened")
# these functions (defined in extract_screened_season_summary.R) loop through each .docx file and extract the pertinent table, then assemble those into a single data frame. Bundling those data frames into a list brings the data back to the same structure and names as wrangled_s123, so that a log of screening changes can be easily made (step 5)
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
# uses functions defined in step5_make_screening_change_log.R
source(here("code/step5_make_screening_change_log.R"))

screened_sheets <- get_screened_sheets(zyear)

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


saveRDS(track_changes_s123, here(paste("data/track_changes/track_changes_s123_", zyear, sep = ""))) 
  
  
  
  
# step 6 convert to HEPDATA ----
source(here("code/step6_screened_to_HEPDATA.R"))


HEPDATA <- screened_to_HEPDATA(zyear)
