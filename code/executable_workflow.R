
# This executable workflow is based off the vignette. Refer to that for full instructions.

# Prepare workspace 
# load the required packages, source the utility functions code file, and indicate the year you will be processing.

library(tidyverse)
library(lubridate)
library(here)
library(birdnames)

source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/rawhep_to_HEPDATA_utility_functions.R")

zyear = 2021


## Step 1, convert raw HEP data to more-usable format.----
# First Survey123
# This step uses functions from step1_wrangle_survey123.R, which can be piped together into a single process.

source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/step1_wrangle_survey123.R")

zversion = "102"

s123_file = paste("HEP", zyear, zversion, sep = "_")

 wrangled_s123 <- read.csv(here(paste("data/downloaded/", s123_file, ".csv", sep = ""))) %>% 
  filter(useforsummary == "y") %>% # 
  fix_s123_names() %>% 
  fix_s123_date_fields(zformat = "%m/%d/%Y %H:%M") %>% # if 2020, need to set zformat  = "%m/%d/%Y %H:%M" because date comes in different format. default works for 2021
  add_multiple_survey_num() %>% 
   mutate(complete.count = "unk") %>% 
  wrangle_s123()


names(wrangled_s123)

str(wrangled_s123$nests)

#Save wrangled_s123 to the appropriate folder.

wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))


# Alcatraz. 
# Wrangling Alcatraz data requires some manual data checks and the process cannot be fully automated into executable functions. To wrangle Alcatraz, follow the instructions and run the code in:
# https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step1_wrangle_alcatraz.R

# TODO create Bolinas code

#Finally combine the wrangled data from each raw data source.
source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/step1_combine_wrangled.R")
combine_wrangled_hep() %>% saveRDS(paste("data/wrangled/wrangled_raw", zyear, sep = "_"))

# 2020 data needs complete.count field added to dates, nests, stages fields
readRDS(here("data/wrangled/wrangled_raw_2020")) %>%
  add_complete_count_2020() %>% # also defined in step1_combine_wrangled.R
  saveRDS(here("data/wrangled/wrangled_raw_2020"))



### Step 1.1, Check wrangled raw data ----
wrangled_raw <- readRDS(paste("data/wrangled/wrangled_raw", zyear, sep = "_"))



#Check year matches zyear  

wrangled_raw$dates %>% 
  mutate(start.year.diff = year(start) - zyear,
         end.year.diff = year(end) - zyear) %>% 
  filter(start.year.diff != 0 | end.year.diff != 0) %>%
  view()

# If there are records in the resulting object, go back and fix in raw HEP  


# Check which species have nested in each colony. This can help the screener see if any unexpected species are nesting in a given colony, or if a species has disappeared from a colony.
# for 1, a few, or all colonies, check which species have nested there and peak active nests for each year.

check_nesting_history(2021, c(53), screened.s123 = FALSE) %>% # from survey123_utility_functions.R
  pivot_wider(id_cols = c(code, site.name, year), values_from = total.nests, names_from = species) %>% 
  view()


# Check for unexpected values in each data group. Each element (dataframe) of the list can be accessed with $.
summary(wrangled_s123$observers.effort)
summary(wrangled_s123$nests)
summary(wrangled_s123$stages)
summary(wrangled_s123$brood.sizes)
summary(wrangled_s123$predators)
summary(wrangled_s123$disturbance)
summary(wrangled_s123$notes)


# Check start and end date match; if nrow = 0, no problem.
wrangled_raw$dates %>% 
  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
  nrow()


# Check for multiple surveys at same site and same date. Such visits are fine and consistent with the field protocol, and the screening code should handle these multiple surveys fine, but nevertheless good to be aware of before proceeding with screening so you can check to make sure all the data made it through screening appropriately

wrangled_s123$dates %>% filter(num.surveys.this.date > 1) %>% view()


## Step 2, output wrangled Survey123 data into Season Summary sheets. ---- 

colony_spp_need_sheet <- get_colony_spp_need_sheet(zyear)

# The colony_spp_need_sheet object has the colony and species fields for purrr::map to iterate over to create each sheet.

# all colonies X species needing sheets
pmap(.l = list(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary)


# single species X colony
render_season_summary(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = 2020, zcode = 1.1, zspp = "GBHE")


# Or all species for a single colony:

pmap(.l = list(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = 2021, zcode = 16, zspp = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO")), .f = render_season_summary)

## Step 4, extract tables from screened Season Summary Sheets ----
source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/step4_extract_screened_season_summary.R")

 screened_seas_summ_files <- list.files(paste("season_summary_forms/", zyear, "/", sep = ""), )


screened_hep <- list(screen.log = map2_df(zyear, screened_seas_summ_files, get_screening_log),
                      observers.effort = map2_df(zyear, screened_seas_summ_files, get_observers_effort),
                      nests = map2_df(zyear, screened_seas_summ_files, get_total_nest_table),
                      stages = map2_df(zyear, screened_seas_summ_files, get_nest_stage_rop_table),
                      brood.sizes = map2_df(zyear, screened_seas_summ_files, get_stage4brd),
                      predators = map2_df(zyear, screened_seas_summ_files, get_predators) %>%  distinct(),
                      disturbance = map2_df(zyear, screened_seas_summ_files, get_disturbance) %>%  distinct(),
                      notes = map2_df(zyear, screened_seas_summ_files, get_notes) %>% distinct())  
names(screened_hep)

saveRDS(screened_hep, here(paste("data/screened/screened_hep_", zyear, sep = "")))



## Step 5 generate screening change log ----
source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/step5_make_screening_change_log.R")

track_changes_hep <- list(screen.log = readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))[["screen.log"]],
                           observers.effort = make_track_change_tables(zyear, "observers.effort"),
                           nests = make_track_change_date_tables(zyear, "nests") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           stages = make_track_change_date_tables(zyear, "stages") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           brood.sizes = make_track_change_date_tables(zyear, "brood.sizes") %>% 
                             mutate(date = as.Date(date)) %>% 
                             group_by(code, species, date) %>% # need to remove dates with no stage 4 nests (these only exist in wrangled_) for meaningful comparison of differences
                            mutate(max.num.nest = max(num.nests)) %>%
                            ungroup() %>%
                            filter(max.num.nest > 0) %>% 
                            arrange(code, species, date, brd),
                           predators = make_track_change_tables(zyear, "predators") %>% 
                             arrange(code, species, predator.species),
                           disturbance = make_track_change_date_tables(zyear, "disturbance") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           notes = make_track_change_date_tables(zyear, "notes") %>% 
                            filter(note.type != "for.HEPDATA") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date)
)

names(track_changes_hep)

str(track_changes_hep$nests)


track_changes_hep$nests %>% 
  filter(grepl("changed", changelog)) %>% 
  arrange(code, species, date) %>% 
  view()

track_changes_hep$brood.sizes %>% 
  filter(grepl("changed", changelog)) %>% 
  arrange(code, species, date) %>% 
  view()


saveRDS(track_changes_hep, here(paste("data/track_changes/track_changes_hep_", zyear, sep = ""))) 

  
  
## Step 6 convert to HEPDATA ----

source("https://raw.githubusercontent.com/scottfjennings/rawhep_to_HEPDATA/main/code/step6_screened_to_HEPDATA.R")

HEPDATA <- screened_to_HEPDATA(zyear)

str(HEPDATA)

saveRDS(HEPDATA, here(paste("data/as_HEPDATA/HEPDATA_", zyear, sep = "")))

write.csv(HEPDATA, here(paste("data/as_HEPDATA/HEPDATA_", zyear, ".csv", sep = "")), row.names = FALSE)


