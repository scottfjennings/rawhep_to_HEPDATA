



# This document describes the workflow for processing data from Survey123 to HEPDATA format. It begins assuming you have downloaded a .csv of the current year's Survey123 data and saved it as data/downloaded/HEP_[year]_[version number].csv, where the version number represents new versions created through proofing directly in Survey123.

# Processing the data from Survey123 to HEPDATA is comprised of 6 steps:  

# 1. Wrangle raw HEP data (Survey123, Alcatraz, Bolinas) to usable format   
# 2. Output Season Summary Sheet    
# 3. Manually screen each Season Summary Sheet
# 4. Extract screened data from Season Summary Sheets
# 5. Generate log of screening changes
# 6. Convert screened data to HEPDATA format

# Each of these steps involves multiple sub-steps, and each major step has its own code file where the functions and processes for each sub-step are defined. Users should not need to access or open the major step code files. Rather, the functions defined in these files should be called in a separate script. The file Survey123_to_HEPDATA_workflow.R provides the recommended script, and this vignette has instructions and additional information to proceed through the entire workflow. 

# This script relies heavily on the pipe operator and on the purrr::map family of functions. The map functions allow iterating the same process over many instances. In this script you will iterate over colony X species combinations in multiple steps. 

## Prepare workspace 
# load the required packages, source the utility functions code file, and indicate the year you will be processing.

library(tidyverse)
library(lubridate)
library(here)
library(birdnames)
library(xlsx)



source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/rawhep_to_HEPDATA_utility_functions.R")

zyear = 2021



# Step 1, Wrangle ---- 
# convert Survey123, HEP_site_visits and USGS Alcatraz data into the same (more-usable) format
### First Survey123 ----
# This step uses functions from step1_wrangle_survey123.R, which can be piped together into a single process.

source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step1_wrangle_survey123.R")
# source(here("code/step1_wrangle_survey123.R"))

# set downloaded survey123 version
zversion = "0"
 
# This step fixes field names and date fields, adds a helper column to indicate where multiple surveys happened on the same date, then finally splits the data into different "types" (e.g. total nest numbers, predator observations, etc.). These groups of "like" data are referred to as data groups. Each data group requires different procedures and manipulations downstream in the workflow.

 wrangled_s123 <- read.csv(here(paste("data/downloaded/HEP_", zyear, "_", zversion, ".csv", sep = ""))) %>%
#   mutate(complete.count = NA) %>% # only do this if wrangling 2020 data!!!
  mutate(useforsummary = tolower(useforsummary)) %>% 
  filter(useforsummary == "y") %>% # 
  fix_s123_names() %>% 
  fix_s123_date_fields() %>% 
  add_multiple_survey_num() %>% 
  wrangle_s123()



# The result of these functions is a list, with the elements of the list being dataframes for each data group.

names(wrangled_s123)



str(wrangled_s123$nests)


# Save wrangled_s123 to the appropriate folder.

wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))




### Next, Alcatraz. ----
# Wrangling Alcatraz data requires some manual data checks and the process cannot be fully automated into a single pipeline of executable functions. 
# Alcatraz step 1. read source, data 
source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step1_wrangle_alcatraz.R")
# source(here("code/step1_wrangle_alcatraz.R"))

all_checks_usgs <- read.xlsx(here("data/alcatraz/70_Alcatraz_2021databackupSNG,BCNH.xlsx"), sheetIndex = "Visits", startRow = 3)

# Alcatraz step 2. basic initial cleaning and reshaping 
long_alcatraz <- pivot_alcatraz(all_checks_usgs)

# Alcatraz step 3. some basic data checking to make sure pivot worked and check for any problems 
summary(long_alcatraz)

# any unexpected species?
count(long_alcatraz, species)

# notes?
distinct(long_alcatraz, notes) %>% view()


# fix any problems 
# Remove from 2021 a "passerine" nest that was monitored for fun.
long_alcatraz <- long_alcatraz %>% 
  filter(species != "PASS")

# Alcatraz step 4. extract info from notes 
all_checks_extracted <- notes_extracter(long_alcatraz)

# and save notes to be reviewed during screening
export_alcatraz_notes(all_checks_extracted)

## Alcatraz step 5. IMPORTANT MANUAL STEP HERE: examine the notes_extracter output to make sure no valuable/valid records have been classified with keeper == "N". 

#Probably just need to look at records that have notes
all_checks_extracted %>% 
    filter(keeper == "N", !is.na(notes)) %>% 
  view()


# can do a manual edit if there is still good data in alc_keeperN_with_notes, changing keeper to Y and changing egg, chick, etc as appropriate
all_checks_extracted <- edit(all_checks_extracted)

  # a helper to check what the most common notes are and compare to the notes that are specified in notes_extracter() to make sure we're dealing with the important ones
check_notes_frequency(all_checks_extracted)


# and now filter to have just the keepers
# and remove renests
# !! each year double check this is how renests were indicated  all_checks_extracted <- all_checks_extracted %>% 
    filter(keeper == "Y") %>% 
    select(-keeper) %>%
    filter(!grepl("A", nest), !grepl("B", nest))


# Alcatrax step 6. assign nest stage 
alcatraz_checks_stages <- alcatraz_assign_stage(all_checks_extracted)
 
  
# Alcatraz step 7. and the final reshape to wrangled format 
wrangled_alcatraz <- wrangle_alcatraz(alcatraz_checks_stages)

wrangled_alcatraz %>% 
  saveRDS(here("data/wrangled/wrangled_alcatraz_2021"))


### And HEP_site_visits ----
# 
source(here("code/step1_wrangle_HEP_site_visits.R"))

hep_site_visits <- hep_site_visits_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_screening_focal/HEP_site_visit_data.accdb")

col_codes = c(53)

wrangled_site_visits <- wrangle_HEP_site_visits(hep_site_visits, col_codes = col_codes)

saveRDS(wrangled_site_visits, paste("data/wrangled/wrangled_site_visits", zyear, sep = "_"))

### Finally combine the wrangled data from each raw data source.----

source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step1_combine_wrangled.R")
source(here("code/step1_combine_wrangled.R"))

combine_wrangled_hep = combine_wrangled_hep(wrangled_s123 = readRDS(here(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))),
                                            wrangled_alcatraz = readRDS(here(paste("data/wrangled/wrangled_alcatraz", zyear, sep = "_"))),
                                            wrangled_site_visits = readRDS(here(paste("data/wrangled/wrangled_site_visits", zyear, sep = "_"))))

combine_wrangled_hep %>% saveRDS(paste("data/wrangled/wrangled_raw", zyear, sep = "_"))

# combine_wrangled_hep <- readRDS(paste("data/wrangled/wrangled_raw", zyear, sep = "_"))

 

## Step 1.1, Check incoming data ----
# Next are some functions and code to check for any data issues and prepare for manual screening. These checks are largely meant to identify "proofing" problems that need to be fixed directly in Survey123.

# Reload wrangled_s123

wrangled_s123 <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))

wrangled_raw <- readRDS(paste("data/wrangled/wrangled_raw", zyear, sep = "_"))


# Check year matches zyear  

wrangled_s123$dates %>% 
  mutate(start.year.diff = year(start) - zyear,
         end.year.diff = year(end) - zyear) %>% 
  filter(start.year.diff != 0 | end.year.diff != 0) %>%
  view()

# If there are records in the resulting object, go back and fix in Survey123  


# Check which species have nested in each colony. This can help the screener see if any unexpected species are nesting in a given colony, or if a species has disappeared from a colony.
# for 1, a few, or all colonies, check which species have nested there and peak active nests for each year.

check_nesting_history(2021, c(53), screened.s123 = FALSE) %>% # from survey123_utility_functions.R
  pivot_wider(id_cols = c(code, site.name, year), values_from = total.nests, names_from = species) %>% 
  view()


# Check for unexpected values in each data group. Each element (dataframe) of the list can be accessed with $.

# output of these calls not included here
summary(wrangled_s123$observers.effort)
summary(wrangled_s123$nests)
summary(wrangled_s123$stages)
summary(wrangled_s123$brood.sizes)
summary(wrangled_s123$predators)
summary(wrangled_s123$disturbance)
summary(wrangled_s123$notes)


# Check start and end date match; if nrow = 0, no problem.

wrangled_s123$dates %>% 
  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
  nrow()
  


# Check for multiple surveys at same site and same date. Such visits are fine and consistent with the field protocol, and the screening code should handle these multiple surveys fine, but nevertheless good to be aware of before proceeding with screening so you can check to make sure all the data made it through screening appropriately

wrangled_s123$dates %>% filter(num.surveys.this.date > 1) %>% view()



# Check observer X date X colony THIS CURRENTLY NOT WORKING

check_expected_observers(zyear) %>% # from survey123_utility_functions.R
  filter(only.unexpected.observer == TRUE) %>% 
  view()


## Step 1.2. At this point the summaries for observers can be generated ----

# Generate a list of colonies that had a volunteer observer and at least 1 species nesting.
# need to resplit the list of observers
colony_vol_obs <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))$observers.effort %>% 
  select(code, colony, observers) %>% 
  mutate(observers = gsub("\\*", "", observers))

out_lengths <- str_split(colony_vol_obs$observers,"\\;") %>% 
  map(length) %>% 
  unlist()

max_out <- out_lengths[out_lengths == max(out_lengths)]

# then finally the list of colonies that had at least 1 volunteer observer
obs_list <- colony_vol_obs  %>%
  separate(observers, sep = ";", into = paste("obs", seq(1:max_out))) %>% 
  pivot_longer(cols = contains("obs"), values_to = "observer.name") %>% 
  mutate(across(c(observer.name, colony), ~trimws(.)),
         colony = gsub(" ", "", colony),
         colony = gsub("#", "", colony),
         colony = gsub("'", "", colony),
         colony = gsub(",", "", colony),
         colony = gsub("\\.", "", colony)) %>% 
  select(-name) %>% 
  filter(!is.na(observer.name), !observer.name %in% c("David Lumpkin", "Emiko Condeso", "Barbara Wechsberg", "Nils Warnock", "Scott Jennings", "Jim Jensen")) %>% 
  distinct(code, colony) %>% 
  mutate(year = zyear) 




pmap(.l = list(file = here("code/summary_for_observer.Rmd"), zyear = 2021, zcode = 83.0, zcol.name = "BlakesLandingSouth"), .f = render_summary_for_observer)

pmap(.l = list(file = here("code/summary_for_observer.Rmd"), zyear = obs_list$year[16], zcode = obs_list$code[16], zcol.name = obs_list$colony[16]), .f = render_summary_for_observer)

pmap(.l = list(file = here("code/summary_for_observer.Rmd"), zyear = obs_list$year[9:51], zcode = obs_list$code[9:51], zcol.name = obs_list$colony[9:51]), .f = render_summary_for_observer)

pmap(.l = list(file = here("code/summary_for_observer.Rmd"), zyear = obs_list$year, zcode = obs_list$code, zcol.name = obs_list$colony), .f = render_summary_for_observer)


 
## Step 2, output wrangled Survey123 data into Season Summary sheets. ----

# We want to create a sheet for each instance where a species nested in a colony. 

# The function get_colony_spp_need_sheet() queries wrangled_s123 to get a list of all species in each colony this year, and queries the year-appropriate season_summary_forms folder to create a list of colony X species that still need a Season Summary Sheet created. Rendering Season Summary Sheets to .docx files is the most time-consuming process of this workflow, so it is beneficial to only render each sheet once.


colony_spp_need_sheet <- get_colony_spp_need_sheet(zyear, include.inactive = FALSE) 

  # also add colony X species combos that were active within the last 3 years
  recent_nesters <- readRDS(here("data/hep_annual_nest_abundance")) %>%
              filter(year > zyear - 3, peakactvnsts > 0) %>% 
              distinct(code, species) %>% 
    right_join(., distinct(colony_spp_need_sheet, code))
  
colony_spp_need_sheet <- full_join(colony_spp_need_sheet, recent_nesters) %>% 
  filter(!is.na(species)) %>% 
  mutate(year = zyear)

# The colony_spp_need_sheet object has the colony and species fields for purrr::map to iterate over to create each sheet.
safely(pmap(.l = list(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary))

# The colony_spp_need_sheet object has the colony and species fields for purrr::map to iterate over to create each sheet.
pmap(.l = list(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year[105:108], zcode = colony_spp_need_sheet$code[105:108], zspp = colony_spp_need_sheet$species[105:108]), .f = render_season_summary)

# Note: you can generate a Season Summary Sheet for a single species X colony instance by specifying species and colony in the call to render_season_summary:
render_season_summary(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = 2021, zcode = 70, zspp = "BCNH")


# Or all species for a single colony:
pmap(.l = list(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear = 2021, zcode = 53, zspp = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO")), .f = render_season_summary)




## Step 3, NO CODE - MANUALLY SCREEN SEASON SUMMARY SHEETS. ----

# UPdate 2021-12-02: For now we are not renaming files.

# * When a sheet has been screened the first time, change its name from [code]_[year]_[species].docx to [code]_[year]_[species]_screened1.docx
# * If a sheet is screened a second time, change its name from [code]_[year]_[species]_screened1.docx to [code]_[year]_[species]_screened2.docx
  
# This renaming helps prevent accidental overwriting of already screened summary sheets (e.g. when rendering a single sheet), and also helps identify which files in the year-appropriate season_summary_forms folder still need to be screened.
  
   
## Step 4, extract tables from screened Season Summary Sheets ----
# This step uses functions in extract_screened_season_summary.R

source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step4_extract_screened_season_summary.R")

  
# Create a list of Season Summary Sheets that have been screened (requires file renaming in step 3).

  screened_seas_summ_files <- list.files(paste("season_summary_forms/", zyear, "/", sep = ""))
# if you have one of the season summary sheets in this directory open, you will not get the right file name for that sheet and creating screened_hep below will fail


# There are separate functions in step4_extract_screened_season_summary.r for each data group. These functions loop through each .docx file and extract the pertinent table, then assemble those into a single data frame. Bundling those data frames into a list brings the data back to the same structure and names as wrangled_s123, so that a log of screening changes can be easily made (step 5)


  # Also note that messages about "the condition having length > 1" are OK and can be ignored


screened_hep <- list(screen.log = map2_df(zyear, screened_seas_summ_files, get_screening_log),
                      observers.effort = map2_df(zyear, screened_seas_summ_files, get_observers_effort),
                      nests = map2_df(zyear, screened_seas_summ_files, get_total_nest_table),
                      stages = map2_df(zyear, screened_seas_summ_files, get_nest_stage_rop_table),
                      brood.sizes = map2_df(zyear, screened_seas_summ_files, get_stage4brd),
                      predators = map2_df(zyear, screened_seas_summ_files, get_predators) %>%  distinct(),
                      disturbance = map2_df(zyear, screened_seas_summ_files, get_disturbance) %>%  distinct(),
                      notes = map2_df(zyear, screened_seas_summ_files, get_notes) %>% distinct())  


# Check the result. You will see that screened_hep does not contain a dates table, but it does contain a screening.log table. The latter is the top table on the Season Summary Sheet.

names(screened_hep)


# Save screened_hep to disk
  # Note, running the code below will overwrite any previous version of screened_s123, it will not append new records to the previous version. This generally shouldn't be a problem (I can't think of a realistic scenario), but it is something to be aware of.  

saveRDS(screened_hep, here(paste("data/screened/screened_hep_", zyear, sep = "")))

# screened_hep <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))


## Step 5 generate screening change log. ----
# The logic for this step is based on merging pre-screened and screened data. Where this merge results in 2 records instead of 1, this indicates the record was changed during screening.

# This step uses functions defined in step5_make_screening_change_log.R

source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step5_make_screening_change_log.R")


# As with step 4, a function is called for each data group, and the results are combined into a list. Here the function is a simple join that could be generalized for all data groups, so the same function is called for all data groups. 

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
                             arrange(code, species, date),
                           predators = make_track_change_tables(zyear, "predators") %>% 
                             arrange(code, species, predator.species),
                           disturbance = make_track_change_date_tables(zyear, "disturbance") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           notes = make_track_change_date_tables(zyear, "notes") %>% 
                             mutate(date = ifelse(date == "", NA, date),
                                    date = as.Date(date)) %>% 
                             arrange(code, species, date)
)


# You can see that we have now added some additional fields to each data group table to indicate whether records are in wrangled_s123, screened_s123, or both, and whether a Season Summary Sheet was made. Screened and changelog are helper columns, filled based the values of record.in.wrangled, summary.sheet.made and record.in.screened, which can help you quickly identify the status of each record.

names(track_changes_hep)
str(track_changes_hep$nests)


# check screen_log against list of active nesters to make sure at least all active nesting data got screened
# hopefully this filter returns 0 rows
track_changes_hep$nests %>% 
  filter(total.nests > 0, screened == FALSE) %>% 
  view()
  


# You can filter based on changelog to see the records that differed between wrangled_s123 and screened_s123. The main fields we expect to change in screening are:

# * peak active date
# * ROP days
# * stage 4 date
 
# The numeric nest count fields should not change, so be sure to scan those to make sure they didn't change accidentally 

track_changes_hep$nests %>% 
  filter(grepl("changed", changelog)) %>% view()

track_changes_hep$stages %>% 
  filter(grepl("changed", changelog)) %>% 
  arrange(code, species, date, stage) %>% 
  view()

track_changes_hep$brood.sizes %>% 
  filter(grepl("changed", changelog)) %>% 
  arrange(code, species, date, brd) %>% 
  view()

# Save to disk

saveRDS(track_changes_hep, here(paste("data/track_changes/track_changes_hep_", zyear, sep = ""))) 

  
  
  
## Step 6 convert to HEPDATA ----

# HEPDATA is a "wide" data structure, and has field names in ALLCAPS. Thus this comprises a bunch of reshaping and renaming. 


source("https://raw.githubusercontent.com/scottfjennings/Survey123_to_HEPDATA/main/code/step6_screened_to_HEPDATA.R")

HEPDATA <- screened_to_HEPDATA(zyear)

count(HEPDATA, CODE) %>% view()

str(HEPDATA)

# 2020 only
HEPDATA <- HEPDATA %>% 
  mutate(PEAKACTVNSTS = ifelse((CODE == 70.0 & SPECIES %in% c("BCNH", "SNEG")), NA, PEAKACTVNSTS),
         NOTES = ifelse((CODE == 70.0 & SPECIES %in% c("BCNH", "SNEG")), "No BCNH or SNEG survey by USGS due to COVID", NOTES))



# Finally, save HEPDATA for appending to the HEPDATA access database.

saveRDS(HEPDATA, here(paste("data/as_HEPDATA/HEPDATA_", zyear, sep = "")))

write.csv(HEPDATA, here(paste("data/as_HEPDATA/HEPDATA_", zyear, ".csv", sep = "")), row.names = FALSE)

# HEPDATA2020 <- readRDS(here(paste("data/as_HEPDATA/HEPDATA_", zyear, sep = "")))


