
## this file contains code and instructions for converting Alcatraz nest monitoring data to ACR's HEP data format

## 


# all_checks_usgs <- read.xlsx(here("data/alcatraz/70_Alcatraz_2021databackupSNG,BCNH.xlsx"), sheetIndex = "Visits", startRow = 3)

# names(all_checks_usgs) <- tolower(names(all_checks_usgs))

#' Pivot funky format USGS data to long format
#'
#' @param usgs_checks data frame from USGS provided .csv or .xlsx. has a record for each nest observed (multiple observation days per row)
#'
#' @return data frame with a record for each nest for each observation day
#' @export  
#' 
#' @details The data files we get from USGS for Alcatraz are in a funky format. They come in a xlsx file, with a sheet for each species (but 2018 data had both species in 1 sheet). In each species sheet, there are 3 rows at the top before the data actually start, with the column names occupying the 2nd and 3rd rows (i.e., column names spanning 2 rows). Then the actual nest check data are arranged in a wide format, with a row for each nest and then repeating sets of columns for each nest check (e.g. date, nest contents, etc) stacked horizontally out as far as needed to match the number of checks for each nest. The number of checks varies between species and between individual nests.
#' 
#'  This function pivots these data to a longer format with a record for each check of each nest, and it is reactive to the nest-specific number of checks. This takes advantage of the way R handles duplicated column names upon reading in data: adding sequential numbers to the end of column names (e.g. date, date.1, date.2, etc.). These trailing numbers can be used in dplyr::select() to isolate each observation day. 
#'
#' @examples 
#' # read data
#' all_checks_usgs <- read.xlsx(here("data/alcatraz/70_Alcatraz_2021databackupSNG,BCNH.xlsx"), sheetIndex = "Visits", startRow = 3) 
#' 
#' # names to lower case
#' names(all_checks_usgs) <- tolower(names(all_checks_usgs)) 
#' 
#' long_alcatraz <- pivot_alcatraz(all_checks_usgs)
pivot_alcatraz <- function(usgs_checks) {
  
names(usgs_checks) <- tolower(names(usgs_checks))

usgs_checks <- usgs_checks %>% 
  rename(species = spp)
# remove un-needed columns and make field types friendly  
just_checks <- all_checks_usgs %>% 
  select(nest = no., species, contains(c("date", "egg", "chick", "age", "notes"))) %>%
  mutate_all(as.character) %>% 
  filter(!is.na(nest)) 

# visit_cols is a vector representing the maximum number of observation days, to be used for slicing off each observation day.
visit_cols = seq(1:(((ncol(just_checks)-2)/5) - 1))
visit_cols = as.character(visit_cols)

# this slices off the columns for each observation day, plus the species and nest id columns
get_visit_cols <- function(col_nums) {
  alc_visit <- just_checks %>% 
    select(nest, species, ends_with(col_nums)) %>% 
    # there's probably a way to gsub(col_nums) to do this rename in one line, but getting the evaluation is likely tricky and I didn't try. this works.
    rename(date = contains("date"),
           egg = contains("egg"),
           chick = contains("chick"),
           age = contains("age"),
           notes = contains("notes"))
  return(alc_visit)
}

# loop through get_visit_cols on each element of visit_cols and stack them vertically
all_visit_cols <- map_df(visit_cols, get_visit_cols)

# do a bit of basic cleaning
long_alcatraz <- just_checks %>% 
  select(nest, species, date, egg, chick, age, notes) %>% 
  bind_rows(all_visit_cols) %>% 
  filter(!is.na(egg) & !is.na(chick) & !is.na(age)) %>% 
  # some egg, chick, age cells are "x" for renests; this will turn those to NA, which is appropriate
  mutate(date = ymd(date), across(.cols = c(egg, chick, age), as.numeric)) %>% 
  arrange(species, nest, date)

return(long_alcatraz)

}


# some basic data checking ----

# summary(long_alcatraz)
# any unexpected species?
# count(long_alcatraz, species)

# notes?
# distinct(long_alcatraz, notes) %>% view()


# fix any problems ----
# Remove from 2021 a "passerine" nest that was monitored for fun.
#long_alcatraz <- long_alcatraz %>% filter(species != "PASS")



#' Extract info from notes to fill the egg or chick fields with data extracted from the notes field, as appropriate
#'
#' @param long_alcatraz data frame with pivoted-long alcatraz data; output from pivot_alcatraz().
#'
#' @return data frame with similar structure as long_alcatraz, but with "keeper" field added, see details.
#' @export
#' 
#' @details Many records with egg or chick blank are nonetheless valuable because the notes specify that the nest was empty (so egg and chick should be 0); is valuable info for nest screening, allowing a nest's record to end with an affirmative failure rather than just no data. This function fills in egg and chick for some of the most common such occurrences.
#' 
#' Warnings about NAs introduced by coercion are OK
#' 
#' This function adds a 'keeper' field, which flags records as having real data that we want to keep, or as being empty records that are relics of the USGS table structure.
#' 
#' The user can then double check the records with keeper == "N" in the output to make sure the rules for exclusion worked correctly for the current data file. 
#'
#' @examples all_checks_extracted <- notes_extracter(long_alcatraz)
notes_extracter <- function(long_alcatraz){
  sp_checks2 <- long_alcatraz %>%  
  mutate(notes.failed = ifelse(
                          #(is.na(egg) & is.na(chick)) & 
                         ((str_detect(notes, "empty") & !str_detect(notes, "missed")) | 
                            str_detect(notes, "destroyed") | 
                            str_detect(notes, "depred") | 
                            str_detect(notes, "renest") | 
                            str_detect(notes, "re-nest") |
                            (str_detect(notes, "see") & str_detect(notes, "\\d")) |
                            (str_detect(notes, "now") & str_detect(notes, "\\d"))), "Y", "N"),
         notes.failed = ifelse(is.na(notes), "N", notes.failed)) 
  
  sp_checks3 <- sp_checks2 %>% 
    # fill egg and chick with 9 if check missed
    mutate(egg2 = ifelse(grepl("not checked", notes) | 
                           grepl("not found", notes) | 
                           grepl("missed", notes), 9, egg),
           chick2 = ifelse(grepl("not checked", notes) | 
                           grepl("not found", notes) | 
                           grepl("missed", notes), 9, chick),
           egg2 = ifelse(notes.failed == "Y", 0, egg2),
           chick2 = ifelse(is.na(chick2) & str_detect(notes, "chick"), 8, chick2),
           chick2 = ifelse(chick2 == 8 & str_detect(notes, "no") & str_detect(notes, "chick"), 9, chick2),
           chick2 = ifelse(is.na(chick2) & notes.failed == "Y", 0, chick2),
           egg2 = ifelse(is.na(egg2), egg, egg2),
           chick2 = ifelse(is.na(chick2), chick, chick2)) 
  
  sp_checks4 <- sp_checks3 %>% 
    select(-egg, -chick) %>%
    select(nest, species, date, egg = egg2, chick = chick2, age, notes, notes.failed)  %>% 
    mutate(keeper = ifelse(is.na(egg) & is.na(chick), "N", "Y")) %>%
    arrange(nest, date) %>%
    distinct()
return(sp_checks4)                           
}


#' Export Alcatraz notes
#'
#' @param sp_checks data frame in long format with a notes field. Could be the output of either pivot_alcatraz or notes_extractor. 
#' @param zyear 
#'
#' @return nothing, just saves .csv to the season_summary_forms folder for zyear
#' @export
#'
#' @details filters to all non-NA notes and exports them as a .csv to the year appropriate season summary forms folder
#'
#' @examples
#' export_alcatraz_notes(all_checks_extracted)
export_alcatraz_notes <- function(sp_checks, zyear) {
all_checks_extracted %>% 
  filter(!is.na(notes)) %>% 
  distinct(species, date, notes) %>% 
  write.csv(here(paste("season_summary_forms/", zyear, "/Alcatraz_usgs_notes.csv", sep = "")), row.names = FALSE)
}


check_notes_frequency <- function() {
  all_checks_extracted %>% 
  select(notes) %>% 
  mutate(notes = sub("\\.$", "", notes),
         notes = tolower(notes)) %>% 
  table() %>% 
  data.frame() %>% 
  arrange(-Freq) %>% 
  view()
}

# and now filter to have just the keepers
#  all_checks_extracted <- all_checks_extracted %>% 
#    filter(keeper == "Y") %>% 
#    select(-keeper)




# assign nest stage -----

#' Assign Alcatraz nest stages
#' 
#' Much of HEP data is based on nest stages, which are not recorded by USGS Alcatraz observers. This function assigns each nest to stage based on USGS-estimated chick age. Thus stage for Alcatraz BCNH and SNEG are not exactly the same as for all other HEP data.
#'
#' @param checks_extracted data frame with Alcatraz check data. this should generally be the output of all_checks_extracted()
#'
#' @return data frame with same structure as checks_extracted, but with stage field added.
#' @export
#' 
#' @details 
#' The generation of Stage is species-specific. This function currently only handles BCNH and SNEG. If we get USGS data on other species the assignment of stages in this function will need to be changed to accommodate those species.

#' 
#' @examples alcatraz_checks_stages <- alcatraz_assign_stage(all_checks_extracted)
alcatraz_assign_stage <- function(checks_extracted){

# create a small df with some of the nest stage boundaries
stages <- data.frame(species = as.character(c("BCNH", "SNEG")),
                     end.stg2 = c(10, 10),
                     end.stg4 = c(15, 14))  
# add it to the check data
zsp_checks <- checks_extracted %>% 
  left_join(., stages, by = c("species"))

# now the meat of the function to assign stages
hep_checks <- zsp_checks %>% 
  mutate(stage = as.numeric(""),
         stage = ifelse((chick == 0 | is.na(chick)) & egg > 0, 1, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age <= end.stg2, 2, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age > end.stg2 & age <= end.stg4, 4, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age > end.stg4, 5, stage)) 
return(hep_checks)
}



  
  


#' Wrangle Alcatraz data
#'
#' @param checks_stages 
#'
#' @return a list with 8 elements to match the other wrangled_ objects: dates, observers.effort, nests, stages, brood.sizes, predators, disturbance, notes
#' @export
#' 
#'  @details some info is not collected at all by USGS observers (start and end times), or only exists in by-nest notes (e.g., disturbance, predator info). So wrangled_alcatraz ends up having a lot of blank cells. Some of this is to be filled manually during Season Summary sheet screening.
#' 
#' @examples
wrangle_alcatraz <- function(checks_stages) {
#
dates <- checks_stages %>% 
  distinct(date, species) %>% 
  mutate(code = 70.888,
         multiple.survey.num = "",
         start = NA,
         end = NA,
         num.surveys.this.date = NA,
         complete.count = "no") %>%
  mutate(date = as.Date(date),
         multiple.survey.num = as.numeric(multiple.survey.num))

#
observers_effort <- checks_stages %>% 
  distinct(date, species) %>% 
  count(species) %>% 
  rename(total.days = n) %>% 
  mutate(code = 70.888,
         colony = "Alcatraz Island",
         observers = "USGS/NPS",
         total.surveys = total.days,
         total.hours = NA)

# 
nests <- checks_stages %>%  
  filter(!is.na(stage), (egg > 0 | chick > 0)) %>% 
  count(date, species, name = "total.nests") %>% 
  group_by(species) %>% 
  mutate(peak.active = ifelse(total.nests == max(total.nests), TRUE, FALSE),
         code = 70.888,
         multiple.survey.num = 1,
         complete.count = "no",
         obs.initials = NA,
         date = as.Date(date)) %>% 
  ungroup() %>% 
  arrange(species, date)
  
# no stages for BCNH, SNEG
# no brood sizes for BCNH, SNEG

#
predators <- data.frame(date = as.Date(NA),
                        predator.species = "",
                        present = as.double(""),
                        nesting = as.double(""),
                        code = 70.888,
                        multiple.survey.num = 1)

#
disturbance <- data.frame(date = as.Date(NA),
                          obs.inf = "",
                          type = "",
                          result = "",
                          description = "",
                          code = 70.888,
                          multiple.survey.num = "1")

#
notes <- data.frame(date = as.Date(NA),
                    note.type = "",
                    code = 70.888,
                    multiple.survey.num = 1)

wrangled_alcatraz <- list(dates = dates,
                      observers.effort = observers_effort,
                      nests = nests,
                      stages = NULL,
                      brood.sizes = NULL,
                      predators = predators,
                      disturbance = disturbance,
                      notes = notes)

return(wrangled_alcatraz)
}


