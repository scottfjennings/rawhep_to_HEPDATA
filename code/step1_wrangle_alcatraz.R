
## this file contains code and instructions for converting Alcatraz nest monitoring data to ACR's HEP data format

## all this code should work fine if you acceessed it via the R Project named "alcatraz_hep". If you simply opened the R code file named "alcatraz", then you will have problems with file paths. 


## the data files we get from USGS for Alcatraz are in a funky format.
## they come in a xlsx file, with a sheet for each species (but 2018 data had both species in 1 sheet)
## in each species sheet, there are 3 rows at the top before the data actually start, with the column names occupying the 2nd and 3rd rows (yes, column names spanning 2 rows)
## then the actual nest check data are arranged in a wide format, with a row for each nest and then repeating sets of columns for each nest check (e.g. date, nest contents, etc) stacked horizontally out as far as needed to match the number of checks for each nest
## the number of checks varies between species and between individual nests, so the code below reactive to the nest-specific number of checks


# load the required packages
library(tidyverse) 
library(lubridate)
library(xlsx)
library(stringr)
library(here)



all_checks_usgs <- read.xlsx(here("data/alcatraz/70_Alcatraz_2021databackupSNG,BCNH.xlsx"), sheetIndex = "Visits", startRow = 3)



#--------------------------------------------------------

names(all_checks_usgs) <- tolower(names(all_checks_usgs))

alcatraz_checks <- all_checks_usgs %>% 
  select(nest = no., spp, date, contains(c("egg", "chick", "age", "notes"))) %>%
  mutate_all(as.character) %>% 
  pivot_longer(cols = contains(c("egg", "chick", "age", "notes"))) %>% 
  filter(!is.na(nest)) %>% 
  separate(name, c("variable", "check.num")) %>% 
  mutate(check.num = ifelse(is.na(check.num), 0, check.num),
         check.num = as.numeric(check.num),
         check.num = 1 + check.num) %>% 
  pivot_wider(id_cols = c("nest", "spp", "date", "check.num"), names_from = variable, values_from = value) %>% 
  filter(!is.na(egg) & !is.na(chick) & !is.na(age)) %>% 
  # some egg, chick, age cells are "x" for renests; tis will turn those to NA, which is appropriate
  mutate(date = ymd(date), across(.cols = c(egg, chick, age), as.numeric))



# some basic data checking ----

summary(alcatraz_checks)
# any unexpected species?
count(alcatraz_checks, spp)

# notes?
distinct(alcatraz_checks, notes) %>% view()


# fix any problems ----
alcatraz_checks <- alcatraz_checks %>% 
  filter(spp != "PASS")


#
## extract usefull info from notes; this likely not needed for hepraw_to_HEPDATA ----

notes_extracter <- function(sp_checks){
  # fill the egg or chick fields with data extracted from the notes field, as appropriate
  # many records with no info in egg or chick are nonetheless valuable because the notes specify that the nest was empty (egg and chick = 0); is valuable info for nest screening, allowing a nest's record to end with an affirmative failure rather than just no data
  # this function fills in egg and chick for some of the most common such occurences
  # warnings about NAs introduced by coercion are OK
  # this function adds a 'keeper' field, which flags records as having real data that we want to keep, or as being empty records that are relicts of the USGS table structure
  # the user can double check the records with keeper == "N" in the output to make sure the rules for exclusion worked correctly for the current data file
  # alc_noter() below can help ID new records (with different notes) that may need to be included in this function
  # INPUT: sp_checks is the species-specific df created by alcatraz_nest_checker()

  sp_checks2 <- alcatraz_checks %>%  
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
    select(nest, spp, date, egg = egg2, chick = chick2, age, notes, notes.failed)  %>% 
    mutate(keeper = ifelse(is.na(egg) & is.na(chick), "N", "Y")) %>%
    arrange(nest, date) %>%
    distinct()
                           
}


all_checks_extracted<- notes_extracter(all_dups_Fixed)



#--------------------------------------------------------
## IMPORTANT MANUAL STEP HERE: examine the ...checks_extracted outputs to make sure no valuable/valid records have been classified with keeper == "N". 

#Probably just need to look at records that have notes
alcatraz_checks %>% 
    filter(keeper == "N", !is.na(notes)) %>% 
  view()


# can do a manual edit if there is still good data in alc_keeperN_with_notes, changing keeper to Y and changing egg, chick, etc as appropriate
all_checks_extracted <- edit(all_checks_extracted)

  # a helper to check what the most common notes are and compare to the notes that are specified in notes_extracter() to make sure we're dealing with the important ones
all_checks_extracted %>% 
  select(notes) %>% 
  mutate(notes = sub("\\.$", "", notes),
         notes = tolower(notes)) %>% 
  table() %>% 
  data.frame() %>% 
  arrange(-Freq)


# and now filter to have just the keepers
  all_checks_extracted <- all_checks_extracted %>% 
    filter(keeper == "Y") %>% 
    select(-keeper)




# assign nest stage -----

alcatraz_assign_stage <- function(sp_checks){
  # finally convert check data from the Alcatraz format (massaged by above functions) to the HEP_screening format, including addition and filling of HEP-specific fields (status, stage, confidence)
  # note that there are some species-specific assumptions built in to the generation of Stage
  # if GREG or GBHE ever nest on Alcatraz the assignment of stages in this function will need to be changed to include stage 3

# create a small df with some of the nest stage boundaries
stages <- data.frame(spp = as.character(c("BCNH", "SNEG")),
                     end.stg2 = c(10, 10),
                     end.stg4 = c(15, 14))  

zsp_checks <- alcatraz_checks %>% 
  left_join(., stages, by = c("spp"))



# now the meat of the function to assign stages

hep_checks <- zsp_checks %>% 
  mutate(stage = as.numeric(""),
         stage = ifelse((chick == 0 | is.na(chick)) & egg > 0, 1, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age <= end.stg2, 2, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age > end.stg2 & age <= end.stg4, 4, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & age > end.stg4, 5, stage)) 
return(hep_checks)
}


alcatraz_checks_stages <- alcatraz_assign_stage(alcatraz_checks)

alcatraz_checks_stages <- alcatraz_checks_stages %>% 
  rename(species = spp)
# now converting to wrangled format ----
# first the dates table ----
# "date"
# "code"
# "multiple.survey.num"
# "start"
# "end"
# "num.surveys.this.date"
# "complete.count" 

dates <- alcatraz_checks_stages %>% 
  distinct(date, species) %>% 
  mutate(code = 70,
         multiple.survey.num = "",
         start = NA,
         end = NA,
         num.surveys.this.date = NA,
         complete.count = NA)


# then observers.effort ----
# "code"
# "colony"
# "observers"
# "total.days"
# "total.surveys"
# "total.hours"

observers_effort <- alcatraz_checks_stages %>% 
  distinct(date, species) %>% 
  count(species) %>% 
  rename(total.days = n) %>% 
  mutate(code = 70,
         colony = "Alcatraz Island",
         observers = NA,
         total.surveys = total.days,
         total.hours = NA)



# nests ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "total.nests"
# "complete.count"
# "peak.active"
# "obs.initials"

nests <- alcatraz_checks_stages %>% 
  filter(!is.na(stage)) %>% 
  count(date, species, name = "total.nests") %>% 
  group_by(species) %>% 
  mutate(peak.active = ifelse(total.nests == max(total.nests), TRUE, FALSE),
         code = 70,
         multiple.survey.num = 1,
         complete.count = "no",
         obs.initials = NA) %>% 
  ungroup() %>% 
  arrange(species, date)
  
  


# stages ----
# no stages for BCNH, SNEG


# brood.sizes ----
# no brood sizes for BCNH, SNEG



# for predators, disturbance and notes ----
all_notes <- alcatraz_checks_stages %>% 
  filter(!is.na(notes)) %>% 
  distinct(date, species, notes) %>% view()


# predators ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "predator.species"
# "present"
# "nesting"

# if notes suggest predators, run this and change predator.note to 1 for that record and fill in predator.species, present and nesting as appropriate:
predators <- all_notes %>% 
  mutate(predator.note = 0,
         predator.species = "",
         present = "",
         nesting = "",
         code = 70,
         multiple.survey.num = 1) %>% 
  edit()

predators <- predators %>% 
  filter(predator.note == 1) %>% 
  select(-predator.note)
  
# if no predators:
predators = NULL

# disturbance ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "obs.inf"
# "type"
# "result"
# "description"


# if notes suggest predators, run this and change predator.note to 1 for that record and fill in obs.inf, type, result, and description as appropriate:
disturbance <- all_notes %>% 
  mutate(disturbance.note = 0,
         obs.inf = "",
         type = "",
         result = "",
         description = "",
         code = 70,
         multiple.survey.num = 1) %>% 
  edit()
# then keep just the good records
disturbance <- disturbance %>% 
  filter(disturbance.note == 1) %>% 
  select(-disturbance.note)
  
# if no disturbance:
  disturbance = NULL


# notes ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "note.type"
# "notes"


# if there are pertinent notes to keep, run this and change keep.note to 1 for that record and fill in note.type and edit the note as appropriate:
disturbance <- all_notes %>% 
  mutate(keep.note = 0,
         note.type = "",
         code = 70,
         multiple.survey.num = 1) %>% 
  edit()
# then keep just the good records
notes <- notes %>% 
  filter(keep.note == 1) %>% 
  select(-keep.note)
# if no notes:
notes = NULL

wrangled_alcatraz <- list(dates = dates,
                      observers.effort = observers_effort,
                      nests = nests,
                      stages = NULL,
                      brood.sizes = NULL,
                      predators = predators,
                      disturbance = disturbance,
                      notes = notes)

wrangled_alcatraz %>% 
  saveRDS(here("data/wrangled/wrangled_alcatraz_2021"))

