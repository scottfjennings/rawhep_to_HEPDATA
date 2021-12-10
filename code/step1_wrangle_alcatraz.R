
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

alcatraz_checks <- alcatraz_checks %>% 
  filter(spp != "PASS") 

## extract usefull info from notes ----

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
    select(NO., SPP, DATE, egg = egg2, chick = chick2, Age, notes, notes.failed)  %>% 
    mutate(keeper = ifelse(is.na(egg) & is.na(chick), "N", "Y")) %>%
    arrange(NO., DATE) %>%
    distinct()
                           
}


all_checks_extracted<- notes_extracter(all_dups_Fixed)



#--------------------------------------------------------
## IMPORTANT MANUAL STEP HERE: examine the ...checks_extracted outputs to make sure no valuable/valid records have been classified with keeper == "N". 

#Probably just need to look at records that have notes
keeperN_with_noter <- function(sp_checks_extracted) {
  alc_keeperN_with_notes <- sp_checks_extracted %>% 
    filter(keeper == "N", !is.na(notes))
}
alc_keeperN_with_notes <- keeperN_with_noter(all_checks_extracted)


# can do a manual edit if there is still good data in alc_keeperN_with_notes, changing keeper to Y and changing egg, chick, etc as appropriate
alc_keeperN_with_notes_edited <- edit(alc_keeperN_with_notes)
all_checks_extracted_edited <- rbind(all_checks_extracted, alc_keeperN_with_notes_edited) %>% 
  arrange(SPP, NO., DATE)
# then use all_checks_extracted_edited below

alc_noter <- function(sp_checks) {
  # a helper function to check what the most common notes are and compare to the notes that are specified in notes_extracter() to make sure we're dealing with the important ones
  sp_notes <- sp_checks %>% 
  select(notes) %>% 
  mutate(notes = sub("\\.$", "", notes),
         notes = tolower(notes)) %>% 
  table() %>% 
  data.frame() %>% 
  arrange(-Freq)
}
bcnh_notes <- alc_noter(bcnh_checks)


trim_keepers <- function(sp_checks_extracted){
# and now filter to have just the keepers
  sp_checks_trimmed <- sp_checks_extracted %>% 
    filter(keeper == "Y") %>% 
    select(-keeper)
}


all_checks_trimmed <- trim_keepers(all_checks_extracted)
all_checks_trimmed <- trim_keepers(all_checks_extracted_edited)

#--------------------------------------------------------

alcatraz2HEPer <- function(sp_checks){
  # finally convert check data from the Alcatraz format (massaged by above functions) to the HEP_screening format, including addition and filling of HEP-specific fields (status, stage, confidence)
  # note that there are some species-specific assumptions built in to the generation of Stage
  # if GREG or GBHE ever nest on Alcatraz the assignment of stages in this function will need to be changed to include stage 3

# create a small df with some of the nest stage boundaries
stages <- data.frame(SPP = as.character(c("BCNH", "SNEG")),
                     end.stg2 = c(10, 10),
                     end.stg4 = c(15, 14))  

zsp_checks <- sp_checks %>% 
  left_join(., stages, by = c("SPP"))



# now the meat of the function to generate the HEP_screening data structure

hep_checks <- zsp_checks %>%
  setNames(tolower(names(.))) %>% 
  rename(ch.age = age) %>% 
  mutate(date = ymd(date),
         adults=as.numeric(""),
         stage = as.numeric(""),
         stage = ifelse((chick == 0 | is.na(chick)) & egg > 0, 1, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age <= end.stg2, 2, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age > end.stg2 & ch.age < end.stg4, 4, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age > end.stg4, 5, stage),
         confidence = "",
         status = "A",
         status = ifelse(egg == 0 & chick == 0, "I", status),
         status = ifelse(egg == 0 & is.na(chick), "I", status),
         status = ifelse(is.na(egg) & chick == 0, "I", status),
         status = ifelse(notes == "not checked" & !is.na(notes) |
                        (egg == 9 & chick == 9) |
                        (egg == 9 & is.na(chick)) |
                        (is.na(egg) & chick == 9), "P", status)) %>% 
  select(date, nest = no., spp, status, adults, stage, chicks = chick, confidence, notes) %>% 
  arrange(nest, spp, date) %>% 
  unique() 
return(hep_checks)
}


alc_hep <- alcatraz2HEPer(all_checks_trimmed)

## as a quick check to make sure 'stage' and 'status' have been filled correctly, these filters should return 0 records
filter(alc_hep, status == "A", is.na(stage), is.na(chicks)) # there should be no records with status == "A" but no data for stage or chicks
filter(alc_hep, status == "I", !is.na(stage)) # there should be no records with status == "I" and valid data for stage
filter(alc_hep, status == "I", stage > 0, chicks > 0) # there should be no records with status == "I" and valid data for stage and chicks

year <- 2017
char_date <- as.Date(Sys.time()) %>% as.character() %>% gsub("-", "", .)
file.path <- paste("Alcatraz_ready4screening/alcatraz", year, "_4screening_codeV", code_version, "_", char_date, ".csv", sep = "")
write.csv(alc_hep, file.path, row.names = F)
###

alc_hep <- read.csv("Alcatraz_ready4screening/alcatraz2018_4screening20190908.csv")

nest_viewer <- function(zspp, znest, znest.exact = FALSE) {
  if(znest.exact == FALSE) {
  alc_hep %>% 
    filter(spp == zspp & grepl(znest, nest)) %>% 
    arrange(date)
  } else {
      alc_hep %>% 
    filter(spp == zspp, nest == znest) %>% 
    arrange(date)
  }
}


nest_viewer_all_checks <- function(zspp, znest, znest.exact = FALSE) {
  if(znest.exact == FALSE) {
  all_checks %>% 
    filter(SPP == zspp & grepl(znest, NO.)) %>% 
    arrange(DATE)
  } else {
      all_checks %>% 
    filter(SPP == zspp, NO. == znest) %>% 
    arrange(DATE)
  }
}


