

# load the required packages
library(RODBC)
# convert screened data to HEPDATA format
# screened data are saved as several dfs that are bundled in a list. Data for the various HEPDATA columns are sourced from the appropriate elements (dfs) of this list

# this is one big function


screened_to_HEPDATA <- function(zyear) {
  
  
   
screened_sheets <- get_screened_col_spp(zyear) %>% 
    fix_alc_code() %>% 
    #select(-screened) %>% 
    mutate(code = as.numeric(code))
  
  
screened_hep <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))

# 70.888 back to 70 for alcatraz colony code
screened_hep <- map(screened_hep, fix_alc_code)


surveyed_colonies  <- get_surveyed(zyear) %>% 
  fix_alc_code()

surveyed_colonies_all_spp <- expand.grid(CODE = distinct(surveyed_colonies, code)$code,
                                         SPECIES = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO"))

  


# 1         DATAID
# 2 YEAR; # 3 SITE; # 4 CODE; # 5 SPECIES
# 2 added based on zyear above. 4, 5 are in every table, will be used to merge all tables, 3 added from saved site name file ----
# 6 and 7 not consistently filled in current HEPDATA, these filled with NA at the end ----
# 6   PEAKRICHNESS
# 7   TOTALSPECIES
#8 and 9 come from effort_summary ----
# 8   NUMBERVISITS
# 9     TOTALHOURS

# this is the effort for all screened sheets. there should be a screened sheet for every surveyed colony
effort_screened <- screened_hep$observers.effort %>% 
  right_join(screened_sheets) %>% 
  select(CODE = code, SPECIES = species, NUMBERVISITS = total.surveys, TOTALHOURS = total.hours) 


non_nester_col_spp <- anti_join(surveyed_colonies_all_spp, rename(screened_sheets, CODE = code, SPECIES = species)) 


non_nester_effort <- effort_screened %>%
  group_by(CODE) %>% 
  filter(NUMBERVISITS == max(NUMBERVISITS)) %>% 
  distinct(CODE, NUMBERVISITS, TOTALHOURS)  %>% 
  right_join(non_nester_col_spp) %>% 
  arrange(CODE, SPECIES)

effort <- full_join(effort_screened, non_nester_effort)

# 10  PEAKACTVNSTS ----
# this is all the colony X species combos that were really active
# NOTE as of Feb 2023 PEAKACTVDATE is a new field for HEPDATA
peakactvnsts.gr0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>%  
  filter(peak.active == 1) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = total.nests, PEAKACTVDATE = date)

# this is all the colony X species combos that were not actively nesting but for which the season summary sheet was screened.
peakactvnsts.0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>% 
  group_by(code, species) %>% 
  summarise(peakactvnsts = sum(peak.active)) %>% 
  filter(peakactvnsts == 0) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = peakactvnsts)

peakactvnsts_out <- bind_rows(peakactvnsts.gr0, peakactvnsts.0) %>% 
  full_join(surveyed_colonies_all_spp) %>% 
  mutate(PEAKACTVNSTS = replace_na(PEAKACTVNSTS, 0)) %>% 
  arrange(CODE, SPECIES)

HEPDATA_out <- full_join(effort, peakactvnsts_out)
# 11-13 (-16?) no longer collected, these added with NA at the end ----
# 11   INDIVIDUALS
# 12    FOCALNESTS
# 13    FOCFAILURE
# 14   DISTURBANCE
# 15        SOURCE


# 16         NOTES ----
spp_notes <- screened_hep$notes %>%  
  right_join(screened_sheets) %>% 
  filter(note.type == "for.HEPDATA.species", notes != "", !is.na(notes)) %>% 
  select(CODE = code, SPECIES = species, NOTES.spp = notes) %>% 
  distinct()

col_notes <- screened_hep$notes %>%  
  right_join(screened_sheets) %>% 
  filter(note.type == "for.HEPDATA.colony", notes != "", !is.na(notes)) %>% 
  select(CODE = code, NOTES.col = notes) %>% 
  distinct() %>% 
  left_join(surveyed_colonies_all_spp)

# If colony notes are the same as species notes, or colony notes are contained in species notes, then just use species notes, otherwise, paste the 2 together

notes = full_join(spp_notes, col_notes) %>% 
  mutate(NOTES.col = replace_na(NOTES.col, "zzzz"),
         NOTES.spp = ifelse(is.na(NOTES.spp), NOTES.col, NOTES.spp),
         zz = NOTES.spp == NOTES.col,
         xx = str_detect(NOTES.spp, NOTES.col)) %>% 
  mutate(NOTES = ifelse(NOTES.spp == NOTES.col | str_detect(NOTES.spp, NOTES.col), NOTES.spp, paste(NOTES.spp, NOTES.col)),
         NOTES = gsub("zzzz", "", NOTES)) %>% 
  select(CODE, SPECIES, NOTES)


HEPDATA_out <- full_join(HEPDATA_out, notes)

# stage 4 brood sizes ----
# 17 BRD1; # 18 BRD2; # 19 BRD3; # 20 BRD4; # 21 BRD5; # 22 BRD6
brood_sizes <- screened_hep$brood.sizes %>%  
  right_join(screened_sheets) %>% 
  data.frame() %>% 
  filter(brd.size.date == TRUE) %>% 
  mutate(brd = paste("BRD", brd, sep = "")) %>% 
  pivot_wider(id_cols = c(code, species, date), values_from = num.nests, names_from = brd)


# stop test for multple brood size days selected
num_brood_size_days <- brood_sizes %>% 
  count(code, species, date) %>% 
  mutate(out.message = paste(code, species, "\n")) %>% 
  filter(n > 1)

if(nrow(num_brood_size_days) > 0) {
  stop("multiple days selected for brood size for: \n", num_brood_size_days$out.message, "\nPlease edit appropriate Season Summary Sheets")
}
# if there are any records here, need to stop and fix before proceeding. can uncomment the view call to see code X species combos with multiple brood size dates
# fix will likely need to be returning to season summary form and editing the brood size table so only 1 date is selected

brood_sizes <- brood_sizes %>% 
  select(CODE = code, SPECIES = species, contains("BRD"))

HEPDATA_out <- full_join(HEPDATA_out, brood_sizes)

# ROP stage data ----
# 23 MARSTGEDATE; 24 MARSTAGE1; # 25 MARSTAGE2; # 26 MARSTAGE3; # 27 MARSTAGE4; # 28 MARSTAGE5; # 29 LTMARSTGDATE; # 30 LTMARSTAGE1; # 31 LTMARSTAGE2; # 32 LTMARSTAGE3; # 33 LTMARSTAGE4; # 34 LTMARSTAGE5; # 35 APRSTGEDATE; # 36 APRSTAGE1; # 37 APRSTAGE2; # 38 APRSTAGE3; # 39 APRSTAGE4; # 40 APRSTAGE5; # 41 MAYSTGEDATE; # 42 MAYSTAGE1; # 43; MAYSTAGE2; # 44; MAYSTAGE3; # 45 MAYSTAGE4; # 46 MAYSTAGE5; # 47 JUNSTGEDATE; # 48 JUNSTAGE1; # 49 JUNSTAGE2; # 50 JUNSTAGE3; # 51; JUNSTAGE4; # 52 JUNSTAGE5; # 53 LTJUNSTGDATE; # 54 LTJUNSTAGE1; # 55 LTJUNSTAGE2; # 56 LTJUNSTAGE3; # 57 LTJUNSTAGE4; # 58 LTJUNSTAGE5; # 59 JULSTGEDATE; # 60 JULSTAGE1; # 61 JULSTAGE2; # 62 JULSTAGE3; # 63 JULSTAGE4; # 64 JULSTAGE5
rop_names <- data.frame(which.rop = paste("rop.", seq(1, 7), sep = ""),
                        rop.name = c("MAR", "LTMAR", "APR", "MAY", "JUN", "LTJUN", "JUL"),
                        rop.date.name = c("MARSTGEDATE", "LTMARSTGDATE", "APRSTGEDATE", "MAYSTGEDATE", "JUNSTGEDATE", "LTJUNSTGDATE", "JULSTGEDATE"))


all_rop_stages <- screened_hep$stages %>%  
  right_join(screened_sheets) %>% 
  filter(grepl("rop", which.rop)) %>% 
  mutate(stage = paste("STAGE", stage, sep = "")) %>% 
  left_join(., rop_names)

# stop test for multiple dates selected for the smae ROP
dup_rop <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  count(code, species, rop.stage, num.nests) %>% 
  filter(n > 1) %>% 
  mutate(out.message = paste(code, species, "\n"))

if(nrow(dup_rop) > 0) {
  stop("multiple days selected for the same ROP for: \n", dup_rop$out.message, "\nPlease edit appropriate Season Summary Sheets")
}

rop_nests <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  select(code, species, rop.stage, num.nests) %>% 
  pivot_wider(id_cols = c("code", "species"), names_from = rop.stage, values_from = num.nests)

rop_dates <- all_rop_stages %>% 
  select(code, species, date, rop.date.name) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("code", "species"), names_from = rop.date.name, values_from = date)
  
rop_dates_nests <- full_join(rop_nests, rop_dates) %>% 
  select(CODE = code, SPECIES = species, 
         MARSTGEDATE, MARSTAGE1, MARSTAGE2, MARSTAGE3, MARSTAGE4, MARSTAGE5
         , LTMARSTGDATE, LTMARSTAGE1, LTMARSTAGE2, LTMARSTAGE3, LTMARSTAGE4, LTMARSTAGE5
         , APRSTGEDATE, APRSTAGE1, APRSTAGE2, APRSTAGE3, APRSTAGE4, APRSTAGE5
         , MAYSTGEDATE, MAYSTAGE1, MAYSTAGE2, MAYSTAGE3, MAYSTAGE4, MAYSTAGE5
         , JUNSTGEDATE, JUNSTAGE1, JUNSTAGE2, JUNSTAGE3, JUNSTAGE4, JUNSTAGE5
         , LTJUNSTGDATE, LTJUNSTAGE1, LTJUNSTAGE2, LTJUNSTAGE3, LTJUNSTAGE4, LTJUNSTAGE5
         #, JULSTGEDATE, JULSTAGE1, JULSTAGE2, JULSTAGE3, JULSTAGE4, JULSTAGE5
         ) %>% 
  full_join(surveyed_colonies_all_spp) %>% 
  arrange(CODE, SPECIES)


HEPDATA_out <- full_join(HEPDATA_out, rop_dates_nests)

# disturbances ----
# 65 DIST1DATE; # 66 DIST1TYPE; # 67 DIST1RESULT; # 68 DIST2DATE; # 69 DIST2TYPE; # 70 DIST2RESULT; # 71 DIST3DATE; # 72 DIST3TYPE; # 73 DIST3RESULT; # 74 DIST4DATE; # 75 DIST4TYPE; # 76 DIST4RESULT; # 77 DIST5DATE; # 78 DIST5TYPE; # 79 DIST5RESULT; # 80 DIST6DATE; # 81 DIST6TYPE; # 82 DIST6RESULT; # 83; DIST7DATE; # 84 DIST7TYPE; # 85 DIST7RESULT; # 86 DIST8DATE; # 87 DIST8TYPE; # 88 DIST8RESULT; # 89 DIST9DATE; # 90 DIST9TYPE; # 91 DIST9RESULT

disturbance <- screened_hep$disturbance %>%  
  right_join(screened_sheets) %>% 
  mutate(result = tolower(result)) %>% 
  filter(!grepl("x", type), !grepl("x", result)) %>% 
  group_by(code, species) %>% 
  arrange(code, species, date) %>% 
  mutate(dist.num = row_number()) %>% 
  ungroup() %>% 
  left_join(data.frame(dist.num = seq(1, 8))) %>% 
  rename("CODE" = code,
         "SPECIES" = species)

non_nesters_disturbance <- disturbance %>% 
  filter(copy.to.non.nesters == "yes") %>% 
  select(-SPECIES) %>% 
  left_join(surveyed_colonies_all_spp)

disturbance_long <- disturbance %>% 
  full_join(non_nesters_disturbance) %>% 
  select(CODE, SPECIES, date, type, result, dist.num) %>% 
  pivot_longer(cols = c(date, type, result)) %>% 
  mutate(name = toupper(name),
         outname = paste("DIST", dist.num, name, sep = ""))

disturbance_out <- disturbance_long %>% 
  pivot_wider(id_cols = c(CODE, SPECIES), names_from = outname, values_from = value) %>%
  filter(!is.na(CODE)) %>% 
  full_join(surveyed_colonies_all_spp) %>% 
  arrange(CODE, SPECIES)


HEPDATA_out <- full_join(HEPDATA_out, disturbance_out)

# predators ----
# 92   GHOWNESTING
# 93   RTHANESTING
# 94   OSPRNESTING
# 95   CORANESTING
# 96   BAEANESTING
# 97  TUVUROOSTING



nesting_predators <- screened_hep$predators %>%  
  right_join(screened_sheets) %>% 
  filter((nesting == 1 | tolower(copy.to.non.nesters) == "yes"), predator.species %in% c("GHOW", "RTHA", "OSPR", "CORA", "BAEA", "TUVU")) %>% 
  mutate(predator = ifelse(predator.species == "TUVU", 
                           paste(toupper(predator.species), "ROOSTING", sep = ""),
                           paste(toupper(predator.species), "NESTING", sep = ""))) %>% 
  distinct(code, predator, nesting) %>% 
  rename("CODE" = code)  %>% 
  full_join(surveyed_colonies_all_spp) %>% 
  arrange(CODE, SPECIES)
  

predators <- nesting_predators %>% 
  pivot_wider(id_cols = c(CODE, SPECIES), names_from = predator, values_from = nesting) %>% 
  arrange(CODE, SPECIES)
  
  


HEPDATA_out <- full_join(HEPDATA_out, predators)%>% 
  arrange(CODE, SPECIES)
# 98 Entry_Proofed
# 99    Entered_By

hepdata_names <- readRDS("data/support_data/HEPDATA_names") %>% 
              data.frame() %>% 
              rename(colname = 1) %>% 
              mutate(blah = NA) %>% 
              pivot_wider(values_from = blah, names_from = colname)


HEPDATA_out <- HEPDATA_out %>% 
  mutate(Entry_Proofed = "",
         Entered_By = paste("code-generated record.", Sys.Date())) %>% 
  left_join(., readRDS(here("data/support_data/HEP_site_names_nums_utm")) %>% 
              select(CODE = code, SITE = site.name)) %>%  
  mutate(YEAR = zyear) %>% 
  full_join(., hepdata_names) %>% 
  select(names(hepdata_names), PEAKACTVDATE) %>% 
  relocate(PEAKACTVDATE, .after = PEAKACTVNSTS) %>% 
  filter(!is.na(CODE))

HEPDATA_out <- HEPDATA_out %>% 
  mutate(across(everything(), ~as.character(.)),
         across(contains("DATE"), ~as.POSIXct(.)),
         across(matches("RESULT|NESTING|STAGE|BRD|YEAR|CODE"), ~as.numeric(.)),
         across(contains("TYPE"), ~as.character(.)),
         across(c("PEAKRICHNESS", "TOTALSPECIES", "NUMBERVISITS", "TOTALHOURS", "PEAKACTVNSTS", "INDIVIDUALS", "FOCALNESTS", "FOCFAILURE"), ~as.numeric(.)))



return(HEPDATA_out)
}


#' hep_observers_from_access
#' 
#' Get the observers table from HEPDATA 
#'
#' @param HEP_location file path for the access database
#'
#' @return
#' @export
#'
#' @examples hep_observers <- hep_observers_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
hep_observers_from_access <- function(HEP_location = HEP_location){
  if(is.na(HEP_location)){
    print("Please assign location of HEPDATA access file to HEP_location")
  } else {
    db <- HEP_location
  }
 
 con2 <- odbcConnectAccess2007(db)

hep_observers <- sqlFetch(con2, "tbl_HEPObservers")
close(con2)

return(hep_observers)
}


#' hep_observers_from_access
#' 
#' Get the observers table from HEPDATA 
#'
#' @param HEP_location file path for the access database
#'
#' @return
#' @export
#'
#' @examples max_DATAID <- max_DATAID_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
max_DATAID_from_access <- function(HEP_location = HEP_location){
  if(is.na(HEP_location)){
    print("Please assign location of HEPDATA access file to HEP_location")
  } else {
    db <- HEP_location
  }
 
 con2 <- odbcConnectAccess2007(db)

hepdat <- sqlFetch(con2, "tbl_HEPDATA")
close(con2)

hepdat <- hepdat %>% 
  select(DATAID, YEAR, CODE, SPECIES) %>% 
  filter(DATAID == max(DATAID))

return(hepdat)
}



#' CGP_contacts_from_access
#' 
#' Get the CGP_Contacts table from CGRC_Contacts 
#'
#' @param contacts_location file path for the access database
#'
#' @return
#' @export
#'
#' @examples cgp_contacts <- CGP_contacts_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/Contacts/CGRC_Contacts.accdb")
CGP_contacts_from_access <- function(contacts_location = contacts_location){
  if(is.na(contacts_location)){
    print("Please assign location of HEPDATA access file to contacts_location")
  } else {
    db <- contacts_location
  }
 
 con2 <- odbcConnectAccess2007(db)

contacts <- sqlFetch(con2, "tbl_CGP_Contacts")
close(con2)

return(contacts)
}

#' get_observers_from_screened
#' 
#' Get observers for each colony.
#'
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
get_observers_from_screened <- function(zyear) {
  
observers <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))$observers.effort %>%
  fix_alc_code() %>%
  select(code, observers) %>%
  separate(observers, into = c(paste("obs", seq(1:8), sep = ".")), sep = ";") %>%
  pivot_longer(cols = contains("obs."), values_to = "observer") %>%
  filter(!is.na(observer)) %>%
  mutate(observer = gsub("\\*", "", observer)) %>%
  mutate(observer = trimws(observer)) %>%
  distinct(code, observer)

}


# hepdata_names <- readRDS("data/HEPDATA_names")
# we need to expand out to all possible columns here

# deprecated
screened_to_HEPDATA_active_colonies <- function(zyear) {


    
screened_sheets <- get_screened_col_spp(zyear) %>% 
    fix_alc_code() %>% 
    select(-screened) %>% 
    mutate(code = as.numeric(code))
  
  
screened_hep <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))


screened_hep <- map(screened_hep, fix_alc_code)


# 1         DATAID
# 2 YEAR; # 3 SITE; # 4 CODE; # 5 SPECIES
# 2 added based on zyear above. 4, 5 are in every table, will be used to merge all tables, 3 added from saved site name file ----
# 6 and 7 not consistently filled in current HEPDATA, these filled with NA at the end ----
# 6   PEAKRICHNESS
# 7   TOTALSPECIES
#8 and 9 come from effort_summary ----
# 8   NUMBERVISITS
# 9     TOTALHOURS
effort <- screened_hep$observers.effort %>% 
  right_join(screened_sheets) %>% 
  select(CODE = code, SPECIES = species, NUMBERVISITS = total.surveys, TOTALHOURS = total.hours)

# 10  PEAKACTVNSTS ----
# this is all the colony X species combos that were really active
# NOTE as of Feb 2023 PEAKACTVDATE is a new field for HEPDATA
peakactvnsts.gr0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>%  
  filter(peak.active == 1) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = total.nests, PEAKACTVDATE = date)

# this is all the colony X species combos that were not actively nesting but for which the season summary sheet was screened.
peakactvnsts.0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>% 
  group_by(code, species) %>% 
  summarise(peakactvnsts = sum(peak.active)) %>% 
  filter(peakactvnsts == 0) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = peakactvnsts)

peakactvnsts_out <- bind_rows(peakactvnsts.gr0, peakactvnsts.0)

HEPDATA_out <- full_join(effort, peakactvnsts_out)
# 11-13 (-16?) no longer collected, these added with NA at the end ----
# 11   INDIVIDUALS
# 12    FOCALNESTS
# 13    FOCFAILURE
# 14   DISTURBANCE
# 15        SOURCE


# 16         NOTES ----
spp_notes <- screened_hep$notes %>%  
  right_join(screened_sheets) %>% 
  filter(note.type == "for.HEPDATA.species", notes != "", !is.na(notes)) %>% 
  select(CODE = code, SPECIES = species, NOTES.spp = notes) %>% 
  distinct()

col_notes <- screened_hep$notes %>%  
  right_join(screened_sheets) %>% 
  filter(note.type == "for.HEPDATA.colony", notes != "", !is.na(notes)) %>% 
  select(CODE = code, SPECIES = species, NOTES.col = notes) %>% 
  distinct()

# If colony notes are the same as species notes, or colony notes are contained in species notes, then just use species notes, otherwise, paste the 2 together
notes = full_join(spp_notes, col_notes) %>% 
  mutate(NOTES = ifelse(NOTES.spp == NOTES.col | str_detect(NOTES.spp, NOTES.col), NOTES.spp, paste(NOTES.spp, NOTES.col))) %>% 
  select(CODE, SPECIES, NOTES)

HEPDATA_out <- full_join(HEPDATA_out, notes)

# stage 4 brood sizes ----
# 17 BRD1; # 18 BRD2; # 19 BRD3; # 20 BRD4; # 21 BRD5; # 22 BRD6
brood_sizes <- screened_hep$brood.sizes %>%  
  right_join(screened_sheets) %>% 
  data.frame() %>% 
  filter(brd.size.date == TRUE) %>% 
  mutate(brd = paste("BRD", brd, sep = "")) %>% 
  pivot_wider(id_cols = c(code, species, date), values_from = num.nests, names_from = brd)

num_brood_size_days <- brood_sizes %>% 
  count(code, species, date) %>% 
  mutate(out.message = paste(code, species, "\n")) %>% 
  filter(n > 1)



if(nrow(num_brood_size_days) > 0) {
  stop("multiple days selected for brood size for: \n", num_brood_size_days$out.message, "\nPlease edit appropriate Season Summary Sheets")
}
# if there are any records here, need to stop and fix before proceeding. can uncomment the view call to see code X species combos with multiple brood size dates
# fix will likely need to be returning to season summary form and editing the brood size table so only 1 date is selected

brood_sizes <- brood_sizes %>% 
  select(CODE = code, SPECIES = species, contains("BRD"))

HEPDATA_out <- full_join(HEPDATA_out, brood_sizes)

# ROP stage data ----
# 23 MARSTGEDATE; 24 MARSTAGE1; # 25 MARSTAGE2; # 26 MARSTAGE3; # 27 MARSTAGE4; # 28 MARSTAGE5; # 29 LTMARSTGDATE; # 30 LTMARSTAGE1; # 31 LTMARSTAGE2; # 32 LTMARSTAGE3; # 33 LTMARSTAGE4; # 34 LTMARSTAGE5; # 35 APRSTGEDATE; # 36 APRSTAGE1; # 37 APRSTAGE2; # 38 APRSTAGE3; # 39 APRSTAGE4; # 40 APRSTAGE5; # 41 MAYSTGEDATE; # 42 MAYSTAGE1; # 43; MAYSTAGE2; # 44; MAYSTAGE3; # 45 MAYSTAGE4; # 46 MAYSTAGE5; # 47 JUNSTGEDATE; # 48 JUNSTAGE1; # 49 JUNSTAGE2; # 50 JUNSTAGE3; # 51; JUNSTAGE4; # 52 JUNSTAGE5; # 53 LTJUNSTGDATE; # 54 LTJUNSTAGE1; # 55 LTJUNSTAGE2; # 56 LTJUNSTAGE3; # 57 LTJUNSTAGE4; # 58 LTJUNSTAGE5; # 59 JULSTGEDATE; # 60 JULSTAGE1; # 61 JULSTAGE2; # 62 JULSTAGE3; # 63 JULSTAGE4; # 64 JULSTAGE5
rop_names <- data.frame(which.rop = paste("rop.", seq(1, 7), sep = ""),
                        rop.name = c("MAR", "LTMAR", "APR", "MAY", "JUN", "LTJUN", "JUL"),
                        rop.date.name = c("MARSTGEDATE", "LTMARSTGDATE", "APRSTGEDATE", "MAYSTGEDATE", "JUNSTGEDATE", "LTJUNSTGDATE", "JULSTGEDATE"))


all_rop_stages <- screened_hep$stages %>%  
  right_join(screened_sheets) %>% 
  filter(grepl("rop", which.rop)) %>% 
  mutate(stage = paste("STAGE", stage, sep = "")) %>% 
  left_join(., rop_names)

dup_rop <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  count(code, species, rop.stage, num.nests) %>% 
  filter(n > 1) %>% 
  mutate(out.message = paste(code, species, "\n"))

if(nrow(dup_rop) > 0) {
  stop("multiple days selected for the same ROP for: \n", dup_rop$out.message, "\nPlease edit appropriate Season Summary Sheets")
}

rop_nests <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  select(code, species, rop.stage, num.nests) %>% 
  pivot_wider(id_cols = c("code", "species"), names_from = rop.stage, values_from = num.nests)

rop_dates <- all_rop_stages %>% 
  select(code, species, date, rop.date.name) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("code", "species"), names_from = rop.date.name, values_from = date)
  
rop_dates_nests <- full_join(rop_nests, rop_dates) %>% 
  select(CODE = code, SPECIES = species, 
         MARSTGEDATE, MARSTAGE1, MARSTAGE2, MARSTAGE3, MARSTAGE4, MARSTAGE5
         , LTMARSTGDATE, LTMARSTAGE1, LTMARSTAGE2, LTMARSTAGE3, LTMARSTAGE4, LTMARSTAGE5
         , APRSTGEDATE, APRSTAGE1, APRSTAGE2, APRSTAGE3, APRSTAGE4, APRSTAGE5
         , MAYSTGEDATE, MAYSTAGE1, MAYSTAGE2, MAYSTAGE3, MAYSTAGE4, MAYSTAGE5
         , JUNSTGEDATE, JUNSTAGE1, JUNSTAGE2, JUNSTAGE3, JUNSTAGE4, JUNSTAGE5
         , LTJUNSTGDATE, LTJUNSTAGE1, LTJUNSTAGE2, LTJUNSTAGE3, LTJUNSTAGE4, LTJUNSTAGE5
         #, JULSTGEDATE, JULSTAGE1, JULSTAGE2, JULSTAGE3, JULSTAGE4, JULSTAGE5
         )


HEPDATA_out <- full_join(HEPDATA_out, rop_dates_nests)

# disturbances ----
# 65 DIST1DATE; # 66 DIST1TYPE; # 67 DIST1RESULT; # 68 DIST2DATE; # 69 DIST2TYPE; # 70 DIST2RESULT; # 71 DIST3DATE; # 72 DIST3TYPE; # 73 DIST3RESULT; # 74 DIST4DATE; # 75 DIST4TYPE; # 76 DIST4RESULT; # 77 DIST5DATE; # 78 DIST5TYPE; # 79 DIST5RESULT; # 80 DIST6DATE; # 81 DIST6TYPE; # 82 DIST6RESULT; # 83; DIST7DATE; # 84 DIST7TYPE; # 85 DIST7RESULT; # 86 DIST8DATE; # 87 DIST8TYPE; # 88 DIST8RESULT; # 89 DIST9DATE; # 90 DIST9TYPE; # 91 DIST9RESULT

disturbance <- screened_hep$disturbance %>%  
  right_join(screened_sheets) %>% 
  mutate(result = tolower(result)) %>% 
  filter(!grepl("x", type), !grepl("x", result)) %>% 
  group_by(code, species) %>% 
  arrange(code, species, date) %>% 
  mutate(dist.num = row_number()) %>% 
  ungroup() %>% 
  left_join(data.frame(dist.num = seq(1, 8)))

disturbance_long <- disturbance %>% 
  select(code, species, date, type, result, dist.num) %>% 
  pivot_longer(cols = c(date, type, result)) %>% 
  mutate(name = toupper(name),
         outname = paste("DIST", dist.num, name, sep = ""))

disturbance_out <- disturbance_long %>% 
  pivot_wider(id_cols = c(code, species), names_from = outname, values_from = value) %>% 
  rename(CODE = code,
         SPECIES = species) %>% 
  filter(!is.na(CODE))


HEPDATA_out <- full_join(HEPDATA_out, disturbance_out)

# predators ----
# 92   GHOWNESTING
# 93   RTHANESTING
# 94   OSPRNESTING
# 95   CORANESTING
# 96   BAEANESTING
# 97  TUVUROOSTING



nesting_predators <- screened_hep$predators %>%  
  right_join(screened_sheets) %>% 
  filter((nesting == 1 | tolower(copy.to.non.nesters) == "yes"), predator.species %in% c("GHOW", "RTHA", "OSPR", "CORA", "BAEA", "TUVU")) %>% 
  mutate(predator = ifelse(predator.species == "TUVU", 
                           paste(toupper(predator.species), "ROOSTING", sep = ""),
                           paste(toupper(predator.species), "NESTING", sep = ""))) %>% 
  distinct(code, species, predator, nesting)
  

predators <- nesting_predators %>% 
  pivot_wider(id_cols = c(code, species), names_from = predator, values_from = nesting) %>% 
  rename(CODE = code,
         SPECIES = species) 
  


HEPDATA_out <- full_join(HEPDATA_out, predators)
# 98 Entry_Proofed
# 99    Entered_By

hepdata_names <- readRDS("data/support_data/HEPDATA_names") %>% 
              data.frame() %>% 
              rename(colname = 1) %>% 
              mutate(blah = NA) %>% 
              pivot_wider(values_from = blah, names_from = colname)

all_colony_species <- expand.grid(code = distinct(HEPDATA_out, CODE)$CODE,
                                  species = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO"))

colony_notes<- HEPDATA_out %>% distinct(CODE, NOTES) %>% 
  filter(!is.na(NOTES)) %>% 
  group_by(CODE) %>% 
  summarise(NOTES = paste(NOTES, collapse = ". "))

colony_obs_effort <- effort %>% 
  filter(!(CODE == 70 & SPECIES %in% c("BCNH", "SNEG"))) %>% # only want to use ACR effort from non-nesting records
  select(CODE, NUMBERVISITS, TOTALHOURS) %>% 
  distinct() %>% 
  filter(!is.na(NUMBERVISITS))

colony_disturbance <- disturbance %>%
  select(-species) %>% 
  filter(!is.na(date)) %>% 
  select(code, date, type, result) %>%
  distinct() %>% 
  arrange(code, date) %>%
  group_by(code) %>% 
  mutate(dist.num = row_number()) %>% 
  pivot_longer(cols = c(date, type, result)) %>% 
  mutate(name = toupper(name),
         outname = paste("DIST", dist.num, name, sep = "")) %>% 
  pivot_wider(id_cols = c(code), names_from = outname, values_from = value) %>% 
  rename(CODE = code) %>% 
  filter_at(vars(contains("DIST")), any_vars(!is.na(.)))

colony_predators <- predators %>%  
  select(-SPECIES) %>% 
  distinct() %>% 
  filter_at(vars(contains("NESTING")), any_vars(!is.na(.)))


active_colony_non_nesters <- all_colony_species %>% 
  fix_alc_code() %>%  
  rename(CODE = code, SPECIES = species) %>%
  anti_join(distinct(HEPDATA_out, CODE, SPECIES)) %>% 
  left_join(., colony_notes) %>% 
  left_join(., colony_obs_effort) %>%
  left_join(., colony_disturbance) %>%
  left_join(., colony_predators) %>%   
  mutate(PEAKACTVNSTS = 0,
         Entry_Proofed = "",
         Entered_By = paste("code-generated non-nesting record.", Sys.Date())) 


HEPDATA_out <- HEPDATA_out %>% 
  mutate(Entry_Proofed = "",
         Entered_By = paste("code-generated record.", Sys.Date())) %>% 
  filter(!is.na(CODE)) %>% 
  bind_rows(., active_colony_non_nesters) %>%
  left_join(., readRDS(here("data/support_data/HEP_site_names_nums_utm")) %>% 
              select(CODE = code, SITE = site.name)) %>%  
  mutate(YEAR = zyear) %>% 
  full_join(., hepdata_names) %>% 
  select(names(hepdata_names), PEAKACTVDATE) %>% 
  relocate(PEAKACTVDATE, .after = PEAKACTVNSTS) %>% 
  filter(!is.na(CODE))

HEPDATA_out <- HEPDATA_out %>% 
  mutate(across(everything(), ~as.character(.)),
         across(contains("DATE"), ~as.POSIXct(.)),
         across(matches("RESULT|NESTING|STAGE|BRD|YEAR|CODE"), ~as.numeric(.)),
         across(contains("TYPE"), ~as.character(.)),
         across(c("PEAKRICHNESS", "TOTALSPECIES", "NUMBERVISITS", "TOTALHOURS", "PEAKACTVNSTS", "INDIVIDUALS", "FOCALNESTS", "FOCFAILURE"), ~as.numeric(.)))



return(HEPDATA_out)
}


# HEPDATA <- screened_to_HEPDATA(zyear)

###
# deprecated
screened_to_HEPDATA_inactive_colonies <- function(zyear, active_col_hepdata) {

  # active_col_hepdata = HEPDATA_active

    
active_cols <- active_col_hepdata %>% 
  distinct(CODE, SPECIES) %>% 
  rename(code = CODE, species = SPECIES)
  
  

# these are the inactive colony X species combos that did not have a summary sheet made or screened, so need to access the wrangled (unscreeened) version of the data.
wrangled_hep <- readRDS(here(paste("data/wrangled/wrangled_raw_", zyear, sep = "")))
wrangled_hep <- map(wrangled_hep, fix_alc_code)


# 1         DATAID
# 2 YEAR; # 3 SITE; # 4 CODE; # 5 SPECIES
# 2 added based on zyear above. 4, 5 are in every table, will be used to merge all tables, 3 added from saved site name file ----
# 6 and 7 not consistently filled in current HEPDATA, these filled with NA at the end ----
# 6   PEAKRICHNESS
# 7   TOTALSPECIES
#8 and 9 come from effort_summary ----
# 8   NUMBERVISITS
# 9     TOTALHOURS
colony_effort <- wrangled_hep$observers.effort 

if('species' %in% colnames(colony_effort)){
colony_effort <- colony_effort  %>% 
  select(-species)
}

colony_effort <- colony_effort %>% 
  anti_join(active_cols) %>% 
  distinct(code, total.surveys, total.hours) %>% 
  select(CODE = code, NUMBERVISITS = total.surveys, TOTALHOURS = total.hours)

# 10  PEAKACTVNSTS ----
# 16         NOTES ----

colony_notes <- wrangled_hep$notes %>%  
  select(-species) %>% 
  anti_join(active_cols) %>% 
  select(CODE = code, NOTES.col = notes) %>% 
  distinct() %>% 
  group_by(CODE) %>% 
  summarise(NOTES = paste(NOTES.col, collapse = ". "))


# disturbances ----
# 65 DIST1DATE; # 66 DIST1TYPE; # 67 DIST1RESULT; # 68 DIST2DATE; # 69 DIST2TYPE; # 70 DIST2RESULT; # 71 DIST3DATE; # 72 DIST3TYPE; # 73 DIST3RESULT; # 74 DIST4DATE; # 75 DIST4TYPE; # 76 DIST4RESULT; # 77 DIST5DATE; # 78 DIST5TYPE; # 79 DIST5RESULT; # 80 DIST6DATE; # 81 DIST6TYPE; # 82 DIST6RESULT; # 83; DIST7DATE; # 84 DIST7TYPE; # 85 DIST7RESULT; # 86 DIST8DATE; # 87 DIST8TYPE; # 88 DIST8RESULT; # 89 DIST9DATE; # 90 DIST9TYPE; # 91 DIST9RESULT

colony_disturbance <- wrangled_hep$disturbance %>%  
  anti_join(active_cols) %>% 
  mutate(result = tolower(result)) %>% 
  filter(!grepl("x", type), !grepl("x", result), !is.na(date)) %>% 
  select(-species) %>% 
  distinct() %>% 
  arrange(code, date) %>%
  group_by(code) %>%  
  mutate(dist.num = row_number()) %>% 
  ungroup() 

colony_disturbance_long <- colony_disturbance %>% 
  select(code, date, type, result, dist.num) %>%
  mutate(date = as.character(date)) %>% 
  pivot_longer(cols = c(date, type, result)) %>% 
  mutate(name = toupper(name),
         outname = paste("DIST", dist.num, name, sep = ""))

colony_disturbance_out <- colony_disturbance_long %>% 
  pivot_wider(id_cols = c(code), names_from = outname, values_from = value) %>% 
  rename(CODE = code) %>% 
  filter(!is.na(CODE))



# predators ----
# 92   GHOWNESTING
# 93   RTHANESTING
# 94   OSPRNESTING
# 95   CORANESTING
# 96   BAEANESTING
# 97  TUVUROOSTING



colony_nesting_predators <- wrangled_hep$predators %>%  
  select(-species) %>% 
  anti_join(active_cols) %>% 
  filter(nesting == 1, predator.species %in% c("GHOW", "RTHA", "OSPR", "CORA", "BAEA", "TUVU")) %>% 
  mutate(predator = ifelse(predator.species == "TUVU", 
                           paste(toupper(predator.species), "ROOSTING", sep = ""),
                           paste(toupper(predator.species), "NESTING", sep = ""))) %>% 
  distinct(code, predator, nesting) %>% 
  pivot_wider(id_cols = c(code), names_from = predator, values_from = nesting) %>% 
  rename(CODE = code) 
  

# 98 Entry_Proofed
# 99    Entered_By

hepdata_names <- readRDS("data/HEPDATA_names") %>% 
              data.frame() %>% 
              rename(colname = 1) %>% 
              mutate(blah = NA) %>% 
              pivot_wider(values_from = blah, names_from = colname)


HEPDATA_out <- expand.grid(CODE = distinct(colony_effort, CODE)$CODE,
                           SPECIES = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO")) %>% 
  full_join(., colony_effort) %>%
  full_join(., colony_notes) %>% 
  full_join(., colony_disturbance_out) %>%
  full_join(., colony_nesting_predators) %>% 
  mutate(PEAKACTVNSTS = 0,
         Entry_Proofed = "",
         Entered_By = paste("code-generated non-nesting record.", Sys.Date())) %>% 
  left_join(., readRDS(here("data/HEP_site_names_nums_utm")) %>% 
              select(CODE = code, SITE = site.name)) %>% 
  mutate(YEAR = zyear) %>% 
  full_join(., hepdata_names) %>% 
  select(names(hepdata_names)) %>% 
  filter(!is.na(CODE))


HEPDATA_out <- HEPDATA_out %>% 
  mutate(across(everything(), ~as.character(.)),
         across(contains("DATE"), ~as.POSIXct(.)),
         across(matches("RESULT|NESTING|STAGE|BRD|YEAR|CODE"), ~as.numeric(.)),
         across(contains("TYPE"), ~as.character(.)),
         across(c("PEAKRICHNESS", "TOTALSPECIES", "NUMBERVISITS", "TOTALHOURS", "PEAKACTVNSTS", "INDIVIDUALS", "FOCALNESTS", "FOCFAILURE"), ~as.numeric(.)))


return(HEPDATA_out)
}


###
