

# convert screened data to HEPDATA format
# screened data are saved as several dfs that are bundled in a list. Data for the various HEPDATA columns are sourced from the appropriate elements (dfs) of this list

# this is one big function

# hepdata_names <- readRDS("data/HEPDATA_names")
# we need to expand out to all possible columns here

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
peakactvnsts.gr0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>%  
  filter(peak.active == 1) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = total.nests)

# this is all the colony X species combos that were not actively nesting but for which the season summary sheet was screened.
peakactvnsts.0 <- screened_hep$nests %>% 
  right_join(screened_sheets) %>% 
  group_by(code, species) %>% 
  summarise(peakactvnsts = sum(peak.active)) %>% 
  filter(peakactvnsts == 0) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = peakactvnsts)

peakactvnsts_out <- rbind(peakactvnsts.gr0, peakactvnsts.0)

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
  full_join(data.frame(dist.num = seq(1, 8)))

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

hepdata_names <- readRDS("data/HEPDATA_names") %>% 
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
  left_join(., readRDS(here("data/HEP_site_names_nums_utm")) %>% 
              select(CODE = code, SITE = site.name)) %>% 
  mutate(YEAR = zyear) %>% 
  bind_rows(., active_colony_non_nesters) %>% 
  full_join(., hepdata_names) %>% 
  select(names(hepdata_names)) %>% 
  filter(!is.na(CODE))

# add non-nesting records

return(HEPDATA_out)
}


# HEPDATA <- screened_to_HEPDATA(zyear)

##################################################################################
##################################################################################
##################################################################################
##################################################################################

screened_to_HEPDATA_inactive_colonies <- function(zyear) {


    
screened_sheets <- get_screened_col_spp(zyear) %>% 
    fix_alc_code() %>% 
    select(-screened) %>% 
    mutate(code = as.numeric(code))
  
  


wrangled_hep <- readRDS(here(paste("data/wrangled/wrangled_hep_", zyear, sep = "")))
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
effort <- screened_hep$observers.effort %>% 
  right_join(screened_sheets) %>% 
  select(CODE = code, SPECIES = species, NUMBERVISITS = total.surveys, TOTALHOURS = total.hours)

# 10  PEAKACTVNSTS ----
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
  full_join(data.frame(dist.num = seq(1, 8)))

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
  distinct(code, predator, nesting)
  

predators <- expand.grid(code = distinct(nesting_predators, code)$code,
                        species = c("BCNH", "CAEG", "GREG", "SNEG", "GBHE", "DCCO")) %>%
  full_join(., nesting_predators) %>% 
  pivot_wider(id_cols = c(code, species), names_from = predator, values_from = nesting) %>% 
  rename(CODE = code,
         SPECIES = species) 
  


HEPDATA_out <- full_join(HEPDATA_out, predators)
# 98 Entry_Proofed
# 99    Entered_By

hepdata_names <- readRDS("data/HEPDATA_names") %>% 
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
  filter(!(CODE == 70 & SPECIES %in% c("BCNH", "SNEG"))) %>% 
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

colony_predators <- HEPDATA_out %>% 
  select(CODE, contains("NESTING")) %>% 
  distinct() %>% 
  filter_at(vars(contains("NESTING")), any_vars(!is.na(.)))

active_colony_non_nesters <- all_colony_species %>% 
  fix_alc_code() %>%  
  anti_join(screened_sheets) %>% 
  rename(CODE = code, SPECIES = species) %>%
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
  left_join(., readRDS(here("data/HEP_site_names_nums_utm")) %>% 
              select(CODE = code, SITE = site.name)) %>% 
  mutate(YEAR = zyear) %>% 
  full_join(., hepdata_names) %>% 
  select(names(hepdata_names)) %>% 
  filter(!is.na(CODE))

# add non-nesting records

return(HEPDATA_out)
}
