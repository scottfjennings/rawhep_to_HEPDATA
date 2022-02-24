

# convert screened data to HEPDATA format
# screened data are saved as several dfs that are bundled in a list. Data for the various HEPDATA columns are sourced from the appropriate elements (dfs) of this list

# this is one big function

# hepdata_names <- readRDS("data/HEPDATA_names")
# we need to expand out to all possible columns here

screened_to_HEPDATA <- function(zyear) {
  
  screened_sheets <- get_screened_col_spp(zyear) %>% 
    select(-screened) %>% 
    mutate(code = as.numeric(code))
  
  
screened_s123 <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))


# 1         DATAID
# 2 YEAR; # 3 SITE; # 4 CODE; # 5 SPECIES
# 2 added based on zyear above. 4, 5 are in every table, will be used to merge all tables, 3 added from saved site name file ----
# 6 and 7 not consistently filled in current HEPDATA, these filled with NA at the end ----
# 6   PEAKRICHNESS
# 7   TOTALSPECIES
#8 and 9 come from effort_summary ----
# 8   NUMBERVISITS
# 9     TOTALHOURS
effort <- screened_s123$observers.effort %>% 
  right_join(screened_sheets) %>% 
  left_join(., readRDS(here("data/HEP_site_names_nums_utm")) %>% 
              select(code, site.name)) %>% 
  select(SITE = site.name, CODE = code, SPECIES = species, NUMBERVISITS = total.surveys, TOTALHOURS = total.hours) %>% 
  mutate(YEAR = zyear) 

# 10  PEAKACTVNSTS ----
# this is all the colony X species combos that were really active
peakactvnsts.gr0 <- screened_s123$nests %>% 
  right_join(screened_sheets) %>%  
  filter(peak.active == 1) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = total.nests) %>% 
  mutate(YEAR = zyear)

# this is all the colony X species combos that were not actively nesting but for which the season summary sheet was screened.
peakactvnsts.0 <- screened_s123$nests %>% 
  right_join(screened_sheets) %>% 
  group_by(code, species) %>% 
  summarise(peakactvnsts = sum(peak.active)) %>% 
  filter(peakactvnsts == 0) %>% 
  select(CODE = code, SPECIES = species, PEAKACTVNSTS = peakactvnsts) %>% 
  mutate(YEAR = zyear)

peakactvnsts_out <- rbind(peakactvnsts.gr0, peakactvnsts.0)

HEPDATA_out <- full_join(effort, peakactvnsts_out)
# 11-13 (-16?) no longer collected, these added with NA at the end ----
# 11   INDIVIDUALS
# 12    FOCALNESTS
# 13    FOCFAILURE
# 14   DISTURBANCE
# 15        SOURCE


# 16         NOTES ----
notes <- screened_s123$notes %>%  
  right_join(screened_sheets) %>% 
  filter(note.type == "for.HEPDATA", notes != "", !is.na(notes)) %>% 
  select(CODE = code, SPECIES = species, NOTES = notes) %>% 
  mutate(YEAR = zyear)

HEPDATA_out <- full_join(HEPDATA_out, notes)

# stage 4 brood sizes ----
# 17 BRD1; # 18 BRD2; # 19 BRD3; # 20 BRD4; # 21 BRD5; # 22 BRD6
brood_sizes <- screened_s123$brood.sizes %>%  
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
  select(CODE = code, SPECIES = species, contains("BRD")) %>% 
  mutate(YEAR = zyear)

HEPDATA_out <- full_join(HEPDATA_out, brood_sizes)

# ROP stage data ----
# 23 MARSTGEDATE; 24 MARSTAGE1; # 25 MARSTAGE2; # 26 MARSTAGE3; # 27 MARSTAGE4; # 28 MARSTAGE5; # 29 LTMARSTGDATE; # 30 LTMARSTAGE1; # 31 LTMARSTAGE2; # 32 LTMARSTAGE3; # 33 LTMARSTAGE4; # 34 LTMARSTAGE5; # 35 APRSTGEDATE; # 36 APRSTAGE1; # 37 APRSTAGE2; # 38 APRSTAGE3; # 39 APRSTAGE4; # 40 APRSTAGE5; # 41 MAYSTGEDATE; # 42 MAYSTAGE1; # 43; MAYSTAGE2; # 44; MAYSTAGE3; # 45 MAYSTAGE4; # 46 MAYSTAGE5; # 47 JUNSTGEDATE; # 48 JUNSTAGE1; # 49 JUNSTAGE2; # 50 JUNSTAGE3; # 51; JUNSTAGE4; # 52 JUNSTAGE5; # 53 LTJUNSTGDATE; # 54 LTJUNSTAGE1; # 55 LTJUNSTAGE2; # 56 LTJUNSTAGE3; # 57 LTJUNSTAGE4; # 58 LTJUNSTAGE5; # 59 JULSTGEDATE; # 60 JULSTAGE1; # 61 JULSTAGE2; # 62 JULSTAGE3; # 63 JULSTAGE4; # 64 JULSTAGE5
rop_names <- data.frame(which.rop = paste("rop.", seq(1, 7), sep = ""),
                        rop.name = c("MAR", "LTMAR", "APR", "MAY", "JUN", "LTJUN", "JUL"),
                        rop.date.name = c("MARSTGEDATE", "LTMARSTGDATE", "APRSTGEDATE", "MAYSTGEDATE", "JUNSTGEDATE", "LTJUNSTGDATE", "JULSTGEDATE"))


all_rop_stages <- screened_s123$stages %>%  
  right_join(screened_sheets) %>% 
  filter(grepl("rop", which.rop)) %>% 
  mutate(stage = paste("STAGE", stage, sep = ""),
         YEAR = zyear) %>% 
  left_join(., rop_names)

dup_rop <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  count(YEAR, code, species, rop.stage, num.nests) %>% 
  filter(n > 1) %>% 
  mutate(out.message = paste(code, species, "\n"))
if(nrow(num_brood_size_days) > 0) {
  stop("multiple days selected for the same ROP for: \n", dup_rop$out.message, "\nPlease edit appropriate Season Summary Sheets")
}

rop_nests <- all_rop_stages %>% 
  mutate(rop.stage = paste(rop.name, stage, sep = "")) %>% 
  select(YEAR, code, species, rop.stage, num.nests) %>% 
  pivot_wider(id_cols = c("YEAR", "code", "species"), names_from = rop.stage, values_from = num.nests)

rop_dates <- all_rop_stages %>% 
  select(YEAR, code, species, date, rop.date.name) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("YEAR", "code", "species"), names_from = rop.date.name, values_from = date)
  
rop_dates_nests <- full_join(rop_nests, rop_dates) %>% 
  select(YEAR, CODE = code, SPECIES = species, 
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

disturbance <- screened_s123$disturbance %>%  
  right_join(screened_sheets) %>% 
  mutate(result = tolower(result)) %>% 
  filter(!grepl("x", result)) %>% 
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
  mutate(YEAR = zyear) %>% 
  filter(!is.na(CODE))


HEPDATA_out <- full_join(HEPDATA_out, disturbance_out)

# predators ----
# 92   GHOWNESTING
# 93   RTHANESTING
# 94   OSPRNESTING
# 95   CORANESTING
# 96   BAEANESTING
# 97  TUVUROOSTING
predators <- screened_s123$predators %>%  
  right_join(screened_sheets) %>% 
  filter(nesting == 1, predator.species %in% c("GHOW", "RTHA", "OSPR", "CORA", "BAEA", "TUVU")) %>% 
  mutate(predator = ifelse(predator.species == "TUVU", 
                           paste(toupper(predator.species), "ROOSTING", sep = ""),
                           paste(toupper(predator.species), "NESTING", sep = ""))) %>% 
  pivot_wider(id_cols = c(code, species), names_from = predator, values_from = nesting) %>% 
  rename(CODE = code,
         SPECIES = species) 
  


# 98 Entry_Proofed
# 99    Entered_By


hepdata_names <- readRDS("data/HEPDATA_names") %>% 
              data.frame() %>% 
              rename(colname = 1) %>% 
              mutate(blah = NA) %>% 
              pivot_wider(values_from = blah, names_from = colname)

HEPDATA_out <- full_join(HEPDATA_out, predators) %>% 
  mutate(Entry_Proofed = "",
         Entered_By = paste("code-generated record.", Sys.Date())) %>% 
  full_join(., hepdata_names) %>% 
  filter(!is.na(CODE)) %>% 
  select(names(hepdata_names))

return(HEPDATA_out)
}


# HEPDATA <- screened_to_HEPDATA(zyear)
