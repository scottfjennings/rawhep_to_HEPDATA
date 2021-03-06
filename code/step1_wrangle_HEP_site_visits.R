
# load the required packages
library(RODBC)

#' load HEP_site_visit_data 
#'
#' @param HEP_site_visits_location file path for the access database
#'
#' @return
#' @export
#'
#' @examples hep_site_visits <- hep_site_visits_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_screening_focal/HEP_site_visit_data.accdb")
hep_site_visits_from_access <- function(HEP_site_visits_location = HEP_site_visits_location){
  if(is.na(HEP_site_visits_location)){
    print("Please assign location of HEPDATA access file to HEP_site_visits_location")
  } else {
    db <- HEP_site_visits_location
  }
 
 con2 <- odbcConnectAccess2007(db)

hep_site_visits <- list(front1 = sqlFetch(con2, "HEP_site_visit_front_1"),
                        front2 = sqlFetch(con2, "HEP_site_visit_front_2"),
                        back = sqlFetch(con2, "HEP_site_visit_back"),
                        predators = sqlFetch(con2, "Predators")) 
 
close(con2)

return(hep_site_visits)
}





#' Wrangle HEP site visits data
#'
#' @param site_visits hep_site_visits data as a list with 4 elements for the 4 main tables in HEP_site_visit_data: front1, front2, back, and predators. use hep_site_visits_from_access to generate this list
#' @param col_codes character string of colony codes to include
#'
#' @return a list with 8 elements, matching the structure of the other wrangled_ lists (e.g., wrangled_S123)
#' @export
#'
#' @examples wrangled_site_visits <- wrangle_HEP_site_visits(hep_site_visits)
wrangle_HEP_site_visits <- function(site_visits, col_codes) {

front1_wrangled = hep_site_visits$front1 %>% 
  filter(year(Date) == zyear) %>% 
  rename("date" = Date, "code" = CODE, "start" = StartTime, "end" = EndTime) %>% 
  mutate(across(c(start, end), ~as.POSIXct(paste(date, paste(hour(.), minute(.), second(.), sep = ":")))))
  
sheets <- distinct(front1_wrangled, SheetNum, code, date)

# now converting to wrangled format ----
# first the dates table ----
# "date"
# "code"
# "multiple.survey.num"
# "start"
# "end"
# "num.surveys.this.date"
# "complete.count" 

dates = front1_wrangled %>% 
  select(date, code, start, end) %>%
  add_multiple_survey_num() %>% 
  mutate(complete.count = "yes")

# then observers.effort ----
# "code"
# "colony"
# "observers"
# "total.days"
# "total.surveys"
# "total.hours"

# number of columns to split ObsInitial into
max.initials = max(str_count(front1_wrangled$ObsInitial, ','))


observers.effort = front1_wrangled %>% 
  #left_join(readRDS(here("data/HEP_site_names_nums_utm")), by = "code") %>%
  separate(ObsInitial, into = paste("obs", seq(1, 1+max.initials))) %>% 
  select(code, contains("obs")) %>% 
  pivot_longer(cols = contains("obs"), values_to = "obs") %>% 
  distinct(code, obs) %>% 
  filter(!is.na(obs)) %>% 
  group_by(code) %>% 
  summarise(observers = paste(obs, collapse = ", ")) %>% 
  left_join(., dates %>% 
  mutate(effort.hours = (end - start)/60,
         effort.hours = as.numeric(effort.hours)) %>% 
  group_by(code) %>% 
  summarise(total.hours = sum(effort.hours),
            total.hours = round(total.hours, 2),
            total.surveys = n(), .groups = "drop")) %>% 
  left_join(., dates %>% 
              distinct(code, date) %>% 
              group_by(code) %>% 
              summarise(total.days = n())) %>% 
  left_join(readRDS(here("data/HEP_site_names_nums_utm")), by = "code") %>% 
  select(code, colony = site.name, observers, total.days, total.surveys, total.hours) %>% 
  filter(code %in% col_codes)
  
  
# nests ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "total.nests"
# "complete.count"
# "peak.active"
# "obs.initials"

# some dates have total nests entered in the front 2 sheet, others just have the individual-nest data on the back. need to compare and take whichever is greater 
front_nests <- hep_site_visits$front2 %>% 
  select(species = SpeciesCode, total.nests.front = Active, SheetNum) %>% 
  right_join(., front1_wrangled %>% select(date, SheetNum, obs.initials = ObsInitial, code))

back_nests <- hep_site_visits$back %>% 
  select(species = SpeciesCode, Status, SheetNum) %>% 
  right_join(., front1_wrangled %>% select(SheetNum, obs.initials = ObsInitial, date, code)) %>% 
  filter(Status == "A") %>% 
  group_by(code, date, species, SheetNum) %>% 
  summarise(total.nests.back = n()) %>% 
  ungroup()

nests <- full_join(front_nests, back_nests) %>% 
  mutate(across(contains("nests"), ~replace_na(., 0)),
         total.nests = ifelse(total.nests.back > total.nests.front, total.nests.back, total.nests.front)) %>% 
  select(code, date, species, total.nests) %>% 
  filter(!is.na(species)) %>% 
  left_join(., dates %>% select(code, date, multiple.survey.num, complete.count)) %>% 
  left_join(., front1_wrangled %>% select(date, code, obs.initials = ObsInitial)) %>% 
  group_by(code, species) %>% 
  mutate(peak.active = ifelse(total.nests == max(total.nests), 1, 0)) %>% 
  ungroup() %>% 
  arrange(code, species, date) %>% 
  filter(code %in% col_codes)

# stages ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "num.nests"
# "stage"
# "complete.count"
# "which.rop"

stages <- hep_site_visits$back %>% 
  filter(Status == "A", between(Stage, 1, 5)) %>% 
  group_by(SheetNum, SpeciesCode, Stage) %>% 
  summarise(num.nests = n()) %>% 
  ungroup() %>% 
  rename(species = SpeciesCode, stage = Stage) %>% 
  left_join(., front1_wrangled %>% select(date, code, obs.initial = ObsInitial, SheetNum)) %>% 
  left_join(., dates %>% select(code, date, multiple.survey.num, complete.count)) %>% 
  filter(year(date) == zyear) %>% 
  full_join(., dates %>% 
              mutate(date = ymd(date)) %>%
              assign_rop(rop_dates = read.csv("data/rop_dates.csv"))) %>% 
  select(code, date, multiple.survey.num, species, num.nests, stage, complete.count, which.rop) %>% 
  mutate(which.rop = replace_na(which.rop, "other"),
         stage = as.character(stage)) %>% 
  filter(code %in% col_codes)

# brood.sizes ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "num.nests"
# "brd"
# "brd.size.date"
# "stage5.nests"


stage4_5 <- hep_site_visits$back %>%
  right_join(sheets) %>% # filters to zyear 
  left_join(dates) %>% 
  rename(species = SpeciesCode, stage = Stage, chicks = Chicks) %>%  
  filter(Status == "A", between(stage, 4, 5)) %>% 
  group_by(code, date, multiple.survey.num, species, stage, chicks) %>%
  summarise(num.nests = n()) %>% 
  ungroup()



stage5 <- stage4_5 %>% 
  filter(stage == 5 & num.nests > 0) %>% 
  distinct(code, species, date) %>% 
  mutate(stage5.nests = TRUE)

brood.sizes <- stage4_5 %>%
  filter(stage == 4) %>% 
  select(code, date, multiple.survey.num, species, num.nests, brd = chicks) %>% 
  group_by(code, species) %>% 
  mutate(brd.size.date = ifelse(date == max(date), TRUE, NA),
         brd = as.character(brd)) %>% 
  left_join(stage5, by = c("code", "date", "species")) %>% 
  arrange(code, species, date) %>% 
  filter(code %in% col_codes)


# predators ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "predator.species"
# "present"
# "nesting"

predators <- hep_site_visits$predators %>% 
  inner_join(sheets) %>% 
  left_join(., distinct(nests, code, date, species)) %>%
  left_join(., dates) %>% 
  mutate(present = 1,
         nesting = "",
         present = as.double(present),
         nesting = as.double(nesting)) %>% 
  select(code, date, multiple.survey.num, species, predator.species = PredatorSpecies, present, nesting) %>% 
  filter(code %in% col_codes) %>% 
  distinct(code, species, predator.species, present, nesting)

# disturbance ----
# disturbance data not collected in site_visits, need to manually check notes in season summary sheet and copy to the disturbance table
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "obs.inf"
# "type"
# "result"
# "description"

disturbance <- nests %>% 
  filter(total.nests > 0) %>% 
  distinct(code, species) %>% 
  mutate(date = NA,
         multiple.survey.num = "",
         obs.inf = "",
         type = "",
         result = "",
         description = "",
         date = as.Date(date)) %>% 
  filter(code %in% col_codes)

# notes ----
# "code"
# "date"
# "multiple.survey.num"
# "species"
# "note.type"
# "notes"


notes <- nests %>%  
  distinct(code, species) %>% 
  full_join(front1_wrangled) %>% 
  #add_multiple_survey_num() %>% 
  full_join(., dates) %>% 
  mutate(note.type = "field") %>% 
  select(code, date, multiple.survey.num, species, note.type, notes = Notes) %>% 
  filter(!is.na(notes)) %>% 
  filter(code %in% col_codes)


# combine ----

wrangled_site_visits <- list(dates = dates,
                             observers.effort = observers.effort,
                             nests = nests,
                             stages = stages,
                             brood.sizes = brood.sizes,
                             predators = predators,
                             disturbance = disturbance,
                             notes = notes)

return(wrangled_site_visits)
}




