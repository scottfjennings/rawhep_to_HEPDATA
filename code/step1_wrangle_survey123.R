


# requires tidyverse; lubridate
# requires rawhep_to_HEPDATA_utility_functions.R


# 
# read, create data ----

add_2020_test_data <- function(s123) {
# test data made with make_test_data.R
 s123 <- rbind(s123, readRDS(here("data/downloaded/test_data")))

}

# code to add test data site info
# if test data are not added to s_123 in the above read function, then test site data are ignored at left_join where observers_effort created below


#' Fix Survey123 field names
#'
#' Field names in Survey123 are attrocious. The first step is to make them more usable.
#'
#' @param s123 data frame of imported .csv which has been downloaded from Survey 123.
#'
#' @return data frame with same number of rows but fewer columns as s123
#' @export
#'
#' @examples # generally called in a pipeline with other functions defined here:
#'  wrangled_s123 <- read.csv(here(paste("data/downloaded/HEP_", zyear, "_", zversion, ".csv", sep = ""))) %>%
#'  #   mutate(complete.count = NA) %>% # only do this if wrangling 2020 data!!!
#'  mutate(useforsummary = tolower(useforsummary)) %>%
#'  filter(useforsummary == "y") %>% #
#'  fix_s123_names() %>%
#'  fix_s123_date_fields() %>%
#'  add_multiple_survey_num() %>%
#'  wrangle_s123()
#'  
fix_s123_names <- function(s123) {
colnames(s123) <- gsub("Please.describe.why.you.were.unable.to.complete.colony.count..and.list.which.species.weren.t.adequately.counted.", "incomplete.count.notes", colnames(s123))

colnames(s123) <- gsub("Did.you.make.a.complete.count.of.all.nests.you.could.find.for.the.6.species.above.within.100m.of.the.colony.", "complete.count", colnames(s123))
                       
s123 <- s123 %>%   
  select(-starts_with(c("X.", "Tally.", "A.nest.is.active", "Re.locate.the", "Please", "Really", "You.have", "in.future", "are.you.able"))) %>% # drop fields that are really just instructions in the survey
  rename(code = Select.Colony, start = Start.Time, end = End.Time,
         field.notes = Any.other.notes.belong.here.,
         predators.present = Did.you.see.any.potential.nest.predators.within.100m.of.the.colony.,
         predator.species = Which.predator.species.did.you.see.,
         other.predator.species = Which.predator.species.not.listed.did.you.see.,
         predators.nesting = Were.any.potential.predator.species.nesting.within.100m.of.the.colony.,
         predator.species.nesting = Which.species.were.nesting.within.100m.of.the.colony.) 

names(s123) <- tolower(names(s123))

names(s123) <- gsub("great.egret", "greg", names(s123))
names(s123) <- gsub("great.blue.heron", "gbhe", names(s123))
names(s123) <- gsub("snowy.egret", "sneg", names(s123))
names(s123) <- gsub("black.crowned.night.heron", "bcnh", names(s123))
names(s123) <- gsub("cattle.egret", "caeg", names(s123))
names(s123) <- gsub("double.crested.cormorant", "dcco", names(s123))



return(s123)
}

#s123 <- fix_s123_names(s123)

#' Fix time zone to pacific
#'
#' @param s123 data frame with start, end and date fields, generally the output of fix_s123_names()  
#' @param zformat time format of the start, end and date fields. If left blank, default is "%m/%d/%Y %I:%M:%S %p"
#'
#' @return
#' @export
#'
#' @examples # generally called in a pipeline with other functions defined here:
#'  wrangled_s123 <- read.csv(here(paste("data/downloaded/HEP_", zyear, "_", zversion, ".csv", sep = ""))) %>%
#'  #   mutate(complete.count = NA) %>% # only do this if wrangling 2020 data!!!
#'  mutate(useforsummary = tolower(useforsummary)) %>%
#'  filter(useforsummary == "y") %>% #
#'  fix_s123_names() %>%
#'  fix_s123_date_fields() %>%
#'  add_multiple_survey_num() %>%
#'  wrangle_s123()
fix_s123_date_fields <- function(s123, zformat = "%m/%d/%Y %I:%M:%S %p") {

  
  if(zyear <= 2021) {
  s123 <- s123 %>% 
  mutate(start = as.POSIXct(start, format = zformat, tz = "GMT"),
         end = as.POSIXct(end, format = zformat, tz = "GMT")) %>% 
  mutate(start = with_tz(start, Sys.timezone(location = TRUE)),
         end = with_tz(end, Sys.timezone(location = TRUE)),
         date = as.Date(start, tz = Sys.timezone(location = TRUE)))
  } else {
      s123 <- s123 %>% 
  mutate(date = as.Date(date, format = zformat, tz = "GMT"),
         start = as.POSIXct(paste(date, start), format = "%Y-%m-%d %H:%M"),
         end = as.POSIXct(paste(date, end), format = "%Y-%m-%d %H:%M"),
         start = with_tz(start, Sys.timezone(location = TRUE)),
         end = with_tz(end, Sys.timezone(location = TRUE)))
  }
}





#' Wrangle survey123 to more usable structure 
#'
#' @param s123 data frame with cleaned field names, generally output of fix_s123_names() %>% fix_s123_date_fields() %>% add_multiple_survey_num()
#'
#' @return a list with 8 elements to match the other wrangled_ objects: dates, observers.effort, nests, stages, brood.sizes, predators, disturbance, notes
#' @export
#'
#' @examples # generally called in a pipeline with other functions defined here:
#'  wrangled_s123 <- read.csv(here(paste("data/downloaded/HEP_", zyear, "_", zversion, ".csv", sep = ""))) %>%
#'  #   mutate(complete.count = NA) %>% # only do this if wrangling 2020 data!!!
#'  mutate(useforsummary = tolower(useforsummary)) %>%
#'  filter(useforsummary == "y") %>% #
#'  fix_s123_names() %>%
#'  fix_s123_date_fields() %>%
#'  add_multiple_survey_num() %>%
#'  wrangle_s123()
wrangle_s123 <- function(s123) {

# observers, effort ----
# initial check for observer problems
# filter(s123, Recording.Observer %in% c("other", "Other")) %>% view()

observers <- s123 %>% 
  select(code, date, multiple.survey.num, contains("observer")) %>% 
  pivot_longer(cols = contains("observer"), names_to = "role", values_to = "name") %>% 
  filter(!is.na(name), name != "") %>% 
  arrange(code, date)

# filter(observers, role == "Recording.Observer" & name %in% c("other", "Other")) %>% view()


effort_hours_surveys <- s123 %>% 
  select(code, start, end, date, multiple.survey.num, num.surveys.this.date) %>% 
  mutate(effort.hours = (end - start)/60,
         effort.hours = as.numeric(effort.hours)) %>% 
  group_by(code) %>% 
  summarise(total.hours = sum(effort.hours),
            total.hours = round(total.hours, 2),
            total.surveys = n(), .groups = "drop")


effort_days <- s123 %>% 
  distinct(code,  date) %>% 
  group_by(code) %>% 
  summarise(total.days = n(), .groups = "drop")


effort <- full_join(effort_days, effort_hours_surveys, by = "code")


seas_summary_observers <- observers %>% 
  filter(name != "other") %>% 
  mutate(name = ifelse(grepl("recording", role), paste(name, "*", sep = ""), name)) %>% 
  distinct(code, name) %>% 
  group_by(code) %>% 
  summarise(observers = paste(name, collapse = "; "), .groups = "drop") %>%
  mutate(observers = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", observers)) # add space between first and last names
  

observers_effort <- full_join(seas_summary_observers, effort, by = "code") %>% 
  left_join(readRDS(here("data/support_data/HEP_site_names_nums_utm")), by = "code") %>% 
  select(code, colony = site.name, observers, total.days, total.surveys, total.hours)


# dates ----

dates <- s123 %>% 
  select(date, code, multiple.survey.num, start, end, num.surveys.this.date, complete.count)

# determine which ROP each survey date belongs to
rop_dates <- read.csv("data/support_data/rop_dates.csv") %>% 
  filter(year == zyear) %>% 
  mutate(across(contains("date"), mdy)) %>% 
  mutate(rop.mid = end.date - 1,
         rop = paste("rop", rop, sep = ".")) %>% 
  select(rop, rop.mid) %>% 
  pivot_wider(names_from = rop, values_from = rop.mid)

if(nrow(rop_dates) == 0) {
  stop(paste("ROP dates for ", zyear, " not supplied.\nPlease fill ROP dates in data/rop_dates.csv"))
}

which_rop <- s123 %>% 
  select(date, code, multiple.survey.num, complete.count) %>% 
  mutate(rop.1.dif = as.numeric(date - rop_dates$rop.1),
         rop.2.dif = as.numeric(date - rop_dates$rop.2),
         rop.3.dif = as.numeric(date - rop_dates$rop.3),
         rop.4.dif = as.numeric(date - rop_dates$rop.4),
         rop.5.dif = as.numeric(date - rop_dates$rop.5),
         rop.6.dif = as.numeric(date - rop_dates$rop.6)) %>% 
  pivot_longer(cols = contains("rop")) %>% 
  mutate(abs.diff = abs(value)) %>% 
  arrange(code, name, date) %>% 
  group_by(code, name) %>% 
  filter(abs.diff == min(abs.diff)) %>% 
  mutate(name = gsub(".dif", "", name)) %>% 
  rename(which.rop = name,
         days.diff.rop.mid = value)%>% 
  group_by(date, multiple.survey.num) %>% 
  filter(abs.diff == min(abs.diff)) %>% 
  ungroup()

if(any(count(which_rop, date, code, multiple.survey.num)$n > 1)) {
  
  tie_cols_dates <- count(which_rop, date, code, multiple.survey.num) %>% 
    ungroup() %>% 
    filter(n > 1) %>% 
    mutate(out.col = paste(code, date, sep = ", ")) %>% 
    select(out.col) %>% 
    summarise(out.col = paste(out.col, collapse = "; "), .groups = "drop")
  
  which_rop <- which_rop %>% 
    mutate(rop.num = gsub("rop.", "", which.rop)) %>% 
    group_by(date, code, multiple.survey.num) %>% 
    filter(rop.num == min(rop.num)) %>% 
    ungroup() %>% 
    select(-rop.num)
  
  
  print(paste("Note: ROP ties for the following colonies and dates:", tie_cols_dates$out.col))
} else {
  which_rop <- which_rop
}

#
# bird data ----

birds <- s123 %>% 
  select(code, date, multiple.survey.num, contains(c("greg", "gbhe", "sneg", "bcnh", "caeg", "dcco")), complete.count, -starts_with(c("You.have", "Please.fill"))) %>% 
  pivot_longer(contains(c("greg", "gbhe", "sneg", "bcnh", "caeg", "dcco"))) 


birds <- birds %>% 
  mutate(value = ifelse(is.na(value), 0, value))
 
birds <- birds %>%
  mutate(species = case_when(grepl("greg", name) ~ "GREG",
                             grepl("gbhe", name) ~ "GBHE",
                             grepl("sneg", name) ~ "SNEG",
                             grepl("bcnh", name) ~ "BCNH",
                             grepl("caeg", name) ~ "CAEG",
                             grepl("dcco", name) ~ "DCCO"))


birds <- birds %>%
  data.frame() %>% 
  mutate(znum = str_extract(name, "[[:digit:]]+"))

birds <- birds %>%
  mutate(variable = case_when(grepl("colony.count", name) ~ "total.nests",
                              grepl("stage", name) ~ "stage",
                              grepl("chick", name) ~ "brd"))


# number nests ----
bird_total_nests <- birds %>% 
  filter(variable == "total.nests") %>% 
  select(code, date, multiple.survey.num, species, total.nests = value, complete.count) 

peak_active <- bird_total_nests %>% 
  group_by(code, species)  %>% 
  filter(total.nests ==  max(total.nests) & total.nests > 0) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>% 
  select(-total.nests) %>% 
  mutate(peak.active = TRUE)


observer_initials_by_date <- observers %>%  
  filter(name != "other") %>% 
  distinct(code, multiple.survey.num, date, name) %>% 
  mutate(initials = gsub("[^::A-Z::]","", name)) %>% 
  group_by(code, date, multiple.survey.num) %>%
  summarise(obs.initials = paste(initials, collapse='; '), .groups = "drop")
  
  

total_nests <- full_join(bird_total_nests, peak_active, by = c("code", "date", "multiple.survey.num", "species", "complete.count")) %>% 
  mutate(peak.active = ifelse(is.na(peak.active), FALSE, peak.active)) %>% 
  full_join(observer_initials_by_date, by = c("code", "date", "multiple.survey.num"))

# stages ----
bird_stages <- birds %>% 
  filter(variable == "stage") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, stage = znum) %>% 
  full_join(., select(which_rop, -contains("diff"), -complete.count), by = c("code", "date", "multiple.survey.num")) %>% 
  mutate(which.rop = ifelse(is.na(which.rop), "other", which.rop))

which_rop <- bird_stages %>% 
  distinct(date, code, multiple.survey.num) %>% 
  mutate(rop.1.dif = as.numeric(date - rop_dates$rop.1),
         rop.2.dif = as.numeric(date - rop_dates$rop.2),
         rop.3.dif = as.numeric(date - rop_dates$rop.3),
         rop.4.dif = as.numeric(date - rop_dates$rop.4),
         rop.5.dif = as.numeric(date - rop_dates$rop.5),
         rop.6.dif = as.numeric(date - rop_dates$rop.6)) %>% 
  pivot_longer(cols = contains("rop")) %>% 
  mutate(abs.diff = abs(value)) %>% 
  arrange(code, name, date) %>% 
  group_by(code, name) %>% 
  filter(abs.diff == min(abs.diff)) %>% 
  mutate(name = gsub(".dif", "", name)) %>% 
  rename(which.rop = name,
         days.diff.rop.mid = value)%>% 
  group_by(date, multiple.survey.num) %>% 
  filter(abs.diff == min(abs.diff)) %>%
  ungroup()




stages <- left_join(bird_stages, which_rop, by = c("code", "date", "multiple.survey.num", "which.rop")) %>% 
  mutate(which.rop = ifelse(is.na(which.rop), "other", which.rop)) %>% 
  select(-contains("diff"))

# stage 4 brood sizes ----
brd_size_dates <- bird_stages %>%
  filter(stage > 3) %>% 
  mutate(stage = paste("stage.", stage, sep = "")) %>% 
  pivot_wider(id_cols = c(code, species, date, multiple.survey.num), names_from = stage, values_from = num.nests) %>% 
  filter(stage.4 > 0 & stage.5 == 0) %>% 
  group_by(code, species) %>% 
  summarise(date = max(date), .groups = "drop") %>% 
  mutate(brd.size.date = TRUE)

stage5_dates <- bird_stages %>% 
  filter(stage == 5 & num.nests > 0) %>% 
  distinct(code, species, date) %>% 
  mutate(stage5.nests = TRUE)

bird_brood_sizes <- birds %>% 
  filter(variable == "brd") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, brd = znum) %>% 
  #pivot_wider(id_cols = c(code, date, species, multiple.survey.num), names_from = brd, values_from = num.nests) %>% 
  full_join(brd_size_dates, by = c("code", "date", "species")) %>% 
  full_join(stage5_dates, by = c("code", "date", "species")) %>% 
  arrange(code, species, date)



# predators, disturbance, notes details ----
# for these 3 data types, data come in at the colony level, but when output to Season Summary sheets they are expanded to the colony X species level, and this is how they eventually get input into HEPDATA. Tracking screening changes is easier if these 3 data types are explicitly expanded out to the colony X species level at this stage, rather than relying on implicit expansion at step 2. This expansion is done with the full_join(., distinct(birds, code, species)) call in the final step of each process for these data types
# predators ----
# temporarily save global warning options
oldw <- getOption("warn")
# suppress all global warnings
options(warn = -1)
predators  <- s123 %>% 
  select(code, date, multiple.survey.num, contains("predator.species")) %>% 
  pivot_longer(cols = contains("pred"), values_to = "predator.species", names_to = "obs.type") %>% 
  filter(predator.species != "") %>% 
  mutate(predator.species = paste(bird_names_from_text(predator.species), get_terrestrial_predators(predator.species), sep = "_")) %>% 
  separate(predator.species, into = paste("pred.sp", seq(1:6), sep = ""), sep = "_") %>% 
  mutate(obs.type = ifelse(grepl("nesting", obs.type), "nesting", "present"))
# reset to old global warnings setting
options(warn = oldw)

predators_longer <- predators %>% 
  pivot_longer(cols = contains("pred"), values_to = "predator.species") %>% 
  filter(!is.na(predator.species), predator.species != "", predator.species != "no") %>% 
  distinct(code, date, multiple.survey.num, obs.type, predator.species) %>% 
  mutate(predator.species = translate_bird_names(predator.species, "common.name", "alpha.code"),
         predator.species = toupper(predator.species))

 
predators_rewide <- predators_longer %>% 
  distinct(code, predator.species, obs.type) %>% 
  mutate(yup = 1) %>% 
  pivot_wider(id_cols = c(predator.species, code), names_from = obs.type, values_from = yup) %>% 
  full_join(., distinct(birds, code, species), by = "code") %>% 
  select(code, species, everything())

# disturbance ----
disturbance_long  <- s123 %>% 
  select(date, code, multiple.survey.num, contains("isturbance")) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(contains("isturbance"))

disturbance <- disturbance_long %>% 
  mutate(disturbance.obs = NA,
         disturbance.obs = ifelse(grepl("did.you.see", name), "see.signs", disturbance.obs),
         disturbance.obs = ifelse(grepl("observed.or", name), "obs.inf", disturbance.obs),
         disturbance.obs = ifelse(grepl("the.source.of", name), "type", disturbance.obs),
         disturbance.obs = ifelse(grepl("the.result.of", name), "result", disturbance.obs),
         disturbance.obs = ifelse(grepl("any.details", name), "description", disturbance.obs)) %>% 
  mutate(disturbance.num = 1,
         disturbance.num = ifelse(grepl("second", name), 2, disturbance.num),
         disturbance.num = ifelse(grepl("third", name), 3, disturbance.num),
         species = case_when(grepl("greg", name) ~ "GREG",
                             grepl("gbhe", name) ~ "GBHE",
                             grepl("sneg", name) ~ "SNEG",
                             grepl("bcnh", name) ~ "BCNH",
                             grepl("caeg", name) ~ "CAEG",
                             grepl("dcco", name) ~ "DCCO"))

disturbance_rewide <- right_join(disturbance %>%
                                  filter(!is.na(species)) %>% 
                                  pivot_wider(id_cols = c(code, date, multiple.survey.num, disturbance.num, species), values_from = value, names_from = disturbance.obs),
                                disturbance %>%
                                  filter(is.na(species)) %>% 
                                  pivot_wider(id_cols = c(code, date, multiple.survey.num, disturbance.num), values_from = value, names_from = disturbance.obs) %>% 
                                  filter(see.signs == "yes")) %>% 
  mutate(code = as.numeric(code),
         date = as.Date(date)) %>% 
#  select(-see.signs, -disturbance.num)%>% 
  full_join(., distinct(birds, code, species)) %>% 
  dplyr::select(code, species, everything())
 
# notes ----
# check notes fields for non-standard records

wide_notes <- s123 %>% 
  select(code, date, multiple.survey.num, contains("notes")) %>% 
  mutate(empty.notes = field.notes == "" & reviewnotes == "" |
                       field.notes == "" & reviewnotes == "na" |
                       field.notes == "na" & reviewnotes == "") %>% 
  filter(empty.notes == FALSE) %>% 
  arrange(code, date) %>% 
  select(-empty.notes) %>% 
  rename(review.notes = reviewnotes)

 notes <- wide_notes %>% 
  pivot_longer(cols = contains("notes"), names_to = "note.type", values_to = "notes") %>% 
  filter(!is.na(notes), notes != "", notes != "na") %>% 
  mutate(note.type = gsub(".notes", "", note.type))%>% 
  full_join(., distinct(birds, code, species), by = "code") %>% 
  select(code, species, everything())
# combine and write ----
# sites file created with https://github.com/scottfjennings/HEP_data_work/blob/master/HEP_code/HEP_utility_functions.R
# and copied manually to s123 directory

wrangled_s123 <- list(dates = dates,
                      observers.effort = observers_effort,
                      nests = total_nests,
                      stages = bird_stages,
                      brood.sizes = bird_brood_sizes,
                      predators = predators_rewide,
                      disturbance = disturbance_rewide,
                      notes = notes)
 }

#wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
