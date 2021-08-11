


library(tidyverse)
library(lubridate)


options(scipen = 999)

# read, create data ----
# zyear = 2020

read_s123 <- function(zyear, add.test.data = FALSE) {
s123_file = paste("HEP_", zyear, "_0", sep = "")
#s123_file_proofed <- paste("HEP", zyear, "cumulative", sep = "")

# read site data
# this file created with https://github.com/scottfjennings/HEP_data_work/blob/master/HEP_code/HEP_utility_functions.R
# and copied manually to s123 directory

# downloaded Survey123 data
s123 <- read.csv(
  paste("data/downloaded/", s123_file, ".csv", sep = "")
  )

if(add.test.data == TRUE) {
 s123 <- rbind(s123, readRDS("data/downloaded/test_data"))
}

s123 <- s123 %>% 
  mutate(across(where(is.character), str_trim))

}


# code to add test data site info
# if test data are not added to s_123 in the above read function, then test site data are ignored at left_join where observers_effort created below
# readRDS("data/HEP_site_names_nums_utm") %>% rbind(., data.frame(code = 599, site.name = "Test colony", utmnorth = 4213415, utmeast = 516975)) %>% saveRDS(., "data/HEP_site_names_nums_utm") 





#  wrangled_s123$sites <- rbind(wrangled_s123$sites, data.frame(code = 599, site.name = "Test colony", utmnorth = 4213415, utmeast = 516975))


# deal with the funky Survey123 field names ----
fix_s123_names <- function(s123) {
colnames(s123) <- gsub("Please.describe.why.you.were.unable.to.complete.colony.count..and.list.which.species.weren.t.adequately.counted.", "incomplete.count.notes", colnames(s123))
                       
s123 <- s123 %>%   
  select(-starts_with(c("X.", "Tally.", "A.nest.is.active", "Re.locate.the", "Please", "Really", "You.have"))) %>% # drop fields that are really just instructions in the survey
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

# convert date fields ----
# fix time zone to pacific
fix_s123_date_fields <- function(s123) {
s123 <- s123 %>% 
  mutate(start = as.POSIXct(start, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT"),
         end = as.POSIXct(end, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT")) %>% 
  mutate(start = with_tz(start, Sys.timezone(location = TRUE)),
         end = with_tz(end, Sys.timezone(location = TRUE)),
         date = as.Date(start, tz = Sys.timezone(location = TRUE)))
}

# add helper columns to indicate if multiple surveys were done at the same colony on the same day ----
add_multiple_survey_num <- function(s123) {
s123 <- s123 %>%
  group_by(code, date) %>% 
  mutate(multiple.survey.num = row_number(),
         num.surveys.this.date = n()) %>% 
  ungroup()
}


#s123 <- read_s123(zyear, add.test.data = TRUE) %>% filter(useforsummary == "y") %>% fix_s123_names() %>% fix_s123_date_fields() %>% add_multiple_survey_num()

# proofed data ----
#s123_proofed <- read.csv(paste("HEP_data/", s123_file_proofed, ".csv", sep = ""))


#check_cols <- full_join(names(s123_proofed) %>% 
#  data.frame() %>% 
#  rename(varb = 1) %>% 
#  mutate(s123.proofed = 1),
#names(s123) %>% 
#  data.frame() %>% 
#  rename(varb = 1) %>% 
#  mutate(s123 = 1))

# wrangle survey123 ----
wrangle_s123 <- function(s123) {

# observers, effort ----
# initial check for observer problems
# filter(s123, Recording.Observer %in% c("other", "Other")) %>% view()

observers <- s123 %>% 
  select(code, date, multiple.survey.num, contains("observer")) %>% 
  pivot_longer(cols = contains("Observer"), names_to = "role", values_to = "name") %>% 
  filter(!is.na(name), name != "") %>% 
  arrange(code, date)

# filter(observers, role == "Recording.Observer" & name %in% c("other", "Other")) %>% view()

# deal with single funky name record in 2021
#  TODO fix this in Survey123
if(zyear == 2021) {
observers <- rbind(observers, code = rep("68.1", 2),
                              date = rep("2021-03-06", 2),
                               role = c("Recording.Observer.", "Other.Observer.Name."),
                               name = c("WendyCole", "SusanCalcagno")) %>% 
  filter(name != "Wendy Cole recording observer, Susan Calcagno") %>% 
  mutate(code = as.numeric(code)) %>% 
  arrange(code, date)
} else {
  observers <- observers
}


effort_hours_surveys <- s123 %>% 
  select(code, start, end, date, multiple.survey.num, num.surveys.this.date) %>% 
  mutate(effort.hours = (end - start)/60,
         effort.hours = as.numeric(effort.hours)) %>% 
  group_by(code) %>% 
  summarise(total.hours = sum(effort.hours),
            total.hours = round(total.hours, 2),
            total.surveys = n())


effort_days <- s123 %>% 
  distinct(code,  date) %>% 
  group_by(code) %>% 
  summarise(total.days = n())


effort <- full_join(effort_days, effort_hours_surveys)


seas_summary_observers <- observers %>% 
  filter(name != "other") %>% 
  mutate(name = ifelse(grepl("Recording", role), paste(name, "*", sep = ""), name)) %>% 
  distinct(code, name) %>% 
  group_by(code) %>% 
  summarise(observers = paste(name, collapse = "; ")) %>% 
  ungroup() %>% 
  mutate(observers = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", observers)) # add space between first and last names
  

observers_effort <- full_join(seas_summary_observers, effort) %>% 
  left_join(readRDS(here("data/HEP_site_names_nums_utm"))) %>% 
  select(code, colony = site.name, observers, total.days, total.surveys, total.hours)


# dates ----
# determine which ROP each survey date belongs to
rop_dates <- read.csv("data/rop_dates.csv") %>% 
  filter(year == zyear) %>% 
  mutate(across(contains("date"), mdy)) %>% 
  mutate(rop.mid = end.date - 1,
         rop = paste("rop", rop, sep = ".")) %>% 
  select(rop, rop.mid) %>% 
  pivot_wider(names_from = rop, values_from = rop.mid)

which_rop <- s123 %>% 
  select(date, code, multiple.survey.num) %>% 
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

#
# bird data ----

birds <- s123 %>% 
  select(code, date, multiple.survey.num, contains(c("greg", "gbhe", "sneg", "bcnh", "caeg", "dcco")), -starts_with(c("You.have", "Please.fill"))) %>% 
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
  select(code, date, multiple.survey.num, species, total.nests = value) 

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
  summarise(obs.initials = paste(initials, collapse='; ')) %>% 
  ungroup()
  
  

total_nests <- full_join(bird_total_nests, peak_active) %>% 
  mutate(peak.active = ifelse(is.na(peak.active), FALSE, peak.active)) %>% 
  full_join(observer_initials_by_date)

# stages ----
bird_stages <- birds %>% 
  filter(variable == "stage") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, stage = znum) %>% 
  full_join(., select(which_rop, -contains("diff"))) %>% 
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

stages <- left_join(bird_stages, which_rop) %>% 
  mutate(which.rop = ifelse(is.na(which.rop), "other", which.rop)) %>% 
  select(-contains("diff"))

# stage 4 brood sizes ----
brd_size_dates <- bird_stages %>%
  filter(stage > 3) %>% 
  mutate(stage = paste("stage.", stage, sep = "")) %>% 
  pivot_wider(id_cols = c(code, species, date, multiple.survey.num), names_from = stage, values_from = num.nests) %>% 
  filter(stage.4 > 0 & stage.5 == 0) %>% 
  group_by(code, species) %>% 
  summarise(date = max(date)) %>% 
  mutate(brd.size.date = TRUE)

stage5_dates <- bird_stages %>% 
  filter(stage == 5 & num.nests > 0) %>% 
  distinct(code, species, date) %>% 
  mutate(stage5.nests = TRUE)

bird_brood_sizes <- birds %>% 
  filter(variable == "brd") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, brd = znum) %>% 
  pivot_wider() %>% 
  full_join(brd_size_dates) %>% 
  full_join(stage5_dates) %>% 
  arrange(code, species, date)



# predators, disturbance, notes details ----
# for these 3 data types, data come in at the colony level, but when output to Season Summary sheets they are expanded to the colony X species level, and this is how they eventually get input into HEPDATA. Tracking screening changes is easier if these 3 data types are explicitly expanded out to the colony X species level at this stage, rather than relying on implicit expansion at step 2. This expansion is done with the full_join(., distinct(birds, code, species)) call in the final step of each process for these data types
# predators ----
predators  <- s123 %>% 
  select(code, date, multiple.survey.num, contains("predator.species")) %>% 
  pivot_longer(cols = contains("pred"), values_to = "predator.species", names_to = "obs.type") %>% 
  filter(predator.species != "") %>% 
  mutate(predator.species = paste(bird_names_from_text(predator.species), get_terrestrial_predators(predator.species), sep = "_")) %>% 
  separate(predator.species, into = paste("pred.sp", seq(1:6), sep = ""), sep = "_") %>% 
  mutate(obs.type = ifelse(grepl("nesting", obs.type), "nesting", "present"))

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
  full_join(., distinct(birds, code, species)) %>% 
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
         disturbance.num = ifelse(grepl("third", name), 3, disturbance.num))

disturbance_rewide <- disturbance %>% 
  pivot_wider(id_cols = c(code, date, multiple.survey.num, disturbance.num), values_from = value, names_from = disturbance.obs) %>% 
  mutate(code = as.numeric(code)) %>% 
  filter(see.signs == "yes") %>% 
  select(-see.signs, -disturbance.num)%>% 
  full_join(., distinct(birds, code, species)) %>% 
  select(code, species, everything())
 
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
  full_join(., distinct(birds, code, species)) %>% 
  select(code, species, everything())
# combine and write ----
# sites file created with https://github.com/scottfjennings/HEP_data_work/blob/master/HEP_code/HEP_utility_functions.R
# and copied manually to s123 directory

wrangled_s123 <- list(observers.effort = observers_effort,
     nests = total_nests,
     stages = bird_stages,
     brood.sizes = bird_brood_sizes,
     predators = predators_rewide,
     disturbance = disturbance_rewide,
     notes = notes)
}

#wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
