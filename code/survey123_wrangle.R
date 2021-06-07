


library(tidyverse)
library(lubridate)
library(data.table)
options(scipen = 999)

# read, create data ----
zyear = 2020
s123_file = paste("HEP_", zyear, "_0", sep = "")
#s123_file_proofed <- paste("HEP", zyear, "cumulative", sep = "")
# check for multiple surveys at same site and same time
# check for time span between surveys

rop_dates <- read.csv("data/rop_dates.csv") %>% 
  filter(year == zyear) %>% 
  mutate(across(contains("date"), mdy)) %>% 
  mutate(rop.mid = end.date - 1,
         rop = paste("rop", rop, sep = ".")) %>% 
  select(rop, rop.mid) %>% 
  pivot_wider(names_from = rop, values_from = rop.mid)

# read site data
# this file created with https://github.com/scottfjennings/HEP_data_work/blob/master/HEP_code/HEP_utility_functions.R
# and copied manually to s123 directory
hep_sites <- readRDS("data/HEP_site_names_nums_utm")


# downloaded Survey123 data
s123 <- read.csv(
  paste("data/downloaded/", s123_file, ".csv", sep = "")
  )

# add test data??

# s123 <- rbind(s123, readRDS("data/downloaded/test_data"))
  
# hep_sites <- rbind(hep_sites, data.frame(code = 599, site.name = "Test colony", utmnorth = 4213415, utmeast = 516975))


colnames(s123) <- gsub("Please.describe.why.you.were.unable.to.complete.colony.count..and.list.which.species.weren.t.adequately.counted.", "incomplete.count.notes", colnames(s123))
                       
s123 <- s123 %>%   
  select(-starts_with(c("X.", "Tally.", "A.nest.is.active", "Re.locate.the", "Please", "Really", "You.have"))) %>% # drop fields that are really just instructions in the survey
  rename(code = Select.Colony, start = Start.Time, end = End.Time) %>% 
  mutate(start = as.POSIXct(start, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT"),
         end = as.POSIXct(end, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT")) %>% 
  mutate(start = with_tz(start, Sys.timezone(location = TRUE)),
         end = with_tz(end, Sys.timezone(location = TRUE)),
         date = as.Date(start, tz = Sys.timezone(location = TRUE))) %>% 
  filter(useforsummary == "y") %>%
  group_by(code, date) %>% 
  mutate(multiple.survey.num = row_number()) %>% 
  ungroup()

# select(s123, code, date, Start.Time, End.Time) %>% arrange(code, date) %>% group_by(code) %>% mutate(dup.days = date == lag(date)) %>%  view()

# check for multiple records for the same colony on the same date

# multiple_records <- s123 %>% group_by(code, date) %>% mutate(num.records = n()) %>% filter(num.records > 1) 



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

# notes ----
# check notes fields for non-standard records

notes <- s123 %>% 
  select(code, date, multiple.survey.num, contains("notes")) %>% 
  rename(field.notes = Any.other.notes.belong.here.) %>% 
  mutate(empty.notes = field.notes == "" & reviewnotes == "" |
                       field.notes == "" & reviewnotes == "na" |
                       field.notes == "na" & reviewnotes == "") %>% 
  filter(empty.notes == FALSE) %>% 
  arrange(code, date) %>% 
  select(-empty.notes) %>% 
  rename(review.notes = reviewnotes)


# observers ----
# initial check for observer problems
# filter(s123, Recording.Observer %in% c("other", "Other")) %>% view()

observers <- s123 %>% 
  select(code, date, multiple.survey.num, contains("Observer")) %>% 
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

# check that each observerXsite combination matches that expected based on the hep tracking sheet
# this is a nifty idea but tracking sheet may not be totally accurate
#check_observers_sites <- observers %>% 
#  distinct() %>% 
#  filter(name != "other", !grepl("Other", role)) %>% 
#  mutate(code = as.numeric(code)) %>% 
#  left_join(observers_sites, by = "code") %>% 
#  rowwise() %>%
#  mutate(expected.observer = grepl(name, assigned.name)) %>% 
#  arrange(code) %>% 
  #filter(expected.observer == FALSE) %>% 
#  view()



# dates ----
survey_dates <- s123 %>% 
  select(code, start, end, date, multiple.survey.num) %>% 
  mutate(total.minutes = (end - start)/60,
         total.minutes = as.numeric(total.minutes)) 

which_rop <- survey_dates %>% 
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

survey_dates <- full_join(survey_dates, which_rop) 

# check start and end date match; if nrow = 0, no problem
#survey_dates %>% 
#  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
#  nrow()

# check for any surveys entered as happening at night; this probably not needed
#check_survey_daytime <- function(dates_df){
#pts <- cbind(-122.454812, 38.037819)
#df.sp <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))

#dawn = data.frame(crepuscule(df.sp, survey_dates$start, solarDep = 6, direction = "dawn", POSIXct.out = TRUE))
#dusk = data.frame(crepuscule(df.sp, survey_dates$start, solarDep = 6, direction = "dusk", POSIXct.out = TRUE))
  
#dawn.dusk <- cbind(select(dawn, dawn.time = time), select(dusk, dusk.time = time))
#df_dawn.dusk <- cbind(dates_df, dawn.dusk) %>% 
#  mutate(inlight = start	%within%	interval(dawn.time, dusk.time)) %>% 
#  filter(inlight == FALSE)
#}

#survey_dates %>% 
#  check_survey_daytime() %>% 
#  view()

# check for multiple surveys at same site and same time
# survey_dates %>% group_by(code, date) %>% summarise(num.survey = n()) %>% filter(num.survey > 1) %>% arrange(-num.survey) %>% left_join(s123) %>% view()


# predators ----
predators  <- s123 %>% 
  select(code, date, multiple.survey.num, contains("redator"), contains("within.100m")) %>% 
  rename(predators.present = Did.you.see.any.potential.nest.predators.within.100m.of.the.colony.,
         predator.species = Which.predator.species.did.you.see.,
         other.predator.species = Which.predator.species.not.listed.did.you.see.,
         predators.nesting = Were.any.potential.predator.species.nesting.within.100m.of.the.colony.,
         species.nesting = Which.species.were.nesting.within.100m.of.the.colony.) %>% 
  filter(predators.present == "yes" | predators.nesting == "yes")

# disturbance ----
disturbance_long  <- s123 %>% 
  select(date, code, multiple.survey.num, contains("isturbance")) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(contains("isturbance"))

disturbance <- disturbance_long %>% 
  mutate(disturbance.obs = NA,
         disturbance.obs = ifelse(grepl("Did.you.see", name), "see.signs", disturbance.obs),
         disturbance.obs = ifelse(grepl("observed.or", name), "obs.inf", disturbance.obs),
         disturbance.obs = ifelse(grepl("the.source.of", name), "source", disturbance.obs),
         disturbance.obs = ifelse(grepl("the.result.of", name), "result", disturbance.obs),
         disturbance.obs = ifelse(grepl("any.details", name), "details", disturbance.obs)) %>% 
  mutate(disturbance.num = 1,
         disturbance.num = ifelse(grepl("second", name), 2, disturbance.num),
         disturbance.num = ifelse(grepl("third", name), 3, disturbance.num))

disturbance_rewide <- disturbance %>% 
  pivot_wider(id_cols = c(code, date, multiple.survey.num, disturbance.num), values_from = value, names_from = disturbance.obs) %>% 
  filter(see.signs == "yes")
 
# bird data ----
birds <- s123 %>% 
  select(code, date, multiple.survey.num, contains(c("eron", "gret", "ormorant",  "GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO")), -starts_with(c("You.have", "Please.fill"))) %>% 
  pivot_longer(contains(c("eron", "gret", "ormorant", "GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO"))) 


birds <- birds %>% 
  mutate(value = ifelse(is.na(value), 0, value))
 
birds <- birds %>%
  mutate(species = case_when(grepl("GREG", name) | grepl("Great.Egret", name) ~ "GREG",
                             grepl("GBHE", name) | grepl("Great.Blue.Heron", name) ~ "GBHE",
                             grepl("SNEG", name) | grepl("Snowy.Egret", name) ~ "SNEG",
                             grepl("BCNH", name) | grepl("Black.Crowned.Night.Heron", name) ~ "BCNH",
                             grepl("CAEG", name) | grepl("Cattle.Egret", name) ~ "CAEG",
                             grepl("DCCO", name) | grepl("Double.Crested.Cormorant", name) ~ "DCCO"))


birds <- birds %>%
  data.frame() %>% 
  mutate(znum = str_extract(name, "[[:digit:]]+"))

birds <- birds %>%
  mutate(variable = case_when(grepl("Colony.Count", name) ~ "total.nests",
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

total_nests <- full_join(bird_total_nests, peak_active) %>% 
  mutate(peak.active = ifelse(is.na(peak.active), FALSE, peak.active))

# stages ----
bird_stages <- birds %>% 
  filter(variable == "stage") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, stage = znum)

# brood sizes ----
bird_brood_sizes <- birds %>% 
  filter(variable == "brd") %>% 
  select(code, date, multiple.survey.num, species, num.nests = value, brd = znum) %>% 
  pivot_wider()




# combine and write ----

list(observers = observers,
     nests = total_nests,
     stages = bird_stages,
     brood.sizes = bird_brood_sizes,
     predators = predators,
     disturbance = disturbance_rewide,
     notes = notes,
     sites = hep_sites,
     dates = survey_dates) %>% 
  saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
