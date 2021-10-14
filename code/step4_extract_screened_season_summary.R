


# only load package that's needed here. 
library(docxtractr)


# zyear = 2020
# zcode = 599
# zspp = "GREG"
# doc <- read_docx(paste("season_summary_forms/", zyear, "/", zcode, "_", zyear, "_", zspp, "_screened1.docx", sep = ""))





# screening log ----
get_screening_log <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  screening_log <- extr_doc[[1]]
}

# all_screening_log <- map2_df(zyear, seas_summ_files, get_screening_log)

# effort summary ----

get_observers_effort <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  observers_effort <- extr_doc[[2]]
  observers_effort <- observers_effort %>% 
    mutate(across(.cols = c(code, total.days, total.surveys, total.hours), as.numeric))
  }

# all_effort_summary <- map2_df(zyear, seas_summ_files, get_effort_summary)


# tables with date columns ----
# first 2 and last 3 tables will always be the same, and they will always exist even if they don't contain data
# middle n tables will have the by-date data, the first n/2 of these will have the total nest numbers, and the second n/2 will have nest stages


# helper function for date tables
# fix date field
monthday_to_date <- function(zdf) {
  zdf <- zdf %>% 
    mutate(date = gsub("*", "", date)) %>% 
    separate(date, into = c("date", "multiple.survey.num"), 7) %>% 
    mutate(multiple.survey.num = ifelse(multiple.survey.num == "" | is.na(multiple.survey.num), 1, multiple.survey.num),
           multiple.survey.num = as.numeric(multiple.survey.num)) %>% 
    mutate(date = gsub("X", paste(zyear, "-", sep = ""), date),
           date = gsub("\\.", "-", date),
           date = as.Date(date))
}

#

# total nests ----

get_total_nest_table <- function(zyear, zfile) {
  
  
get_date_tables <- function(table_num) {
  nest_tab_wide <- extr_doc[[table_num]]
}

doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
extr_doc <- docx_extract_all_tbls(doc)

num_total_nest_tables <- extr_doc[[length(extr_doc)]]$num_total_nest_tables


total_nest_tables <- seq(3, length.out = as.numeric(num_total_nest_tables))

total_nests <- map(total_nest_tables, get_date_tables)  %>%
             reduce(full_join) %>% 
  pivot_longer(cols = contains("X"), names_to = "date") %>% 
  pivot_wider(values_from = value, names_from = variable)  %>% 
  mutate(code = as.numeric(code),
         total.nests = as.numeric(total.nests),
         peak.active = ifelse(peak.active == 0, FALSE, TRUE)) %>% 
  monthday_to_date() %>% 
  select(code, date, multiple.survey.num, everything())

}

#get_total_nest_table <- safely(get_total_nest_table)
#get_total_nest_table <- possibly(get_total_nest_table, otherwise = NULL)

# all_total_nest_tables <- map2_df(zyear, seas_summ_files, get_total_nest_table) %>% fix_dates() 





# nest stages ----
# the nest stages tables contain 2 different types of information: number of nests in each stage, and ROP dates. need to separate these 2 types of info, so have 1 function to extract the stage tables from the doc list, then 2 more to separate the 2 types of info

get_nest_stage_rop_table <- function(zyear, zfile) {

get_date_tables <- function(table_num) {
  nest_tab_wide <- extr_doc[[table_num]]
}

  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  
num_stage_tables <- extr_doc[[length(extr_doc)]]$num_nest_stage_tables %>% as.numeric()

  if(num_stage_tables > 0) {
nest_stage_tables <- seq(3 + num_stage_tables, length.out = num_stage_tables)

nest_stages_rop <- map(nest_stage_tables, get_date_tables)  %>%
             reduce(full_join) %>% 
  pivot_longer(cols = contains("X"), names_to = "date") %>%  
  monthday_to_date() 

stages <- full_join(nest_stages_rop %>% 
                      filter(grepl("stage", variable)) %>% 
                      rename(stage = variable, num.nests = value),
                    nest_stages_rop %>% 
                      filter(variable == "which.rop") %>% 
                      rename(which.rop = value) %>% 
                      select(-variable), by = c("code", "species", "date", "multiple.survey.num")) %>% 
  mutate(stage = gsub("stage.", "", stage),
         which.rop = tolower(which.rop))%>% 
    mutate(across(.cols = c(code, num.nests), as.numeric))
}
}

# all_stage_nest_nums_rop <- map2_df(zyear, seas_summ_files, get_nest_stage_rop_table)


# brood sizes ----
get_stage4brd <- function(zyear, zfile) {
if(grepl("GREG|GBHE", zfile)){
    doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  stage4brd <- extr_doc[[length(extr_doc) - 4]]
  stage4brd <- stage4brd %>% 
    rename_all(~sub("brd.date", "brd.size.date", .x))

  if(nrow(stage4brd) == 1 & stage4brd$brd.size.date == "NA") {
    stage4brd = NULL
  } else {
  stage4brd <- stage4brd %>% 
    pivot_longer(cols = c(paste("brd", seq(1, 5), sep = ".")), names_to = "brd", values_to = "num.nests") %>%
    mutate(brd = gsub("brd.", "", brd)) %>% 
    separate(date, into = c("date", "multiple.survey.num"), 11) %>% 
    mutate(multiple.survey.num = ifelse(multiple.survey.num == "" | is.na(multiple.survey.num), 1, multiple.survey.num)) %>% 
    select(code, date, multiple.survey.num, species, num.nests, brd, brd.size.date, stage5.nests) %>% 
    mutate(across(.cols = c(code, num.nests), as.numeric))
  }
  }
}

# all_stage4brd <- map2(zyear, screened_seas_summ_files[1:14], safely(get_stage4brd)) 

# get_stage4brd(zyear, screened_seas_summ_files[3]) %>% view()

# disturbance ----
 get_disturbance <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  disturbance <- extr_doc[[length(extr_doc) - 3]]
  disturbance <- disturbance %>% 
    mutate(code = as.numeric(code))
}

# all_disturbance <- map2_df(zyear, seas_summ_files, get_disturbance) %>% distinct() # should be duplicated for each species, distinct will remove dups
  
# predators ----
get_predators <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  predators <- extr_doc[[length(extr_doc)-2]]
  predators <- predators %>% 
    mutate(code = as.numeric(code),
           present = ifelse(present == "NA", NA, present),
           nesting = ifelse(nesting == "NA", NA, nesting))
}

# all_predators <- map2_df(zyear, seas_summ_files, get_predators) %>% distinct() # should be duplicated for each species, distinct will remove dups
  
# notes ----
get_notes <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  notes <- extr_doc[[length(extr_doc)-1]]
  notes <- notes %>% 
    separate(date, into = c("date", "multiple.survey.num"), 11) %>% 
    mutate(code = as.numeric(code),
           multiple.survey.num = ifelse(multiple.survey.num == "" | is.na(multiple.survey.num), 1, multiple.survey.num),
           multiple.survey.num = as.numeric(multiple.survey.num))
  }

# all_notes <- map2_df(zyear, seas_summ_files, get_notes) %>% distinct() # should be duplicated for each species, distinct will remove dups
  

# combine ----

#screened_s123 <- list(screen_log = all_screening_log,
#                      effort_summary = all_effort_summary,
#                      total_nests = all_total_nest_tables,
#                      nest_stages = all_stage_nest_nums,
#                      rop_dates = all_rop_dates,
#                      brood_sizes = all_stage4brd,
#                      predators = all_predators,
#                      disturbance = all_disturbance,
#                      notes = all_notes)



# saveRDS(screened_s123, paste("data/screened/screened_s123_", zyear, sep = ""))
 