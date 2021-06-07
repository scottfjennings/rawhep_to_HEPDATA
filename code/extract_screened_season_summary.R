


library(tidyverse)
library(docxtractr)
library(here)

zyear = 2020
seas_summ_files <- list.files(paste("season_summary_forms/", zyear, "/", sep = ""))


# zcode = 599
# zspp = "GREG"



#zspp_list <- c("GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO")


#doc <- read_docx(paste("season_summary_forms/", zyear, "/", zcode, "_", zyear, "_", zspp, ".docx", sep = ""))


#dat.file <- paste("data/wrangled/wrangled_s123", zyear, sep = "_")
#wrangled_s123 <- readRDS(here(dat.file))


#parm lists

#parms <- expand.grid(zyear = zyear, zfile = seas_summ_files)

# extr_doc <- docx_extract_all_tbls(doc)

#----


# screening log ----
get_screening_log <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  screening_log <- extr_doc[[1]]
}

all_screening_log <- map2_df(zyear, seas_summ_files, get_screening_log)

# effort summary ----

get_effort_summary <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  effort_summary <- extr_doc[[2]]
}

all_effort_summary <- map2_df(zyear, seas_summ_files, get_effort_summary)


# tables with date columns ----
# first 2 and last 3 tables will always be the same, and they will always exist even if they don't contain data
# middle n tables will have the by-date data, the first n/2 of these will have the total nest numbers, and the second n/2 will have nest stages

#num_date_tables <- ((length(extr_doc) - 5)/2) - 1


#total_nest_tables <- seq(3, 3 + num_date_tables)
#nest_stage_tables <- seq(3 + num_date_tables + 1, 3 + num_date_tables + 1 + num_date_tables)

#get_date_tables <- function(table_num) {
#  nest_tab_wide <- extr_doc[[table_num]]
#}


# fix date field
fix_dates <- function(zdf) {
  zdf <- zdf %>% 
    separate(date, into = c("date", "multiple.survey.num"), 7) %>% 
    mutate(multiple.survey.num = ifelse(multiple.survey.num == "" | is.na(multiple.survey.num), 1, multiple.survey.num)) %>% 
    mutate(date = gsub("X", paste(zyear, "-", sep = ""), date),
           date = gsub("\\.", "-", date),
           date = as.Date(date))
}



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
         peak.active = ifelse(peak.active == 0, FALSE, TRUE))

}

all_total_nest_tables <- map2_df(zyear, seas_summ_files, get_total_nest_table) %>%
  fix_dates() 





# nest stages ----


get_nest_stage_table <- function(zyear, zfile) {

get_date_tables <- function(table_num) {
  nest_tab_wide <- extr_doc[[table_num]]
}

  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  
num_stage_tables <- extr_doc[[length(extr_doc)]]$num_nest_stage_tables %>% as.numeric()




  if(num_stage_tables > 0) {
nest_stage_tables <- seq(3 + num_stage_tables, length.out = num_stage_tables)

nest_stages <- map(nest_stage_tables, get_date_tables)  %>%
             reduce(full_join) %>% 
  pivot_longer(cols = contains("X"), names_to = "date") %>% 
  pivot_wider(values_from = value, names_from = stage)
}
}

all_nest_stage_tables <- map2_df(zyear, seas_summ_files, get_nest_stage_table) %>%
  fix_dates()



# brood sizes ----
get_stage4brd <- function(zyear, zfile) {
if(grepl("GREG|GBHE", zfile)){
    doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  stage4brd <- extr_doc[[length(extr_doc) - 2]]
  stage4brd <- stage4brd
}
}

all_stage4brd <- map2_df(zyear, seas_summ_files, get_stage4brd) 
#

# predators ----
get_predators <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  predators <- extr_doc[[length(extr_doc)]]
}

all_predators <- map2_df(zyear, seas_summ_files, get_predators) %>% 
  distinct() # should be duplicated for each species, distinct will remove dups
  

# disturbance ----
 get_disturbance <- function(zyear, zfile) {
  doc <- read_docx(paste("season_summary_forms/", zyear, "/", zfile, sep = ""))
  extr_doc <- docx_extract_all_tbls(doc)
  disturbance <- extr_doc[[length(extr_doc) - 1]]
}

all_disturbance <- map2_df(zyear, seas_summ_files, get_disturbance) %>% 
  distinct() # should be duplicated for each species, distinct will remove dups
  

# combine ----

screened_s123 <- list(screen_log = all_screening_log,
                      effort_summary = all_effort_summary,
                      total_nests = all_total_nest_tables,
                      nest_stages = all_nest_stage_tables,
                      brood_sizes = all_stage4brd,
                      predators = all_predators,
                      disturbance = all_disturbance)

saveRDS(screened_s123, paste("data/screened/screened_s123_", zyear))
 