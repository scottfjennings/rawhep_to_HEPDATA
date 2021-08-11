

# compare screened to unscreened to generate a list of changes made during screening

# key data fields we expect to change are:
# peak active date
# ROP days
# stage 4 date
# need to also check numeric nest count fields to make sure they didn't change accidentally 

# logic is based on merging pre-screened and screened data. where this merge results in 2 records instead of 1, this indicates the record was changed during screening

# library(tidyverse)
# library(here)

# zyear = 2020
# screened_s123 <- readRDS(here(paste("data/screened/screened_s123_", zyear, sep = "")))
# wrangled_s123 <- readRDS(here(paste("data/wrangled/wrangled_s123_", zyear, sep = "")))

get_screened_sheets <- function(zyear) {
screened_sheets <- list.files(paste("season_summary_forms/", zyear, "/", sep = "")) %>% 
  data.frame() %>% 
  rename(file.name = 1) %>% 
  mutate(file.name = gsub(".docx", "", file.name)) %>% 
  separate(file.name, into = c("code", "year", "species", "screened"), sep = "_") %>% 
  mutate(summary.sheet.made = TRUE,
         screened = ifelse(is.na(screened), "no", screened)) %>% 
  mutate(across(everything(), as.character)) %>% 
  select(-year)
}


make_track_change_tables <- function(zyear, ztable) {
# get wrangled table 
wrangled_table <- readRDS(here(paste("data/wrangled/wrangled_s123_", zyear, sep = "")))[[ztable]] %>%  
  mutate(record.in.prescreened = TRUE) %>% 
  mutate(across(everything(), as.character))

# get screened table 
screened_table <- readRDS(here(paste("data/screened/screened_s123_", zyear, sep = "")))[[ztable]] %>% 
  ungroup() %>% 
  mutate(record.in.screened = TRUE) %>% 
  mutate(across(everything(), as.character))

table_changelog <- full_join(wrangled_table, screened_table) %>% 
  full_join(screened_sheets) %>% # join with screen_log to have screening notes and confirmation that this colony X species was screened (assuming good record keeping during screening process) 
  mutate(changelog = case_when(record.in.prescreened == TRUE & is.na(summary.sheet.made) ~ "no summary sheet",
                               #species.not.nesting == TRUE ~ "species not nesting",
                               screened == "no" ~ "not screened",
                               grepl("screened", screened) & record.in.prescreened == TRUE & is.na(record.in.screened) ~ "screened, changed from",
                               grepl("screened", screened) & is.na(record.in.prescreened) & record.in.screened == TRUE ~ "screened, changed to",
                               grepl("screened", screened) & record.in.prescreened == TRUE & record.in.screened == TRUE ~ "screened, no change")) %>% 
  mutate(code = as.numeric(code)) %>% 
  select(code, species, everything())
return(table_changelog)
}





# screened_stages <- make_track_change_tables(2020, "stages")
