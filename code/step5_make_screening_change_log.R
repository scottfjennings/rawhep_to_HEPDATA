

# compare screened to unscreened to generate a list of changes made during screening



#' Title
#'
#' @param zyear 
#' @param ztable 
#'
#' @return
#' @export
#'
#' @examples
make_track_change_tables <- function(zyear, ztable) {
screened_sheets <- get_screened_col_spp(zyear)  
  
# get wrangled table 
wrangled_table <- readRDS(here(paste("data/wrangled/wrangled_raw_", zyear, sep = "")))[[ztable]] %>%  
  mutate(record.in.wrangled = TRUE) %>% 
  mutate(across(everything(), as.character))

# starting with 2021, observers.effort has species field and predators has date and multi survey fields. need to remove them
if(ztable == "observers.effort" & zyear > 2020) {
  wrangled_table <- wrangled_table %>% 
    dplyr::select(-species)
}

if(ztable == "predators" & zyear > 2020) {
  wrangled_table <- wrangled_table %>% 
    select(-date, -multiple.survey.num)
}
# get screened table 
screened_table <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))[[ztable]] %>% 
  data.frame() %>%
  ungroup() %>% 
  mutate(record.in.screened = TRUE) %>% 
  mutate(across(everything(), as.character))


table_changelog <- full_join(wrangled_table, screened_table) %>% 
  full_join(screened_sheets) %>% # join with screen_log to have screening notes and confirmation that this colony X species was screened (assuming good record keeping during screening process)
  arrange(code, species) %>% 
  mutate(changelog = case_when(screened == TRUE & record.in.wrangled == record.in.screened ~ "screened, no change",
                               screened == TRUE & record.in.wrangled == TRUE & is.na(record.in.screened) ~ "screened, changed from",
                               screened == TRUE & is.na(record.in.wrangled) & record.in.screened == TRUE ~ "screened, changed to")) %>% 
  mutate(code = as.numeric(code)) %>% 
  select(code, species, everything()) %>% 
  relocate(c(record.in.wrangled, record.in.screened, screened, changelog), .after = last_col())
return(table_changelog)
}


#' Title
#'
#' @param zyear 
#' @param ztable 
#'
#' @return
#' @export
#'
#' @examples
make_track_change_date_tables <- function(zyear, ztable) {
screened_sheets <- get_screened_col_spp(zyear)  
  
# get wrangled table 
wrangled_table <- readRDS(here(paste("data/wrangled/wrangled_raw_", zyear, sep = "")))[[ztable]] %>%  
  mutate(record.in.wrangled = TRUE) %>% 
  mutate(across(everything(), as.character))

# get screened table 
screened_table <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))[[ztable]] %>% 
  data.frame() %>% 
  ungroup() %>% 
  mutate(record.in.screened = TRUE) %>% 
  mutate(across(everything(), as.character)) 

if(ztable == "disturbance") {
  screened_table <- screened_table %>% 
    rename("obs.inf" = observed.inferred)
}

# "NA" is a valid observer initial, and there shouldn't be any NA in obs.initials
if(ztable == "nests") {
  screened_table <- screened_table %>% 
    mutate(across(-obs.initials, ~(ifelse(.=="NA", NA, .))))
} else {
  screened_table <- screened_table %>% 
  mutate_all(~(ifelse(.=="NA", NA, .)))
}

if(ztable == "brood.sizes") {
  wrangled_table <- right_join(wrangled_table, distinct(screened_table, code, date, species))
}



table_changelog <- full_join(wrangled_table, screened_table) %>% 
  full_join(screened_sheets) %>% # join with screen_log to have screening notes and confirmation that this colony X species was screened (assuming good record keeping during screening process)
  arrange(code, species, date) %>% 
  mutate(changelog = case_when(screened == TRUE & record.in.wrangled == record.in.screened ~ "screened, no change",
                               screened == TRUE & record.in.wrangled == TRUE & is.na(record.in.screened) ~ "screened, changed from",
                               screened == TRUE & is.na(record.in.wrangled) & record.in.screened == TRUE ~ "screened, changed to")) %>% 
  mutate(code = as.numeric(code)) %>% 
  select(code, species, everything()) %>% 
  relocate(c(record.in.wrangled, record.in.screened, screened, changelog), .after = last_col())
return(table_changelog)
}

# screened_stages <- make_track_change_tables(2020, "stages")



