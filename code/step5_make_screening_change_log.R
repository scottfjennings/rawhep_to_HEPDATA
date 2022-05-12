

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

if(ztable == "observers.effort") {
  wrangled_table <- wrangled_table %>% 
    select(-species)
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


#' Title
#'
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
check_multiple_brood_dates <- function(zyear) {
  
num_brood_size_days <- screened_hep$brood.sizes %>%  
  data.frame() %>% 
  filter(brd.size.date == TRUE) %>% 
  mutate(brd = paste("BRD", brd, sep = "")) %>% 
  pivot_wider(id_cols = c(code, species, date), values_from = num.nests, names_from = brd) %>% 
  count(code, species, date) %>% 
  mutate(out.message = paste(code, species, "\n")) %>% 
  filter(n > 1) %>% 
  distinct()

if(nrow(num_brood_size_days) > 0) {
  stop("multiple days selected for brood size for: \n", num_brood_size_days$out.message, "\nPlease edit appropriate Season Summary Sheets")
} 
}


#' Title
#'
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
check_multiple_rop_dates <- function(zyear) {
dup_rop <- screened_hep$stages %>%  
  filter(grepl("rop", which.rop)) %>% 
  distinct(code, species, date, which.rop) %>% 
  count(code, species, which.rop) %>% 
  filter(n > 1) %>% 
  mutate(out.message = paste(code, species))

if(nrow(dup_rop) > 0) {
  stop("multiple days selected for the same ROP for: \n", dup_rop$out.message, "\nPlease edit appropriate Season Summary Sheets")
} 
}


# if there are any records here, need to stop and fix before proceeding. can uncomment the view call to see code X species combos with multiple brood size dates
# fix will likely need to be returning to season summary form and editing the brood size table so only 1 date is selected

