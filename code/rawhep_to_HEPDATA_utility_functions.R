

# data checking functions ----

#' Check observer X date X colony combinations for unexpected observers ----


# uses tracking sheet - requires that to be accurate
# This function finds date X colony instances in Survey123 data where the ONLY observer is not expected based on the tracking sheet. For each [date X colony X observer name] combo in Survey123, check the observer name against the list of observers assigned to that site (from the HEP tracking sheet: https://egretorg-my.sharepoint.com/:x:/g/personal/emiko_condeso_egret_org/EbSWdIJ1wLdHhDV1EI1k50sBOPg0sU_UP9BNFVIcicBijg?rtime=wV3D_o772Eg); can't read tracking sheet directly from www, so need to copy to csv first. Also requires wrangled Survey123 file be saved. Stops with message if either file doesn't exist. Function reads both files so neither need to be in environment.

#' Title
#'
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
check_expected_observers <- function(zyear) {
  if(!file.exists(here(paste("data/hep_tracking_", zyear, ".csv", sep = "")))) {
    stop(cat(paste("Please copy HEP tracking sheet for ", zyear, " to a csv saved as \"HEP_tracking_", zyear, ".csv\" in the data folder.", sep = "")))
  }
  if(!file.exists(here(paste("data/wrangled/wrangled_s123_", zyear, sep = "")))) {
    stop(cat(paste("Please process Survey123 data for ", zyear, " and save as \"data/wrangled/wrangled_s123_", zyear, ".RDS\".", sep = "")))
  }
tracking <- read.csv(here(paste("data/hep_tracking_", zyear, ".csv", sep = "")))
# first 3 rows contain header stuff
tracking <- tracking[4:nrow(tracking),]
# only need a few columns, then collapse to a single r
tracking <- tracking %>% 
  mutate(assigned.name = paste(FIRST.NAME, LAST.NAME, sep = "")) %>% 
  select(code = SITE.CODE, SITE.NAME, assigned.name) %>% 
  group_by(code, SITE.NAME) %>% 
  summarise(assigned.name = paste(assigned.name, collapse = '')) %>% 
  ungroup() %>% 
  mutate(assigned.name = gsub(" ", "", assigned.name),
         assigned.name = gsub("KatieClas", "KathleenClas", assigned.name))

check_observers_sites <- readRDS(here(paste("data/wrangled/wrangled_s123", zyear, sep = "_")))$observers %>% 
  distinct() %>% 
  filter(name != "other", !grepl("Other", role)) %>% 
  mutate(name = gsub(" ", "", name),
         code = as.numeric(code)) %>%   
  left_join(tracking, by = c("code")) %>% 
  rowwise() %>% 
# because of the way names are entered in the tracking sheet, need to do 2-way partial string matching with next line
  mutate(expected.observer = grepl(name, assigned.name) | grepl(assigned.name, name)) %>% 
  arrange(code) %>% 
  filter(code != 599) %>% # ditch test data
  # finally check whether any date X colony had ONLY unexpected observers
  group_by(code, SITE.NAME, date) %>% 
  mutate(only.unexpected.observer = sum(expected.observer) < 1)
}

#' Check nesting history
#'
#' Check which species have nested in one or more colonies.
#'
#' @param zyear The year you are currently working on
#' @param zcode The colony code you wish to check. if NA will return history for all colonies; can be a string of multiple colony codes
#' @param screened.s123 if FALSE will use the unscreened data, as they exist in Survey123; if TRUE will use the screened data, as it exists going into HEPDATA
#'
#' @return data frame with the following columns: code, year, species, peakactvnsts. For the current year, 
#' @export
#'
#' @examples
#' # 3 ways to specify zcode:
#' # all colonies
#' nesting_history <- check_nesting_history(2020, screened.s123 = FALSE)
#' # one colony
#' nesting_history <- check_nesting_history(2020, 53, screened.s123 = FALSE)
#' # multiple colonies but not all
#' nesting_history <- check_nesting_history(2020, c(53, 53.1), screened.s123 = FALSE)
check_nesting_history <- function(zyear, zcode = NA, screened.s123 = FALSE) {
  

  if(screened.s123 == FALSE) {
    s123 <- readRDS(here(paste("data/wrangled/wrangled_s123_", zyear, sep = "")))
  } 
  if(screened.s123 == TRUE) {
    s123 <- readRDS(here(paste("data/screened/screened_s123_", zyear, sep = "")))
  } 
  
  nesting_history <- s123$nests %>%
    filter(peak.active == TRUE) %>% 
    left_join(., readRDS(here("data/HEP_site_names_nums_utm")) %>% select(code, site.name)) %>% 
    distinct(code, site.name, species, total.nests) %>% 
    mutate(year = zyear) %>% 
    rbind(., readRDS(here("data/hep_annual_nest_abundance")) %>%
            rename(total.nests = peakactvnsts)) %>% 
    arrange(code, year, species)
    
  if(is.null(zcode) | is.na(zcode)) {
    nesting_history <- nesting_history
  } else {
    nesting_history <- filter(nesting_history, code %in% zcode)
  }
return(nesting_history)
  
  }

# this one probably not needed
#check_survey_during_day <- function(zyear, zcode) {
# this probably not needed
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
#}

# make list of colony X species combos that still need a Season Summary Sheet made ----
get_colony_spp_need_sheet <- function(zyear, include.inactive = TRUE) {
colony_spp <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))$nests 

if(include.inactive == FALSE) {
  colony_spp <- colony_spp %>% 
    filter(total.nests > 0) 
}
 colony_spp <- colony_spp %>% 
   distinct(code, species) %>% 
  mutate(year = zyear) %>% 
  arrange(code, species)

# remove colony X species that already have sheet made
if(length(list.files(paste("season_summary_forms/", zyear, "/", sep = ""))) > 0) {
colony_spp_need_sheet <- colony_spp %>% 
  anti_join(., list.files(paste("season_summary_forms/", zyear, "/", sep = "")) %>% 
              data.frame() %>% 
              rename(file.name = 1) %>% 
              mutate(file.name = gsub(".docx", "", file.name)) %>%
              separate(file.name, into = c("code", "year", "species", "screened"), sep = "_") %>% 
              mutate(across(c(code, year), as.numeric))) 
} else {
  colony_spp_need_sheet <- colony_spp
}

return(colony_spp_need_sheet)
}


# function to output season summary sheet for a given year, colony and species ----
# files must be .docx. .doc will not work without downloading LibreOffice software
render_season_summary <- function(file = here("code/step2_wrangled_to_season_summary.Rmd"), zyear, zcode, zspp) {
  rmarkdown::render(file, params = list(
    zyear = zyear,
    zcode = zcode,
    zspp = zspp
  ), envir = new.env(),
  output_file = here(paste0("season_summary_forms/", zyear, "/", zcode, "_", zyear, "_", zspp, ".docx", sep = ""))
  )
}



# function to output season summary sheet for a given year, colony and species ----
# files must be .docx. .doc will not work without downloading LibreOffice software
render_summary_for_observer <- function(file = here("code/summary_for_observer.Rmd"), zyear, zcode, zcol.name) {
  zcode.sub = gsub("\\.", "_", zcode)
  rmarkdown::render(file, params = list(
    zyear = zyear,
    zcode = zcode
  ), envir = new.env(),
  output_file = here(paste0("summary_for_observers/", zyear, "/", zcode.sub, "_", zyear, "_", zcol.name, ".pdf", sep = ""))
  )
}



# disturbance codes ----
disturbance_code_to_text <- function(dist.code.field) {
dist.code.field = toupper(dist.code.field)
  dist.code.field = case_when(dist.code.field == "A" ~ "Avian",
                              dist.code.field == "H" ~ "Human",
                              dist.code.field == "W" ~ "Weather",
                              dist.code.field == "M" ~ "Mammal", 
                              dist.code.field == "O" ~ "ACR field observer", 
                              dist.code.field == "P" ~ "Unknown Predator", 
                              dist.code.field == "U" ~ "unknown")
  return(dist.code.field)
}


disturbance_response_to_text <- function(dist.response.field) {
  dist.response.field = case_when(dist.response.field == 0 ~ "none", 
                                  dist.response.field == 1 ~ "behavioral response", 
                                  dist.response.field == 2 ~ "nest failure",
                                  dist.response.field == 3 ~ "abandonment of colony", 
                                  dist.response.field == 4 ~ "preseason disturbance")
return(dist.response.field)
  }


# get terrestrial predators
get_terrestrial_predators <- function(x) {
  
  
terr_preds <- c("raccoon", "cat", "coyote", "eagle_sp", "fox")  

terr_pred_str <- paste(terr_preds, collapse = "|")
  
out_names <- paste(str_extract_all(tolower(x), terr_pred_str[[1]]))
out_names <- gsub("[()]", "", out_names)
out_names <- gsub(paste(c("character0", '"'), collapse = "|"), "", out_names)
out_names <- gsub(", ", "_", out_names)
out_names <- gsub("eagle_sp", "eagle species", out_names)
  
}



# generate list of season summary sheets that have been screened

get_screened_col_spp <- function(zyear) {
screened_col_spp <- readRDS(here(paste("data/screened/screened_hep_", zyear, sep = "")))$screen.log %>%
  mutate(screened = ifelse(screener.1 != "not screened", TRUE, FALSE)) %>% 
  filter(screened == TRUE) %>% 
  select(code, species, screened)
}




#' Date table splitter
#'
#' Split tables with many date columns into multiple tables for better display in Season Summary .doc files or other output. 
#'
#' @param ztable the wide format table to be split (one column per date)
#' @param num_dates_per_split number of data columns per subtable
#' @param num.lead.cols number of lead columns
#'
#' @return list with each element being one of the split subtables. Currently the list is always 5 elements long. I haven't yet figured out how to make it dynamic to the number of subtables needed.
#' @export
#'
#' @details Table to be split can have a variable number of lead columns and variable number of date columns; both of these values are specified. Function splits the date columns into groups of num_dates_per_split, and create multiple sub-tables as the concatenation of the same lead columns and each successive group of date columns.
#' 
#' will probably work fine to split other wide format tables with mutliple columns for repeated data, but has not been tested for this.
#'
#' @examples
date_table_splitter <- function(ztable, num.lead.cols = 3, num_dates_per_split = 6) {
num.splits <- ceiling((ncol(ztable) - num.lead.cols) / num_dates_per_split)
seq.splits <- seq(1, num.splits)
col.splits <- num.lead.cols + (num_dates_per_split * seq.splits)
lead.cols <- c(1:num.lead.cols)
ztable1_cols <- c(lead.cols, (num.lead.cols + 1):col.splits[1])

make_date_flextable <- function(date_subtable) {
  out_date_flextable <- date_subtable %>%
    flextable() %>%
  autofit() %>% 
  fit_to_width(max_width = 8.5) #%>% 
  #width(j = 1:3, width = lead.col.width) %>% 
  #width(j = 4:ncol(date_subtable), width = date.col.width)
return(out_date_flextable)
  }

# define sub-table 1 columns
  if(ncol(ztable) <= col.splits[1]){
sub_table1 <- ztable[,c(1:ncol(ztable))]
  }  
  if(ncol(ztable) > col.splits[1]){
sub_table1 <- ztable[,c(1:col.splits[1])]
}

# define sub-table 2 columns
if(length(col.splits) > 1){
if(ncol(ztable) > col.splits[1]) {
  if(ncol(ztable) <= col.splits[2]){
sub_table2 <- ztable[,c(lead.cols, (col.splits[1]+1):ncol(ztable))]
  }  
  if(ncol(ztable) > col.splits[2]){
sub_table2 <- ztable[,c(lead.cols, (col.splits[1]+1):col.splits[2])]
}
}
}
# define sub-table 3 columns
if(length(col.splits) > 2) {
if(ncol(ztable) > col.splits[2]) {
  if(ncol(ztable) <= col.splits[3]){
sub_table3 <- ztable[,c(lead.cols, (col.splits[2]+1):ncol(ztable))]
  }  
  if(ncol(ztable) > col.splits[3]){
sub_table3 <- ztable[,c(lead.cols, (col.splits[2]+1):col.splits[3])]
}
}
}
# define sub-table 4 columns
if(length(col.splits) > 3) {
if(ncol(ztable) > col.splits[3]) {
  if(ncol(ztable) <= col.splits[4]){
sub_table4 <- ztable[,c(lead.cols, (col.splits[3]+1):ncol(ztable))]
  }  
  if(ncol(ztable) > col.splits[4]){
sub_table4 <- ztable[,c(lead.cols, (col.splits[3]+1):col.splits[4])]
}
}
}
# define sub-table 5 columns
if(length(col.splits) > 4) {
if(ncol(ztable) > col.splits[4]) {
  if(ncol(ztable) <= col.splits[5]){
sub_table5 <- ztable[,c(lead.cols, (col.splits[4]+1):ncol(ztable))] %>% 
  make_date_flextable()
  }  
  if(ncol(ztable) > col.splits[5]){
sub_table5 <- ztable[,c(lead.cols, (col.splits[4]+1):col.splits[5])] %>% 
  make_date_flextable()
}
} 
}

#--- make sub table 1
if(exists("sub_table1")) {
sub_table1 <- sub_table1 %>% 
  make_date_flextable() 
} else {
  sub_table1 <- NULL
}
#--- make sub table 2
if(exists("sub_table2")) {
sub_table2 <- sub_table2 %>% 
  make_date_flextable() 
} else {
  sub_table2 <- NULL
}
#--- make sub table 3
if(exists("sub_table3")) {
sub_table3 <- sub_table3 %>% 
  make_date_flextable()
} else {
  sub_table3 <- NULL
}
#--- make sub table 4
if(exists("sub_table4")) {
sub_table4 <- sub_table4 %>% 
  make_date_flextable()
} else {
  sub_table4 <- NULL
}
#--- make sub table 5
if(exists("sub_table5")) {
  sub_table5 <- sub_table5 %>% 
  make_date_flextable()
} else {
  sub_table5 <- NULL
}

out_sub_tables <- list(sub_table1 = sub_table1,
                       sub_table2 = sub_table2,
                       sub_table3 = sub_table3,
                       sub_table4 = sub_table4,
                       sub_table5 = sub_table5)
return(out_sub_tables)
}



# convert date to month-day
date2monthday <- function(df) {
  df <- df %>% 
  mutate(date = as.character(date),
         date = gsub(paste(zyear, "-", sep = ""), "", date),
         date = ifelse(multiple.survey.num > 1, paste(date, multiple.survey.num, sep = "."), date))
}

monthday_completecount <- function(df) {
    df <- df %>%
      mutate(date = ifelse(complete.count == "no", paste(date, "*", sep = ""), date))
}


