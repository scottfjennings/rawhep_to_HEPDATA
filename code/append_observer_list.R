
library(tidyverse)
library(xlsx)
library(here)


#' Append list of HEP observers
#'
#' @param zyear as character
#'
#' @return saves csv to data/HEP_tracking_copy
#'
#' @examples
#' append_observer_list("2020")
append_observer_list <- function(zyear) {
observers <- read.xlsx(here("data/HEP_tracking_copy/HEP Tracking Spreadsheet2_shared.xlsx"), sheetName = zyear) %>% 
  filter(!is.na(SITE.CODE)) %>% 
  select(SITE.CODE, SITE.NAME, LAST.NAME, FIRST.NAME, Coordinator.) %>% 
  mutate(year = as.numeric(zyear))

if(file.exists(here("data/HEP_tracking_copy/observers.csv"))) {
  read.csv(here("data/HEP_tracking_copy/observers.csv")) %>% 
    bind_rows(observers) %>% 
    distinct() %>% 
    write.csv(here("data/HEP_tracking_copy/observers.csv"), row.names = FALSE)
} else {
  write.csv(observers, here("data/HEP_tracking_copy/observers.csv"), row.names = FALSE)
}
}



