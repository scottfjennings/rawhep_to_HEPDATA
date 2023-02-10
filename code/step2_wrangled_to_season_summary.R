


#' get_colony_spp_need_sheet
#' 
#' make list of colony X species combos that still need a Season Summary Sheet made. Creates a record for each species X colony combination where at least 1 nest was observed on at least one visit in zyear.
#' 
#' 
#' @param zyear 
#' @param include.already.made 
#'
#' @return
#' @export
#'
#' @examples
get_colony_spp_need_sheet <- function(zyear, include.already.made = FALSE) {
colony_spp <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))$nests 

#if(include.inactive == FALSE) { pretty sure this part of the function isn't needed. if it is, add this param back include.inactive = FALSE, 
  colony_spp <- colony_spp %>% 
    filter(total.nests > 0) 
#}
 colony_spp <- colony_spp %>% 
   distinct(code, species) %>% 
  mutate(year = zyear) %>% 
  arrange(code, species)

# remove colony X species that already have sheet made
if(include.already.made == FALSE & length(list.files(paste("season_summary_forms/", zyear, "/", sep = ""))) > 0) {
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





#' get_inactive_need_sheet
#' 
#' make list of colony X species combos that were not active in zyear but that still need a Season Summary Sheet made. Only want to render 1 season summary sheet for each inactive colony, so need to pick which species to render for. First picks most recent year colony was active; if only one species that year, picks that species. If >1 species in final year, picks the species with max(peakactvnsts). If there ties in peakactvnsts, picks species in this order c("GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO").
#'
#' @param zyear 
#' @param include.already.made if FALSE, does not re-render sheets that already exist in the appropriate season_summary_forms folder. If TRUE, overwrites files that already exist but that have not had "_screened" added to their file name.
#'
#' @return
#' @export
#'
#' @examples
#' inactive_colony_need_sheet <- get_inactive_colony_need_sheet(zyear, include.already.made = FALSE)
get_inactive_colony_need_sheet <- function(zyear, include.already.made = FALSE) {
inactive_need_sheet <- get_surveyed_inactive(zyear) %>% 
  left_join(readRDS(here("data/support_data/hep_annual_nest_abundance"))) %>%
  filter(peakactvnsts > 0) %>%
  group_by(code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  # next 6 lines select the species with the most nests if multiple spp in last year, and for ties in peakactvnsts takes species in this order c("GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO")
  mutate(species = factor(species, levels = c("GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO"))) %>%  
  arrange(code, peakactvnsts, rev(species)) %>% 
  group_by(code) %>%
  mutate(zrank = row_number()) %>% 
  filter(zrank == max(zrank)) %>% 
  ungroup() %>% 
  rename(most.recent.year = year,
         most.recent.peakactvnsts = peakactvnsts) %>%
  select(-zrank) %>% 
  mutate(year = zyear) 



# remove colony X species that already have sheet made
if(include.already.made == FALSE & length(list.files(paste("season_summary_forms/", zyear, "/", sep = ""))) > 0) {
inactive_colony_spp_need_sheet <- inactive_need_sheet %>% 
  anti_join(., list.files(paste("season_summary_forms/", zyear, "/", sep = "")) %>% 
              data.frame() %>% 
              rename(file.name = 1) %>% 
              mutate(file.name = gsub(".docx", "", file.name)) %>%
              separate(file.name, into = c("code", "year", "species", "screened"), sep = "_") %>% 
              mutate(across(c(code, year), as.numeric))) 
} else {
  inactive_colony_spp_need_sheet <- inactive_need_sheet
}

return(inactive_colony_spp_need_sheet)


}
