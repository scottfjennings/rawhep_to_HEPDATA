

library(tidyverse)
library(here)
# zyear = 2020
# zcode = 699
# zspp = "DCCO"


append_species_not_nesting <- function(zyear, zcode, zspp) {

  if(file.exists(here(paste("season_summary_forms/sheet_creation_logs/species_not_nesting_", zyear, sep = "")))) {
    readRDS(here(paste("season_summary_forms/sheet_creation_logs/species_not_nesting_", zyear, sep = ""))) %>% 
      rbind(data.frame(year = zyear,
                   code = zcode,
                   species = zspp,
                   species.not.nesting = TRUE)) %>% 
      distinct() %>% 
      saveRDS(here(paste("season_summary_forms/sheet_creation_logs/species_not_nesting_", zyear, sep = "")))
  } else {
    data.frame(year = zyear,
                   code = zcode,
                   species = zspp,
                   species.not.nesting = TRUE) %>% 
      saveRDS(here(paste("season_summary_forms/sheet_creation_logs/species_not_nesting_", zyear, sep = "")))
  }
  
}


#append_species_not_nesting(zyear, zcode, zspp)

# append_species_not_nesting(2020, 53, "BCNH")

# several species for 1 colony:
# map2(c(2020), c(53), c("BCNH", "CAEG", "SNEG"), append_species_not_nesting)


# readRDS(here(paste("season_summary_forms/sheet_creation_logs/species_not_nesting_", zyear, sep = ""))) %>% view()



