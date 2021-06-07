






peakactvnsts <- filter(total_nests, peak.active == 1) %>% 
  select(YEAR = year, CODE = code, SPECIES = species, PEAKACTVNSTS = total.nests)
  

rop_dates <- total_nests %>% 
  select(year, code, date, rop) %>% 
  filter(rop != "other")

###
rop_stages <- nest_stages %>% 
  full_join(rop_dates) %>% 
  filter(!is.na(rop))

