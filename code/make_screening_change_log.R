

# compare screened to unsreened to generate a list of changes made during screening

# logic is based on merging pre-screeded and screened data. where this merge results in 2 records instead of 1, this indicates the record was changed during screening

library(tidyverse)
library(here)

zyear = 2020
screened_s123 <- readRDS(paste("data/screened/screened_s123_", zyear))

screen_log <- screened_s123$screen_log %>% 
  mutate(code = as.numeric(code),
         year = as.numeric(year),
         record.in.screened = ifelse(screener.1 != "not screened", TRUE, FALSE)) %>% 
  select(-contains("screener"))

wrangled_s123 <- readRDS(here(paste("data/wrangled/wrangled_s123", zyear, sep = "_")))

sheet_creation_log <- readRDS(here(paste("season_summary_forms/sheet_creation_logs/sheet_creation_log_", zyear, sep = ""))) %>% 
  mutate(code = as.numeric(code),
         year = as.numeric(year),
         summary.sheet.made = TRUE) %>% 
  select(-date.sheet.created)


# total nests ----
orig_total_nests <- wrangled_s123$nests %>%  
  mutate(record.in.prescreened = TRUE,
         year = zyear)
 
screened_total_nests <- screened_s123$total_nests %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-rop) %>%
  full_join(screen_log) 



total_nests_changelog <- full_join(orig_total_nests, sheet_creation_log) %>% 
  full_join(screened_total_nests) %>% 
  select(year, everything()) %>% 
  arrange(code, species, date)   %>% 
  group_by(code, species) %>% 
  mutate(species.not.nesting = ifelse(max(total.nests) == 0, TRUE, FALSE)) %>% 
  mutate(changelog = case_when(species.not.nesting == TRUE ~ "species not nesting",
                               record.in.prescreened == TRUE & is.na(summary.sheet.made) ~ "no summary sheet",
                               record.in.screened == TRUE & is.na(record.in.prescreened) ~ "screened, changed to",
                               is.na(record.in.screened) & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, changed from",
                               record.in.screened == TRUE & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, no change"))  

# rop ----

orig_rop <- wrangled_s123$dates %>% 
  select(code, date, which.rop)%>% 
  mutate(year = zyear,
         record.in.prescreened = TRUE,
         GBHE = 1,
         GREG = 1,
         SNEG = 1,
         BCNH = 1,
         CAEG = 1,
         DCCO = 1) %>% 
  pivot_longer(cols = c("GBHE", "GREG", "SNEG", "BCNH", "CAEG", "DCCO"), names_to = "species") %>% 
  select(-value) %>% 
  mutate(which.rop = ifelse(is.na(which.rop), "other", which.rop))

screened_rop <- screened_s123$total_nests %>% 
  select(code, date, species, which.rop = rop) %>%
  full_join(screen_log)

rop_changelog <- full_join(orig_rop, sheet_creation_log) %>%
  full_join(., screened_rop) %>% 
  full_join(., filter(total_nests_changelog, species.not.nesting == TRUE) %>% distinct(code, species, species.not.nesting)) %>% 
  arrange(code, species, date)%>% 
  mutate(changelog = case_when(species.not.nesting == TRUE ~ "species not nesting",
                               record.in.prescreened == TRUE & is.na(summary.sheet.made) ~ "no summary sheet",
                               record.in.screened == TRUE & is.na(record.in.prescreened) ~ "screened, changed to",
                               is.na(record.in.screened) & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, changed from",
                               record.in.screened == TRUE & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, no change"))   %>% 
  relocate(c("record.in.prescreened", "summary.sheet.made"), .before = record.in.screened)
  
# nest stages ----
orig_nest_stages <- wrangled_s123$stages %>%  
  mutate(stage = paste("stage.", stage, sep = "")) %>% 
  pivot_wider(values_from = num.nests, names_from = stage) %>% 
  mutate(record.in.prescreened = TRUE,
         year = zyear)
 
screened_nest_stages <- screened_s123$nest_stages %>% 
  ungroup()  %>% 
  mutate(code = as.numeric(code),
         year = as.numeric(year)) %>%
  mutate(across(contains("stage"), as.numeric)) %>% 
  full_join(., screen_log) 
  


nest_stages_changelog <- full_join(orig_nest_stages, sheet_creation_log) %>% 
  full_join(screened_nest_stages) %>% 
  full_join(., filter(total_nests_changelog, species.not.nesting == TRUE) %>% select(code, species, species.not.nesting)) %>% 
  filter(species %in% c("GREG", "GBHE")) %>% 
  select(year, everything()) %>% 
  arrange(code, species, date)  %>% 
  mutate(changelog = case_when(species.not.nesting == TRUE ~ "species not nesting",
                               record.in.prescreened == TRUE & is.na(summary.sheet.made) ~ "no summary sheet",
                               record.in.screened == TRUE & is.na(record.in.prescreened) ~ "screened, changed to",
                               is.na(record.in.screened) & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, changed from",
                               record.in.screened == TRUE & record.in.prescreened == TRUE & summary.sheet.made == TRUE ~ "screened, no change"))  %>% 
  relocate(c("record.in.prescreened", "summary.sheet.made"), .before = record.in.screened)











