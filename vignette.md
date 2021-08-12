How to process data from Survey123 to HEPDATA
================

This document describes the workflow for processing data from Survey123
to HEPDATA format. It begins assuming you have downloaded a .csv of the
current year’s Survey123 data and saved it as
data/downloaded/HEP\_\[year\]\_\[version number\].csv, where the version
number represents new versions created through proofing directly in
Survey123.

Processing the data from Survey123 to HEPDATA is comprised of 6 steps:

1.  Wrangle Survey123 data to usable format  
2.  Output Season Summary Sheet  
3.  Manually screen each Season Summary Sheet
4.  Extract screened data from Season Summary Sheets
5.  Generate log of screening changes
6.  Convert screened data to HEPDATA format

Each of these steps involves multiple sub-steps, and each major step has
its own code file where the functions and processes for each sub-step
are defined. Users should not need to access or open the major step code
files. Rather, the functions defined in these files should be called in
a separate script. The file Survey123\_to\_HEPDATA\_workflow.R provides
the recommended script, and this vignette has instructions and
additional information to proceed through the entire workflow.

## Setup

load the required packages, source the utility functions code file, and
indicate the year you will be processing.

``` r
library(tidyverse)
library(here)
```

    ## Warning: package 'here' was built under R version 4.0.3

``` r
source(here("code/survey123_utility_functions.r"))

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")

zyear = 2020
```

## Step 1, convert Survey123 data to more-usable format.

This step uses functions from step1\_wrangle\_survey123.R

``` r
source(here("code/step1_wrangle_survey123.r"))
```

Pipe together the functions defined in step1\_wrangle\_survey123.R  
This step fixes field names and date fields, adds a helper column to
indicate where multiple surveys happened on the same date, then finally
splits the data into different “types” (e.g. total nest numbers,
predator observations, etc.). These groups of “like” data are referred
to as data groups. Each data group requires different procedures and
manipulations downstream in the workflow.

``` r
wrangled_s123 <- read_s123(zyear, add.test.data = TRUE) %>% 
  filter(useforsummary == "y") %>% # 
  fix_s123_names() %>% 
  fix_s123_date_fields() %>% 
  add_multiple_survey_num() %>% 
  wrangle_s123()
```

The result of these functions is a list, with the elements of the list
being dataframes for each data group.

``` r
names(wrangled_s123)
```

    ## [1] "dates"            "observers.effort" "nests"            "stages"          
    ## [5] "brood.sizes"      "predators"        "disturbance"      "notes"

``` r
str(wrangled_s123$nests)
```

    ## 'data.frame':    1272 obs. of  7 variables:
    ##  $ code               : num  160 160 160 160 160 ...
    ##  $ date               : Date, format: "2020-03-13" "2020-03-13" ...
    ##  $ multiple.survey.num: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ species            : chr  "GREG" "GBHE" "SNEG" "BCNH" ...
    ##  $ total.nests        : num  0 7 0 0 0 0 0 0 0 0 ...
    ##  $ peak.active        : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ obs.initials       : chr  "BW; RM" "BW; RM" "BW; RM" "BW; RM" ...

Save wrangled\_s123 to the appropriate folder.

``` r
wrangled_s123 %>% saveRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
```

Next are some functions and code to check for any data issues and
prepare for manual screening.

Reload wrangled\_s123

``` r
wrangled_s123 <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))
```

Check which species have nested in each colony. This can help the
screener see if any unexpected species are nesting in a given colony, or
if a species has disappeared from a colony. \# for 1, a few, or all
colonies, check which species have nested there and peak active nests
for each year.

``` r
check_nesting_history(2020, c(53), screened.s123 = FALSE) %>% # from survey123_utility_functions.R
  pivot_wider(id_cols = c(code, site.name, year), values_from = total.nests, names_from = species) %>% 
  view()
```

    ## Joining, by = "code"

Check for unexpected values in each data group. Each element (dataframe)
of the list can be accessed with $.

``` r
# output of these calls not included here
summary(wrangled_s123$observers.effort)
summary(wrangled_s123$nests)
summary(wrangled_s123$stages)
summary(wrangled_s123$brood.sizes)
summary(wrangled_s123$predators)
summary(wrangled_s123$disturbance)
summary(wrangled_s123$notes)
```

Check start and end date match; if nrow = 0, no problem.

``` r
wrangled_s123$dates %>% 
  filter(as.Date(start, tz = Sys.timezone(location = TRUE)) != as.Date(end, tz = Sys.timezone(location = TRUE))) %>% 
  nrow()
```

    ## [1] 0

Check for multiple surveys at same site and same date. Such visits are
fine and consistent with the field protocol, and the screening code
should handle these multiple surveys fine, but nevertheless good to be
aware of before proceeding with screening so you can check to make sure
all the data made it through screening appropriately

``` r
wrangled_s123$dates %>% filter(num.surveys.this.date > 1) %>% view()
```

# 1.1.5 check observer X date X colony THIS CURRENTLY NOT WORKING —-

Check for time span between surveys, not yet developed, needed?

## Step 2, output wrangled Survey123 data into Season Summary sheets. This code creates a sheet for each instance where a species nested in a colony.

First, query wrangled\_s123 to get a list of all species in each colony
this year. Use this list to make season summary sheets only for species
that actually nested at each colony.

``` r
colony_spp <- readRDS(paste("data/wrangled/wrangled_s123", zyear, sep = "_"))$nests %>% 
  filter(total.nests > 0) %>% 
  distinct(code, species) %>% 
  mutate(year = zyear) %>% 
  arrange(code, species)
```

Rendering Season Summary Sheets to .docx files is the most
time-consuming process of this workflow. You can remove species X colony
instances from this list for sheets that have already been made by
querying which files already exist in the appropriate folder.

``` r
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
```

    ## Joining, by = c("code", "species", "year")

Finally, create season summary sheet for single colony X species.

``` r
#system.time(pmap(.l = list(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary))
# creating the 6 test data sheets takes ~30 seconds
```

Note: you can generate a Season Summary Sheet for a single species X
colony instance by specifying species and colony in the call to
render\_season\_summary:

``` r
#render_season_summary(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = 2020, zcode = 599, zspp = "GREG")
```

## step 3, NO CODE - MANUALLY SCREEN SEASON SUMMARY SHEETS.

  - When a sheet has been screened the first time, change its name from
    \[code\]*\[year\]*\[species\].docx to
    \[code\]*\[year\]*\[species\]\_screened1.docx
  - If a sheet is screened a second time, change its name from
    \[code\]*\[year\]*\[species\]*screened1.docx to
    \[code\]*\[year\]\_\[species\]\_screened2.docx

## Step 4, extract tables from screened Season Summary Sheets

This step uses functions in extract\_screened\_season\_summary.R

``` r
source(here("code/step4_extract_screened_season_summary.r"))
```

    ## Warning: package 'docxtractr' was built under R version 4.0.3

Create a list of Season Summary Sheets that have been screened (requires
file renaming in step 3).

``` r
  screened_seas_summ_files <- list.files(paste("season_summary_forms/", zyear, "/", sep = ""), pattern = "screened")
```

There are separate functions in
step4\_extract\_screened\_season\_summary.r for each data group. These
functions loop through each .docx file and extract the pertinent table,
then assemble those into a single data frame. Bundling those data frames
into a list brings the data back to the same structure and names as
wrangled\_s123, so that a log of screening changes can be easily made
(step 5) Note, running the code below will overwrite any previous
version of screened\_s123, it will not append new records to the
previous version. This generally shouldn’t be a problem (I can’t think
of a realistic scenario), but it is something to be aware of.

``` r
screened_s123 <- list(screen.log = map2_df(zyear, screened_seas_summ_files, get_screening_log),
                      observers.effort = map2_df(zyear, screened_seas_summ_files, get_observers_effort),
                      nests = map2_df(zyear, screened_seas_summ_files, get_total_nest_table),
                      stages = map2_df(zyear, screened_seas_summ_files, get_nest_stage_rop_table),
                      brood.sizes = map2_df(zyear, screened_seas_summ_files, get_stage4brd),
                      predators = map2_df(zyear, screened_seas_summ_files, get_predators) %>%  distinct(),
                      disturbance = map2_df(zyear, screened_seas_summ_files, get_disturbance) %>%  distinct(),
                      notes = map2_df(zyear, screened_seas_summ_files, get_notes) %>% distinct())  
```

    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")
    ## Joining, by = c("code", "species", "variable")

check the result

``` r
str(screened_s123)
```

    ## List of 8
    ##  $ screen.log      : tibble [6 x 6] (S3: tbl_df/tbl/data.frame)
    ##   ..$ year           : chr [1:6] "2020" "2020" "2020" "2020" ...
    ##   ..$ code           : chr [1:6] "599" "599" "599" "599" ...
    ##   ..$ species        : chr [1:6] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ screener.1     : chr [1:6] "SJ" "SJ" "SJ" "SJ" ...
    ##   ..$ screener.2     : chr [1:6] "not screened" "not screened" "not screened" "not screened" ...
    ##   ..$ screening.notes: chr [1:6] "test" "Test. No change" "" "" ...
    ##  $ observers.effort: tibble [6 x 7] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code         : num [1:6] 599 599 599 599 599 599
    ##   ..$ species      : chr [1:6] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ colony       : chr [1:6] "Test colony" "Test colony" "Test colony" "Test colony" ...
    ##   ..$ observers    : chr [1:6] "Scott Jennings; David Lumpkin" "Scott Jennings; David Lumpkin" "Scott Jennings; David Lumpkin" "Scott Jennings; David Lumpkin" ...
    ##   ..$ total.days   : num [1:6] 20 20 20 20 20 20
    ##   ..$ total.surveys: num [1:6] 20 20 20 20 20 20
    ##   ..$ total.hours  : num [1:6] 18.8 18.8 18.8 18.8 18.8 ...
    ##  $ nests           : tibble [120 x 7] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:120] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ date               : Date[1:120], format: "2020-03-01" "2020-03-03" ...
    ##   ..$ multiple.survey.num: num [1:120] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ species            : chr [1:120] "BCNH" "BCNH" "BCNH" "BCNH" ...
    ##   ..$ total.nests        : num [1:120] 8 28 40 16 16 20 40 44 36 40 ...
    ##   ..$ peak.active        : logi [1:120] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##   ..$ obs.initials       : chr [1:120] "SJ; DL" "SJ; DL" "SJ; DL" "SJ; DL" ...
    ##  $ stages          : tibble [200 x 7] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:200] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ species            : chr [1:200] "GBHE" "GBHE" "GBHE" "GBHE" ...
    ##   ..$ stage              : chr [1:200] "1" "1" "1" "1" ...
    ##   ..$ date               : Date[1:200], format: "2020-03-01" "2020-03-03" ...
    ##   ..$ multiple.survey.num: num [1:200] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ num.nests          : num [1:200] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..$ which.rop          : chr [1:200] "other" "other" "rop.1" "other" ...
    ##  $ brood.sizes     : tibble [60 x 8] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:60] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ date               : chr [1:60] "2020-05-25" "2020-05-25" "2020-05-25" "2020-05-25" ...
    ##   ..$ multiple.survey.num: num [1:60] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ species            : chr [1:60] "GBHE" "GBHE" "GBHE" "GBHE" ...
    ##   ..$ num.nests          : num [1:60] 1 1 1 0 0 5 5 5 1 0 ...
    ##   ..$ brd                : chr [1:60] "1" "2" "3" "4" ...
    ##   ..$ brd.size.date      : chr [1:60] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ stage5.nests       : chr [1:60] "NA" "NA" "NA" "NA" ...
    ##  $ predators       : tibble [42 x 5] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code            : num [1:42] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ species         : chr [1:42] "BCNH" "BCNH" "BCNH" "BCNH" ...
    ##   ..$ predator.species: chr [1:42] "RSHA" "AMCR" "RTHA" "CORA" ...
    ##   ..$ present         : chr [1:42] "1" "1" "1" "1" ...
    ##   ..$ nesting         : chr [1:42] NA NA NA NA ...
    ##  $ disturbance     : tibble [12 x 7] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code             : num [1:12] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ species          : chr [1:12] "BCNH" "BCNH" "CAEG" "CAEG" ...
    ##   ..$ date             : chr [1:12] "2020-03-19" "2020-04-11" "2020-03-19" "2020-04-11" ...
    ##   ..$ observed.inferred: chr [1:12] "observed" "observed" "observed" "observed" ...
    ##   ..$ description      : chr [1:12] "Chainsaws operated beneath colony. RTHA gone too." "GHOW roosting in (?) nest of GBHE" "Chainsaws operated beneath colony. RTHA gone too." "GHOW roosting in (?) nest of GBHE" ...
    ##   ..$ type             : chr [1:12] "h" "a" "h" "a" ...
    ##   ..$ result           : chr [1:12] "0" "2" "0" "2" ...
    ##  $ notes           : tibble [108 x 6] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:108] 599 599 599 599 599 599 599 599 599 599 ...
    ##   ..$ species            : chr [1:108] "BCNH" "BCNH" "BCNH" "BCNH" ...
    ##   ..$ date               : chr [1:108] "2020-03-01" "2020-03-03" "2020-03-08" "2020-03-19" ...
    ##   ..$ multiple.survey.num: num [1:108] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ note.type          : chr [1:108] "field" "field" "field" "field" ...
    ##   ..$ notes              : chr [1:108] "Copulation observed in nest #3." "1 GBHE standing on nest platform" "No activity" "No further visits: not enough to confirm inactivity." ...

And save it

``` r
saveRDS(screened_s123, here(paste("data/screened/screened_s123_", zyear, sep = "")))
```

## Step 5 generate screening change log.

The logic for this step is based on merging pre-screened and screened
data. Where this merge results in 2 records instead of 1, this indicates
the record was changed during screening.

This step uses functions defined in
step5\_make\_screening\_change\_log.R

``` r
source(here("code/step5_make_screening_change_log.R"))
```

As with step 4, a function is called for each data group, and the
results are combined into a list. Here the function is a simple join
that could be generalized for all data groups, so the same function is
called for all data groups.

``` r
track_changes_s123 <- list(screen.log = readRDS(here(paste("data/screened/screened_s123_", zyear, sep = "")))[["screen.log"]],
                           observers.effort = make_track_change_tables(zyear, "observers.effort"),
                           nests = make_track_change_tables(zyear, "nests") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           stages = make_track_change_tables(zyear, "stages") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           brood.sizes = make_track_change_tables(zyear, "brood.sizes") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           predators = make_track_change_tables(zyear, "predators") %>% 
                             arrange(code, species, predator.species),
                           disturbance = make_track_change_tables(zyear, "disturbance") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date),
                           notes = make_track_change_tables(zyear, "notes") %>% 
                             mutate(date = as.Date(date)) %>% 
                             arrange(code, species, date)
)
```

You can see that we have now added some additional fields to each data
group table to indicate whether records are in wrangled\_s123,
screened\_s123, or both, and whether a Season Summary Sheet was made.
Screened and changelog are helper columns, filled based the values of
record.in.wrangled, summary.sheet.made and record.in.screened, which can
help you quickly identify the status of each record.

``` r
str(track_changes_s123)
```

    ## List of 8
    ##  $ screen.log      : tibble [6 x 6] (S3: tbl_df/tbl/data.frame)
    ##   ..$ year           : chr [1:6] "2020" "2020" "2020" "2020" ...
    ##   ..$ code           : chr [1:6] "599" "599" "599" "599" ...
    ##   ..$ species        : chr [1:6] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ screener.1     : chr [1:6] "SJ" "SJ" "SJ" "SJ" ...
    ##   ..$ screener.2     : chr [1:6] "not screened" "not screened" "not screened" "not screened" ...
    ##   ..$ screening.notes: chr [1:6] "test" "Test. No change" "" "" ...
    ##  $ observers.effort: tibble [43 x 12] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code              : num [1:43] 1 1.1 3 5 9 15 40 47 47.1 51.1 ...
    ##   ..$ species           : chr [1:43] NA NA NA NA ...
    ##   ..$ colony            : chr [1:43] "Picher Canyon" "Picher Canyon parking lot trees" "Bell Canyon Reservoir" "Bodega Bay Flat Road #1" ...
    ##   ..$ observers         : chr [1:43] "Gwen Heistand" "Gwen Heistand" "Greg Raynor" "Ann Cassidy; Lauren Hammack; David Lumpkin" ...
    ##   ..$ total.days        : chr [1:43] "1" "1" "16" "2" ...
    ##   ..$ total.surveys     : chr [1:43] "1" "1" "16" "2" ...
    ##   ..$ total.hours       : chr [1:43] "0.25" "0.25" "15.92" "1.15" ...
    ##   ..$ record.in.wrangled: chr [1:43] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made: chr [1:43] NA NA NA NA ...
    ##   ..$ record.in.screened: chr [1:43] NA NA NA NA ...
    ##   ..$ screened          : chr [1:43] NA NA NA NA ...
    ##   ..$ changelog         : chr [1:43] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ nests           :'data.frame':    1276 obs. of  12 variables:
    ##   ..$ code               : num [1:1276] 1 1 1 1 1 1 1.1 1.1 1.1 1.1 ...
    ##   ..$ species            : chr [1:1276] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ date               : Date[1:1276], format: "2020-05-28" "2020-05-28" ...
    ##   ..$ multiple.survey.num: chr [1:1276] "1" "1" "1" "1" ...
    ##   ..$ total.nests        : chr [1:1276] "0" "0" "0" "0" ...
    ##   ..$ peak.active        : chr [1:1276] "FALSE" "FALSE" "FALSE" "FALSE" ...
    ##   ..$ obs.initials       : chr [1:1276] "GH" "GH" "GH" "GH" ...
    ##   ..$ record.in.wrangled : chr [1:1276] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made : chr [1:1276] NA NA NA NA ...
    ##   ..$ record.in.screened : chr [1:1276] NA NA NA NA ...
    ##   ..$ screened           : chr [1:1276] NA NA NA NA ...
    ##   ..$ changelog          : chr [1:1276] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ stages          :'data.frame':    2134 obs. of  12 variables:
    ##   ..$ code               : num [1:2134] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ species            : chr [1:2134] "GBHE" "GBHE" "GBHE" "GBHE" ...
    ##   ..$ date               : Date[1:2134], format: "2020-05-28" "2020-05-28" ...
    ##   ..$ multiple.survey.num: chr [1:2134] "1" "1" "1" "1" ...
    ##   ..$ num.nests          : chr [1:2134] "0" "0" "0" "0" ...
    ##   ..$ stage              : chr [1:2134] "1" "2" "3" "4" ...
    ##   ..$ which.rop          : chr [1:2134] "rop.5" "rop.5" "rop.5" "rop.5" ...
    ##   ..$ record.in.wrangled : chr [1:2134] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made : chr [1:2134] NA NA NA NA ...
    ##   ..$ record.in.screened : chr [1:2134] NA NA NA NA ...
    ##   ..$ screened           : chr [1:2134] NA NA NA NA ...
    ##   ..$ changelog          : chr [1:2134] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ brood.sizes     : tibble [2,184 x 13] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:2184] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ species            : chr [1:2184] "GBHE" "GBHE" "GBHE" "GBHE" ...
    ##   ..$ date               : Date[1:2184], format: "2020-05-28" "2020-05-28" ...
    ##   ..$ multiple.survey.num: chr [1:2184] "1" "1" "1" "1" ...
    ##   ..$ num.nests          : chr [1:2184] "0" "0" "0" "0" ...
    ##   ..$ brd                : chr [1:2184] "1" "2" "3" "4" ...
    ##   ..$ brd.size.date      : chr [1:2184] NA NA NA NA ...
    ##   ..$ stage5.nests       : chr [1:2184] NA NA NA NA ...
    ##   ..$ record.in.wrangled : chr [1:2184] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made : chr [1:2184] NA NA NA NA ...
    ##   ..$ record.in.screened : chr [1:2184] NA NA NA NA ...
    ##   ..$ screened           : chr [1:2184] NA NA NA NA ...
    ##   ..$ changelog          : chr [1:2184] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ predators       : tibble [408 x 10] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code              : num [1:408] 1 1 1 1 1 1 1.1 1.1 1.1 1.1 ...
    ##   ..$ species           : chr [1:408] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ predator.species  : chr [1:408] NA NA NA NA ...
    ##   ..$ present           : chr [1:408] NA NA NA NA ...
    ##   ..$ nesting           : chr [1:408] NA NA NA NA ...
    ##   ..$ record.in.wrangled: chr [1:408] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made: chr [1:408] NA NA NA NA ...
    ##   ..$ record.in.screened: chr [1:408] NA NA NA NA ...
    ##   ..$ screened          : chr [1:408] NA NA NA NA ...
    ##   ..$ changelog         : chr [1:408] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ disturbance     : tibble [378 x 14] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:378] 1 1 1 1 1 1 1.1 1.1 1.1 1.1 ...
    ##   ..$ species            : chr [1:378] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ date               : Date[1:378], format: NA NA ...
    ##   ..$ multiple.survey.num: chr [1:378] NA NA NA NA ...
    ##   ..$ obs.inf            : chr [1:378] NA NA NA NA ...
    ##   ..$ type               : chr [1:378] NA NA NA NA ...
    ##   ..$ result             : chr [1:378] NA NA NA NA ...
    ##   ..$ description        : chr [1:378] NA NA NA NA ...
    ##   ..$ observed.inferred  : chr [1:378] NA NA NA NA ...
    ##   ..$ record.in.wrangled : chr [1:378] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made : chr [1:378] NA NA NA NA ...
    ##   ..$ record.in.screened : chr [1:378] NA NA NA NA ...
    ##   ..$ screened           : chr [1:378] NA NA NA NA ...
    ##   ..$ changelog          : chr [1:378] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...
    ##  $ notes           : tibble [1,086 x 11] (S3: tbl_df/tbl/data.frame)
    ##   ..$ code               : num [1:1086] 1 1 1 1 1 1 1.1 1.1 1.1 1.1 ...
    ##   ..$ species            : chr [1:1086] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##   ..$ date               : Date[1:1086], format: "2020-05-28" "2020-05-28" ...
    ##   ..$ multiple.survey.num: chr [1:1086] "1" "1" "1" "1" ...
    ##   ..$ note.type          : chr [1:1086] "field" "field" "field" "field" ...
    ##   ..$ notes              : chr [1:1086] "Gwen Heistand confirmed still inactive in 2020 in email to SJ on 5/28/20. Her observation is from regular (dail"| __truncated__ "Gwen Heistand confirmed still inactive in 2020 in email to SJ on 5/28/20. Her observation is from regular (dail"| __truncated__ "Gwen Heistand confirmed still inactive in 2020 in email to SJ on 5/28/20. Her observation is from regular (dail"| __truncated__ "Gwen Heistand confirmed still inactive in 2020 in email to SJ on 5/28/20. Her observation is from regular (dail"| __truncated__ ...
    ##   ..$ record.in.wrangled : chr [1:1086] "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##   ..$ summary.sheet.made : chr [1:1086] NA NA NA NA ...
    ##   ..$ record.in.screened : chr [1:1086] NA NA NA NA ...
    ##   ..$ screened           : chr [1:1086] NA NA NA NA ...
    ##   ..$ changelog          : chr [1:1086] "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...

You can filter based on changelog to see the records that differed
between wrangled\_s123 and screened\_s123. The main fields we expect to
change in screening are:

  - peak active date
  - ROP days
  - stage 4 date

The numeric nest count fields should not change, so be sure to scan
those to make sure they didn’t change accidentally

``` r
filter(track_changes_s123$nests, grepl("changed", changelog)) %>% view()
```

saveRDS(track\_changes\_s123,
here(paste(“data/track\_changes/track\_changes\_s123\_”, zyear, sep =
"")))

## Step 6 convert to HEPDATA

HEPDATA is a “wide” data structure, and has field names in ALLCAPS. Thus
this comprises a bunch of reshaping and renaming.

``` r
source(here("code/step6_screened_to_HEPDATA.R"))

HEPDATA <- screened_to_HEPDATA(zyear)
```

    ## Joining, by = "code"

    ## Joining, by = c("CODE", "SPECIES", "YEAR")
    ## Joining, by = c("CODE", "SPECIES", "YEAR")

    ## Joining, by = "which.rop"

    ## Joining, by = c("YEAR", "code", "species")

    ## Joining, by = c("CODE", "SPECIES", "YEAR")

    ## Joining, by = "dist.num"

    ## Joining, by = c("CODE", "SPECIES", "YEAR")

    ## Joining, by = c("CODE", "SPECIES")

    ## Joining, by = c("SITE", "CODE", "SPECIES", "NUMBERVISITS", "TOTALHOURS", "YEAR", "PEAKACTVNSTS", "BRD1", "BRD2", "BRD3", "BRD4", "BRD5", "MARSTGEDATE", "MARSTAGE1", "MARSTAGE2", "MARSTAGE3", "MARSTAGE4", "MARSTAGE5", "LTMARSTGDATE", "LTMARSTAGE1", "LTMARSTAGE2", "LTMARSTAGE3", "LTMARSTAGE4", "LTMARSTAGE5", "APRSTGEDATE", "APRSTAGE1", "APRSTAGE2", "APRSTAGE3", "APRSTAGE4", "APRSTAGE5", "MAYSTGEDATE", "MAYSTAGE1", "MAYSTAGE2", "MAYSTAGE3", "MAYSTAGE4", "MAYSTAGE5", "JUNSTGEDATE", "JUNSTAGE1", "JUNSTAGE2", "JUNSTAGE3", "JUNSTAGE4", "JUNSTAGE5", "LTJUNSTGDATE", "LTJUNSTAGE1", "LTJUNSTAGE2", "LTJUNSTAGE3", "LTJUNSTAGE4", "LTJUNSTAGE5", "DIST1DATE", "DIST1TYPE", "DIST1RESULT", "DIST2DATE", "DIST2TYPE", "DIST2RESULT", "DIST3DATE", "DIST3TYPE", "DIST3RESULT", "DIST4DATE", "DIST4TYPE", "DIST4RESULT", "DIST5DATE", "DIST5TYPE", "DIST5RESULT", "DIST6DATE", "DIST6TYPE", "DIST6RESULT", "DIST7DATE", "DIST7TYPE", "DIST7RESULT", "DIST8DATE", "DIST8TYPE", "DIST8RESULT", "GHOWNESTING", "Entry_Proofed", "Entered_By")
