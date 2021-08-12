How to process data from Survey123 to HEPDATA
================
2021-08-12

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

This script relies heavily on the pipe operator and on the purrr::map
family of functions. The map functions allow iterating the same process
over many instances. In this script you will iterate over colony X
species combinations in multiple steps.

## Prepare workspace

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

This step uses functions from step1\_wrangle\_survey123.R, which can be
piped together into a single process.

``` r
source(here("code/step1_wrangle_survey123.r"))
```

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

### Step 1.1, Check incoming data

Next are some functions and code to check for any data issues and
prepare for manual screening. These checks are largely meant to identify
“proofing” problems that need to be fixed directly in Survey123.

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

Check observer X date X colony THIS CURRENTLY NOT WORKING

Check for time span between surveys, not yet developed, needed?

## Step 2, output wrangled Survey123 data into Season Summary sheets.

We want to create a sheet for each instance where a species nested in a
colony.

The function get\_colony\_spp\_need\_sheet() queries wrangled\_s123 to
get a list of all species in each colony this year, and queries the
year-appropriate season\_summary\_forms folder to create a list of
colony X species that still need a Season Summary Sheet created.
Rendering Season Summary Sheets to .docx files is the most
time-consuming process of this workflow, so it is beneficial to only
render each sheet once.

``` r
colony_spp_need_sheet <- get_colony_spp_need_sheet(zyear)
```

    ## Joining, by = c("code", "species", "year")

The colony\_spp\_need\_sheet object has the colony and species fields
for purrr::map to iterate over to create each sheet.

``` r
pmap(.l = list(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = colony_spp_need_sheet$year, zcode = colony_spp_need_sheet$code, zspp = colony_spp_need_sheet$species), .f = render_season_summary)
```

Note: you can generate a Season Summary Sheet for a single species X
colony instance by specifying species and colony in the call to
render\_season\_summary:

``` r
render_season_summary(file = here("code/step2_wrangled_s123_to_season_summary.Rmd"), zyear = 2020, zcode = 599, zspp = "GREG")
```

## Step 3, NO CODE - MANUALLY SCREEN SEASON SUMMARY SHEETS.

  - When a sheet has been screened the first time, change its name from
    \[code\]*\[year\]*\[species\].docx to
    \[code\]*\[year\]*\[species\]\_screened1.docx
  - If a sheet is screened a second time, change its name from
    \[code\]*\[year\]*\[species\]*screened1.docx to
    \[code\]*\[year\]\_\[species\]\_screened2.docx

This renaming helps prevent accidental overwriting of already screened
summary sheets (e.g. when rendering a single sheet), and also helps
identify which files in the year-appropriate season\_summary\_forms
folder still need to be screened.

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

Check the result. You will see that screened\_s123 does not contain a
dates table, but it does contain a screening.log table. The latter is
the top table on the Season Summary Sheet.

``` r
names(screened_s123)
```

    ## [1] "screen.log"       "observers.effort" "nests"            "stages"          
    ## [5] "brood.sizes"      "predators"        "disturbance"      "notes"

Save screened\_s123 to disk

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
names(track_changes_s123)
```

    ## [1] "screen.log"       "observers.effort" "nests"            "stages"          
    ## [5] "brood.sizes"      "predators"        "disturbance"      "notes"

``` r
str(track_changes_s123$nests)
```

    ## 'data.frame':    1276 obs. of  12 variables:
    ##  $ code               : num  1 1 1 1 1 1 1.1 1.1 1.1 1.1 ...
    ##  $ species            : chr  "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##  $ date               : Date, format: "2020-05-28" "2020-05-28" ...
    ##  $ multiple.survey.num: chr  "1" "1" "1" "1" ...
    ##  $ total.nests        : chr  "0" "0" "0" "0" ...
    ##  $ peak.active        : chr  "FALSE" "FALSE" "FALSE" "FALSE" ...
    ##  $ obs.initials       : chr  "GH" "GH" "GH" "GH" ...
    ##  $ record.in.wrangled : chr  "TRUE" "TRUE" "TRUE" "TRUE" ...
    ##  $ summary.sheet.made : chr  NA NA NA NA ...
    ##  $ record.in.screened : chr  NA NA NA NA ...
    ##  $ screened           : chr  NA NA NA NA ...
    ##  $ changelog          : chr  "no summary sheet" "no summary sheet" "no summary sheet" "no summary sheet" ...

You can filter based on changelog to see the records that differed
between wrangled\_s123 and screened\_s123. The main fields we expect to
change in screening are:

  - peak active date
  - ROP days
  - stage 4 date

The numeric nest count fields should not change, so be sure to scan
those to make sure they didn’t change accidentally

``` r
track_changes_s123$nests %>% 
  filter(grepl("changed", changelog)) %>% view()
```

Save to disk

``` r
saveRDS(track_changes_s123, here(paste("data/track_changes/track_changes_s123_", zyear, sep = ""))) 
```

## Step 6 convert to HEPDATA

HEPDATA is a “wide” data structure, and has field names in ALLCAPS. Thus
this comprises a bunch of reshaping and renaming.

``` r
source(here("code/step6_screened_to_HEPDATA.R"))

HEPDATA <- screened_to_HEPDATA(zyear)

str(HEPDATA)
```

    ## tibble [6 x 99] (S3: tbl_df/tbl/data.frame)
    ##  $ SITE         : chr [1:6] "Test colony" "Test colony" "Test colony" "Test colony" ...
    ##  $ CODE         : num [1:6] 599 599 599 599 599 599
    ##  $ SPECIES      : chr [1:6] "BCNH" "CAEG" "DCCO" "GBHE" ...
    ##  $ NUMBERVISITS : num [1:6] 20 20 20 20 20 20
    ##  $ TOTALHOURS   : num [1:6] 18.8 18.8 18.8 18.8 18.8 ...
    ##  $ YEAR         : num [1:6] 2020 2020 2020 2020 2020 2020
    ##  $ PEAKACTVNSTS : num [1:6] 36 33 11 11 54 55
    ##  $ BRD1         : num [1:6] NA NA NA 1 7 NA
    ##  $ BRD2         : num [1:6] NA NA NA 1 7 NA
    ##  $ BRD3         : num [1:6] NA NA NA 1 7 NA
    ##  $ BRD4         : num [1:6] NA NA NA 0 2 NA
    ##  $ BRD5         : num [1:6] NA NA NA 0 0 NA
    ##  $ MARSTGEDATE  : Date[1:6], format: NA NA ...
    ##  $ MARSTAGE1    : num [1:6] NA NA NA 0 5 NA
    ##  $ MARSTAGE2    : num [1:6] NA NA NA 5 32 NA
    ##  $ MARSTAGE3    : num [1:6] NA NA NA 0 0 NA
    ##  $ MARSTAGE4    : num [1:6] NA NA NA 0 0 NA
    ##  $ MARSTAGE5    : num [1:6] NA NA NA 0 0 NA
    ##  $ LTMARSTGDATE : Date[1:6], format: NA NA ...
    ##  $ LTMARSTAGE1  : num [1:6] NA NA NA 0 2 NA
    ##  $ LTMARSTAGE2  : num [1:6] NA NA NA 2 16 NA
    ##  $ LTMARSTAGE3  : num [1:6] NA NA NA 0 0 NA
    ##  $ LTMARSTAGE4  : num [1:6] NA NA NA 0 0 NA
    ##  $ LTMARSTAGE5  : num [1:6] NA NA NA 0 0 NA
    ##  $ APRSTGEDATE  : Date[1:6], format: NA NA ...
    ##  $ APRSTAGE1    : num [1:6] NA NA NA 0 0 NA
    ##  $ APRSTAGE2    : num [1:6] NA NA NA 1 9 NA
    ##  $ APRSTAGE3    : num [1:6] NA NA NA 3 19 NA
    ##  $ APRSTAGE4    : num [1:6] NA NA NA 3 19 NA
    ##  $ APRSTAGE5    : num [1:6] NA NA NA 0 0 NA
    ##  $ MAYSTGEDATE  : Date[1:6], format: NA NA ...
    ##  $ MAYSTAGE1    : num [1:6] NA NA NA 0 0 NA
    ##  $ MAYSTAGE2    : num [1:6] NA NA NA 0 4 NA
    ##  $ MAYSTAGE3    : num [1:6] NA NA NA 2 14 NA
    ##  $ MAYSTAGE4    : num [1:6] NA NA NA 4 29 NA
    ##  $ MAYSTAGE5    : num [1:6] NA NA NA 0 0 NA
    ##  $ JUNSTGEDATE  : Date[1:6], format: NA NA ...
    ##  $ JUNSTAGE1    : num [1:6] NA NA NA 0 0 NA
    ##  $ JUNSTAGE2    : num [1:6] NA NA NA 0 0 NA
    ##  $ JUNSTAGE3    : num [1:6] NA NA NA 1 10 NA
    ##  $ JUNSTAGE4    : num [1:6] NA NA NA 1 10 NA
    ##  $ JUNSTAGE5    : num [1:6] NA NA NA 5 32 NA
    ##  $ LTJUNSTGDATE : Date[1:6], format: NA NA ...
    ##  $ LTJUNSTAGE1  : num [1:6] NA NA NA 0 0 NA
    ##  $ LTJUNSTAGE2  : num [1:6] NA NA NA 0 0 NA
    ##  $ LTJUNSTAGE3  : num [1:6] NA NA NA 1 9 NA
    ##  $ LTJUNSTAGE4  : num [1:6] NA NA NA 1 9 NA
    ##  $ LTJUNSTAGE5  : num [1:6] NA NA NA 4 29 NA
    ##  $ DIST1DATE    : chr [1:6] "2020-03-19" "2020-03-19" "2020-03-19" "2020-03-19" ...
    ##  $ DIST1TYPE    : chr [1:6] "h" "h" "h" "h" ...
    ##  $ DIST1RESULT  : chr [1:6] "0" "0" "0" "0" ...
    ##  $ DIST2DATE    : chr [1:6] "2020-04-11" "2020-04-11" "2020-04-11" "2020-04-11" ...
    ##  $ DIST2TYPE    : chr [1:6] "a" "a" "a" "a" ...
    ##  $ DIST2RESULT  : chr [1:6] "2" "2" "2" "2" ...
    ##  $ DIST3DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST3TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST3RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ DIST4DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST4TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST4RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ DIST5DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST5TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST5RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ DIST6DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST6TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST6RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ DIST7DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST7TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST7RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ DIST8DATE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST8TYPE    : chr [1:6] NA NA NA NA ...
    ##  $ DIST8RESULT  : chr [1:6] NA NA NA NA ...
    ##  $ GHOWNESTING  : chr [1:6] "1" "1" "1" "1" ...
    ##  $ Entry_Proofed: chr [1:6] "" "" "" "" ...
    ##  $ Entered_By   : chr [1:6] "" "" "" "" ...
    ##  $ DATAID       : logi [1:6] NA NA NA NA NA NA
    ##  $ PEAKRICHNESS : logi [1:6] NA NA NA NA NA NA
    ##  $ TOTALSPECIES : logi [1:6] NA NA NA NA NA NA
    ##  $ INDIVIDUALS  : logi [1:6] NA NA NA NA NA NA
    ##  $ FOCALNESTS   : logi [1:6] NA NA NA NA NA NA
    ##  $ FOCFAILURE   : logi [1:6] NA NA NA NA NA NA
    ##  $ DISTURBANCE  : logi [1:6] NA NA NA NA NA NA
    ##  $ SOURCE       : logi [1:6] NA NA NA NA NA NA
    ##  $ NOTES        : logi [1:6] NA NA NA NA NA NA
    ##  $ BRD6         : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTGEDATE  : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTAGE1    : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTAGE2    : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTAGE3    : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTAGE4    : logi [1:6] NA NA NA NA NA NA
    ##  $ JULSTAGE5    : logi [1:6] NA NA NA NA NA NA
    ##  $ DIST9DATE    : logi [1:6] NA NA NA NA NA NA
    ##  $ DIST9TYPE    : logi [1:6] NA NA NA NA NA NA
    ##  $ DIST9RESULT  : logi [1:6] NA NA NA NA NA NA
    ##  $ RTHANESTING  : logi [1:6] NA NA NA NA NA NA
    ##  $ OSPRNESTING  : logi [1:6] NA NA NA NA NA NA
    ##  $ CORANESTING  : logi [1:6] NA NA NA NA NA NA
    ##  $ BAEANESTING  : logi [1:6] NA NA NA NA NA NA
    ##  $ TUVUROOSTING : logi [1:6] NA NA NA NA NA NA

Finally, save HEPDATA for appending to the HEPDATA access database.

``` r
saveRDS(HEPDATA, here(paste("data/as_HEPDATA/HEPDATA_", zyear, sep = "")))
```
