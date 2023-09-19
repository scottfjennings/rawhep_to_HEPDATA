



#' Combine wrangled HEP data objects.
#'
#' @param wrangled_s123 wrangled 
#' @param wrangled_alcatraz 
#' @param wrangled_site_visit 
#'
#' @return Object with the same structure as each individual wrangled_ object
#' @export
#'
#' @examples
#' combine_wrangled_hep()
combine_wrangled_hep <- function(wrangled_s123, wrangled_alcatraz = NA, wrangled_site_visits = NA) {


dates = wrangled_s123$dates

observers.effort = wrangled_s123$observers.effort

nests = wrangled_s123$nests

stages = wrangled_s123$stages

brood.sizes = wrangled_s123$brood.sizes

predators = wrangled_s123$predators

disturbance = wrangled_s123$disturbance

notes = wrangled_s123$notes
                     
                     
if(any(!is.na(wrangled_alcatraz))) {
dates = bind_rows(dates,
                  wrangled_alcatraz$dates)

observers.effort = bind_rows(observers.effort,
                             wrangled_alcatraz$observers.effort)

nests = bind_rows(nests %>% filter(!(code == 70 & (species %in% c("BCNH", "SNEG")))),
                  wrangled_alcatraz$nests)

stages = bind_rows(stages,
                   wrangled_alcatraz$stages)
              
brood.sizes = bind_rows(brood.sizes,
                        wrangled_alcatraz$brood.sizes)

predators = bind_rows(predators %>% filter(!(code == 70 & (species %in% c("BCNH", "SNEG")))),
                      wrangled_alcatraz$predators)

disturbance = bind_rows(disturbance %>% filter(!(code == 70 & (species %in% c("BCNH", "SNEG")))),
                        wrangled_alcatraz$disturbance)

notes = bind_rows(notes %>% filter(!(code == 70 & (species %in% c("BCNH", "SNEG")))),
                  wrangled_alcatraz$notes)
}

if(any(!is.na(wrangled_site_visits))) {
dates = bind_rows(dates,
                  wrangled_site_visits$dates)

observers.effort = bind_rows(observers.effort,
                             wrangled_site_visits$observers.effort)

nests = bind_rows(nests,
                  wrangled_site_visits$nests)

stages = bind_rows(stages,
                   wrangled_site_visits$stages)
              
brood.sizes = bind_rows(brood.sizes,
                        wrangled_site_visits$brood.sizes)

predators = bind_rows(predators,
                      wrangled_site_visits$predators)

disturbance = bind_rows(disturbance,
                        wrangled_site_visits$disturbance)

notes = bind_rows(notes,
                  wrangled_site_visits$notes)
}
              

wrangled_raw <- list(dates = dates,
                     observers.effort = observers.effort,
                     nests = nests,
                     stages = stages,
                     brood.sizes = brood.sizes,
                     predators = predators,
                     disturbance = disturbance,
                     notes = notes)

return(wrangled_raw)

}

