



#' Combine wrangled HEP data objects.
#'
#' @return Object with the same structure as each individual wrangled_ object
#' @export
#'
#' @examples
#' combine_wrangled_hep %>% saveRDS(here("data/wrangled/wrangled_raw_2021"))
combine_wrangled_hep <- function() {
wrangled_alcatraz <- readRDS(here("data/wrangled/wrangled_alcatraz_2021"))


wrangled_s123 <- readRDS(here("data/wrangled/wrangled_s123_2021"))


wrangled_raw <- list(dates = rbind(wrangled_s123$dates %>% 
                                     mutate(species = NA),
                                   wrangled_alcatraz$dates),
                     observers.effort = rbind(wrangled_s123$observers.effort %>% 
                                     mutate(species = NA),
                                              wrangled_alcatraz$observers.effort),
                     nests = rbind(wrangled_s123$nests,
                                   wrangled_alcatraz$nests),
                     stages = rbind(wrangled_s123$stages,
                                   wrangled_alcatraz$stages),
                     brood.sizes = rbind(wrangled_s123$brood.sizes,
                                   wrangled_alcatraz$brood.sizes),
                     predators = rbind(wrangled_s123$predators,
                                   wrangled_alcatraz$predators),
                     disturbance = rbind(wrangled_s123$disturbance,
                                   wrangled_alcatraz$disturbance),
                     notes = rbind(wrangled_s123$notes,
                                   wrangled_alcatraz$notes))

return(wrangled_raw)

}

