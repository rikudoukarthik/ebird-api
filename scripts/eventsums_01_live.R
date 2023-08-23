# API 2.0 documentation https://documenter.getpostman.com/view/664302/S1ENwy59

library(curl)
library(tidyverse)
library(writexl)

# Get an eBird API token and assign it to object myebirdtoken
source("token.R")
source("scripts/functions.R")

# list of region codes
load("data/region_codes.RData")

###


day <- 23
month <- 08
year <- 2023


# main regions
cur_region <- c("IN-KL") # can add other states here if needed

temp1 <- region_codes %>% filter(str_detect(COUNTRY.CODE, cur_region)) %>% distinct(COUNTRY.CODE)
temp2 <- region_codes %>% filter(str_detect(STATE.CODE, cur_region)) %>% distinct(STATE.CODE)
temp3 <- region_codes %>% filter(str_detect(COUNTY.CODE, cur_region)) %>% distinct(COUNTY.CODE)

regions <- c(temp1$COUNTRY.CODE, temp2$STATE.CODE, temp3$COUNTY.CODE)


write_path_species <- "outputs/HBC2023/IN-UL_SpeciesList.csv"
write_path_notablespecies <- "outputs/HBC2023/IN-UL_SpeciesList_Notable.csv"
write_path_participation <- "outputs/HBC2023/IN-UL_Participation.xlsx"


ebd <- read_csv("eBirdTaxonomy.csv") %>% 
  magrittr::set_colnames(c("ENGLISH.NAME", "SORT.V2021")) 


###

# list of species recorded

temp_spec <- map(regions, write_spec_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "ENGLISH.NAME"))

combined_species <- temp_spec %>% 
  distinct(ENGLISH.NAME) %>% 
  left_join(ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  dplyr::select(-SORT.V2021)

write_csv(x = combined_species, file = write_path_species)


# participation stats for the state (district-level stats not working in API currently)

participation_st <- map(regions, write_obs_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "CHECKLISTS", "SPECIES", "OBSERVERS")) %>% 
  mutate(across(c(everything(), -EBIRD.REGION), ~ as.numeric(.))) %>% 
  arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
  # left_join(regions_meta) %>% 
  relocate(EBIRD.REGION, 
           # REGION.NAME, 
           CHECKLISTS, SPECIES, OBSERVERS)


# participation stats for country (states within the country)

regions_nat <- region_codes %>% 
  filter(COUNTRY == "India") %>% 
  distinct(STATE.CODE) %>% 
  mutate(REGION.NAME = "India") %>% 
  rename(EBIRD.REGION = STATE.CODE)

in_spec <- temp_spec %>%
  left_join(regions_nat) %>%
  filter(REGION.NAME == "India") %>%
  group_by(REGION.NAME) %>%
  dplyr::summarise(SPECIES = n_distinct(ENGLISH.NAME))

# cannot calculate no. of observers, because no way to remove duplicates across states
participation_nat <- map(regions_nat[,1], write_obs_tally) %>%
  list_c() %>%
  as.data.frame() %>%
  magrittr::set_colnames(c("EBIRD.REGION", "CHECKLISTS", "SPECIES", "OBSERVERS")) %>%
  mutate(across(c(everything(), -EBIRD.REGION), ~ as.numeric(.))) %>%
  left_join(regions_nat) %>%
  mutate(EBIRD.REGION = NULL) %>%
  group_by(REGION.NAME) %>%
  dplyr::summarise(CHECKLISTS = sum(CHECKLISTS),
                   SPECIES = sum(SPECIES)) %>%
  # will add correct no.sp for India by joining
  mutate(SPECIES = ifelse(REGION.NAME == "India", NA_real_, SPECIES)) %>%
  left_join(in_spec, by = "REGION.NAME") %>%
  mutate(SPECIES = coalesce(SPECIES.x, SPECIES.y),
         SPECIES.x = NULL,
         SPECIES.y = NULL) %>%
  arrange(desc(CHECKLISTS), desc(SPECIES))

write_xlsx(path = write_path_participation,
           list(States = participation_st
                # Countries = participation_nat
                ))


# list of notable species recorded (highlights)

notable_spec <- map(regions, write_notable_spec) %>%
  list_c() %>%
  as.data.frame() %>%
  magrittr::set_colnames(c("EBIRD.REGION", "COMMON.NAME", "SAMPLING.EVENT.IDENTIFIER")) %>%
  group_by(COMMON.NAME) %>%
  dplyr::summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                   NO.REGIONS = n_distinct(EBIRD.REGION),
                   EBIRD.REGION = min(EBIRD.REGION)) %>%
  arrange(NO.LISTS, NO.REGIONS, COMMON.NAME)

write_csv(x = notable_spec, file = write_path_notablespecies)
