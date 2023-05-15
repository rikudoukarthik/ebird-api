library(curl)
library(tidyverse)
library(writexl)

# Get an eBird API token and assign it to object myebirdtoken
source("token.R")
source("scripts/functions.R")

# list of region codes
load("data/region_codes.RData")

# eBird API not working for districts
api_districts <- FALSE

###


day <- 13
month <- 05
year <- 2023


# main regions
cur_regions <- c("IN-JK", "IN-LA", "IN-HP", "IN-UL", "IN-AR", "IN-SK",
                 # "IN-WB-DA", "IN-WB-KA", "IN-WB-AL", "IN-WB-JA",
                 # "IN-AS-KK", "IN-AS-CR", "IN-AS-BK", "IN-AS-SO", "IN-AS-DM", "IN-AS-LA", "IN-AS-UD", "IN-AS-BI",
                 # "IN-PB-HO", "IN-PB-RU", "IN-PB-PK", "IN-PB-SH",
                 # "IN-HR-PK", "IN-HR-YN",
                 # "IN-UP-PI", "IN-UP-LK", "IN-UP-SA", "IN-UP-BH", "IN-UP-SV", "IN-UP-BP",
                 # "IN-HP-UN",
                 # "IN-BR-WC", "IN-BR-EC",
                 "BT-42", "BT-31", "BT-12", "BT-23", "BT-32", "BT-15", "BT-24", "BT-33", "BT-22", "BT-GA",
                 "BT-13", "BT-44", "BT-11", "BT-43", "BT-45", "BT-14", "BT-TY", "BT-41", "BT-21", "BT-34",
                 "NP-1", "NP-2", "NP-3", "NP-4", "NP-5", "NP-6", "NP-7")

temp1 <- region_codes %>% 
  filter(str_detect(COUNTRY.CODE, str_flatten(cur_regions, collapse = "|"))) %>% 
  distinct(COUNTRY.CODE, COUNTRY)
temp2 <- region_codes %>% 
  filter(str_detect(STATE.CODE, str_flatten(cur_regions, collapse = "|"))) %>% 
  distinct(STATE.CODE, STATE)
temp3 <- region_codes %>% 
  filter(str_detect(COUNTY.CODE, str_flatten(cur_regions, collapse = "|"))) %>% 
  distinct(COUNTY.CODE, COUNTY)

if (!api_districts) {
  temp3 <- NULL
}

regions <- c(temp1$COUNTRY.CODE, temp2$STATE.CODE, temp3$COUNTY.CODE)


if (api_districts) {
  regions_meta <- data.frame(EBIRD.REGION = regions) %>%
    left_join(temp1, by = c("EBIRD.REGION" = "COUNTRY.CODE")) %>% 
    left_join(temp2, by = c("EBIRD.REGION" = "STATE.CODE")) %>% 
    left_join(temp3, by = c("EBIRD.REGION" = "COUNTY.CODE")) %>% 
    mutate(REGION.NAME = coalesce(COUNTRY, STATE, COUNTY)) %>% 
    dplyr::select(EBIRD.REGION, REGION.NAME)
} else {
  regions_meta <- data.frame(EBIRD.REGION = regions) %>%
    left_join(temp1, by = c("EBIRD.REGION" = "COUNTRY.CODE")) %>% 
    left_join(temp2, by = c("EBIRD.REGION" = "STATE.CODE")) %>% 
    mutate(REGION.NAME = coalesce(COUNTRY, STATE)) %>% 
    dplyr::select(EBIRD.REGION, REGION.NAME)
}
  
regions_nat <- data.frame(EBIRD.REGION = c("IN-JK", "IN-LA", "IN-HP", "IN-UL", "IN-AR", "IN-SK", "BT", "NP"),
                          REGION.NAME = c(rep("India", 6), "Bhutan", "Nepal"))


write_path_species <- "outputs/HBC2023/SpeciesList.csv"
write_path_notablespecies <- "outputs/HBC2023/SpeciesList_Notable.csv"
write_path_participation <- "outputs/HBC2023/Participation.xlsx"


ebd <- read_csv("eBirdTaxonomy.csv") %>% 
  magrittr::set_colnames(c("ENGLISH.NAME", "SORT.V2021")) 


###

temp_spec <- map(regions, write_spec_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "ENGLISH.NAME"))

combined_species <- temp_spec %>% 
  distinct(ENGLISH.NAME) %>% 
  left_join(ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  dplyr::select(-SORT.V2021) %>% 
  filter(!is.na(ENGLISH.NAME))

write_csv(x = combined_species, file = write_path_species)



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


in_spec <- temp_spec %>%
  left_join(regions_nat) %>%
  filter(REGION.NAME == "India") %>%
  group_by(REGION.NAME) %>%
  dplyr::summarise(SPECIES = n_distinct(ENGLISH.NAME))

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
           list(States = participation_st, 
                Countries = participation_nat))


notable_spec <- map(regions, write_notable_spec) %>%
  list_c() %>%
  as.data.frame() %>%
  magrittr::set_colnames(c("EBIRD.REGION", "COMMON.NAME", "SAMPLING.EVENT.IDENTIFIER")) %>%
  group_by(COMMON.NAME) %>%
  dplyr::summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                   NO.REGIONS = n_distinct(EBIRD.REGION),
                   EBIRD.REGION = min(EBIRD.REGION)) %>%
  arrange(NO.LISTS, NO.REGIONS, COMMON.NAME) %>% 
  filter(!is.na(COMMON.NAME))

write_csv(x = notable_spec, file = write_path_notablespecies)
