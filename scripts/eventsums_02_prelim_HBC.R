library(curl)
library(tidyverse)
library(writexl)
library(googlesheets4)

# Get an eBird API token and assign it to object myebirdtoken
source("token.R")
source("scripts/functions.R")

# list of region codes
load("data/region_codes.RData")

# eBird API not working for districts
api_districts <- FALSE

###

# cur_date <- today() %>% floor_date(unit = "month")
# 
# cur_year <- cur_date %>% year()
# cur_month_num <- cur_date %>% month()
# cur_month_lab <- cur_date %>% month(label = T, abbr = T)
# # cur_month_num <- (cur_date - months(1)) %>% month()
# # cur_month_lab <- (cur_date - months(1)) %>% month(label = T, abbr = T)
# 
# 
# ## authorising google sheets
# gs4_auth(email = "birdcountindia@ncf-india.org")
# 
# ## getting events schedule 
# 
# sched0 <- read_sheet("https://docs.google.com/spreadsheets/d/1Yf_STtmbtPZHybwT544cKkY8jQ5DuoZrM-jiZiG5oiU/edit?usp=sharing") %>% 
#   # creating full event code string
#   mutate(FULL.CODE = ifelse(is.na(TYPE.CODE), 
#                             glue("{SHORT.CODE}_{EDITION}"),
#                             glue("{SHORT.CODE}_{TYPE.CODE}_{EDITION}"))) %>% 
#   mutate(across(contains(".DATE"), ~ as.Date(.)),
#          START.MONTH = month(START.DATE),
#          END.MONTH = month(END.DATE)) %>% 
#   # filtering for HBC
#   filter(SHORT.CODE == "HBC")



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


write_path_species <- "outputs/HBC2023/Final_SpeciesList.csv"
write_path_notablespecies <- "outputs/HBC2023/Final_SpeciesList_Notable.csv"
write_path_participation <- "outputs/HBC2023/Final_Participation.xlsx"


ebd <- read_csv("eBirdTaxonomy.csv") %>% 
  magrittr::set_colnames(c("ENGLISH.NAME", "SORT.V2021")) 


###

year <- 2023
month <- 5
day <- 13

###


temp_spec1 <- map(regions, write_spec_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "ENGLISH.NAME"))

combined_species1 <- temp_spec1 %>% 
  distinct(ENGLISH.NAME) %>% 
  left_join(ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  dplyr::select(-SORT.V2021) %>% 
  filter(!is.na(ENGLISH.NAME))

write_csv(x = combined_species1, file = write_path_species)



participation_st1 <- map(regions, write_obs_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "CHECKLISTS", "SPECIES", "OBSERVERS")) %>% 
  mutate(across(c(everything(), -EBIRD.REGION), ~ as.numeric(.))) %>% 
  arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
  left_join(regions_meta) %>%
  relocate(EBIRD.REGION, REGION.NAME, CHECKLISTS, SPECIES, OBSERVERS)


in_spec1 <- temp_spec1 %>%
  left_join(regions_nat) %>%
  filter(REGION.NAME == "India") %>%
  group_by(REGION.NAME) %>%
  dplyr::summarise(SPECIES = n_distinct(ENGLISH.NAME))

states_part1 <- participation_st1 %>% 
  left_join(region_codes, by = c("EBIRD.REGION" = "STATE.CODE")) %>% 
  group_by(COUNTRY) %>% 
  mutate(TOT.REGIONS = n_distinct(EBIRD.REGION)) %>% 
  filter(OBSERVERS > 0) %>% 
  dplyr::summarise(REGIONS = n_distinct(EBIRD.REGION),
                   TOT.REGIONS = min(TOT.REGIONS)) %>% 
  mutate(REGION.COV = 100*REGIONS/TOT.REGIONS) %>% 
  rename(REGION.NAME = COUNTRY)
  

participation_nat1 <- map(regions_nat[,1], write_obs_tally) %>%
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
  left_join(in_spec1, by = "REGION.NAME") %>%
  mutate(SPECIES = coalesce(SPECIES.x, SPECIES.y),
         SPECIES.x = NULL,
         SPECIES.y = NULL) %>%
  arrange(desc(CHECKLISTS), desc(SPECIES)) %>% 
  left_join(states_part1)

participation_tot1 <- participation_nat1 %>% 
  dplyr::summarise(CHECKLISTS = sum(CHECKLISTS),
                   REGIONS = sum(REGIONS),
                   SPECIES = n_distinct(combined_species1$ENGLISH.NAME))

###

year <- 2022
month <- 5
day <- 14

###


participation_st2 <- map(regions, write_obs_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "CHECKLISTS", "SPECIES", "OBSERVERS")) %>% 
  mutate(across(c(everything(), -EBIRD.REGION), ~ as.numeric(.))) %>% 
  arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
  left_join(regions_meta) %>%
  relocate(EBIRD.REGION, REGION.NAME, CHECKLISTS, SPECIES, OBSERVERS)



temp_spec2 <- map(regions, write_spec_tally) %>% 
  list_c() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "ENGLISH.NAME"))

combined_species2 <- temp_spec2 %>% 
  distinct(ENGLISH.NAME) %>% 
  left_join(ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  dplyr::select(-SORT.V2021) %>% 
  filter(!is.na(ENGLISH.NAME))


in_spec2 <- temp_spec2 %>%
  left_join(regions_nat) %>%
  filter(REGION.NAME == "India") %>%
  group_by(REGION.NAME) %>%
  dplyr::summarise(SPECIES = n_distinct(ENGLISH.NAME))

states_part2 <- participation_st2 %>% 
  left_join(region_codes, by = c("EBIRD.REGION" = "STATE.CODE")) %>% 
  group_by(COUNTRY) %>% 
  mutate(TOT.REGIONS = n_distinct(EBIRD.REGION)) %>% 
  filter(OBSERVERS > 0) %>% 
  dplyr::summarise(REGIONS = n_distinct(EBIRD.REGION),
                   TOT.REGIONS = min(TOT.REGIONS)) %>% 
  mutate(REGION.COV = 100*REGIONS/TOT.REGIONS) %>% 
  rename(REGION.NAME = COUNTRY)


participation_nat2 <- map(regions_nat[,1], write_obs_tally) %>%
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
  left_join(in_spec2, by = "REGION.NAME") %>%
  mutate(SPECIES = coalesce(SPECIES.x, SPECIES.y),
         SPECIES.x = NULL,
         SPECIES.y = NULL) %>%
  arrange(desc(CHECKLISTS), desc(SPECIES)) %>% 
  left_join(states_part2)


participation_tot2 <- participation_nat2 %>% 
  dplyr::summarise(CHECKLISTS = sum(CHECKLISTS),
                   REGIONS = sum(REGIONS),
                   SPECIES = n_distinct(combined_species2$ENGLISH.NAME))


participation_nat_yoy <- participation_nat1 %>% 
  mutate(YEAR = year + 1) %>% 
  full_join(participation_nat2 %>% mutate(YEAR = year)) %>% 
  dplyr::select(-TOT.REGIONS, -REGION.COV) %>% 
  pivot_longer(cols = c(CHECKLISTS, SPECIES, REGIONS), names_to = "METRIC", values_to = "VALUE") %>% 
  pivot_wider(names_from = "YEAR", values_from = "VALUE") %>% 
  mutate(YoY = 100*(`2023`-`2022`)/`2022`,
         `2023` = NULL,
         `2022` = NULL) %>% 
  pivot_wider(names_from = "METRIC", values_from = "YoY")

participation_tot_yoy <- participation_tot1 %>% 
  mutate(YEAR = year + 1) %>% 
  full_join(participation_tot2 %>% mutate(YEAR = year)) %>% 
  pivot_longer(cols = c(CHECKLISTS, SPECIES, REGIONS), names_to = "METRIC", values_to = "VALUE") %>% 
  pivot_wider(names_from = "YEAR", values_from = "VALUE") %>% 
  mutate(YoY = 100*(`2023`-`2022`)/`2022`,
         `2023` = NULL,
         `2022` = NULL) %>% 
  pivot_wider(names_from = "METRIC", values_from = "YoY")




write_xlsx(path = write_path_participation,
           list("States" = participation_st1, 
                "Countries" = participation_nat1,
                "Countries YoY" = participation_nat_yoy,
                "Overall" = participation_tot1,
                "Overall YoY" = participation_tot_yoy))

