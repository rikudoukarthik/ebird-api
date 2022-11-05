library(curl)
library(tidyverse)

# Get an eBird API token and assign it to object myebirdtoken
source("token.R")
source("functions.R")

day <- 04
month <- 11
year <- 2022

regions <- c("IN-NL")

# regions <- c("IN-JK", "IN-LA", "IN-HP", "IN-UL", "NP", "IN-SK", "IN-WB-DA", "IN-WB-KA",
#              "IN-WB-JA", "IN-WB-AL", "BT", "IN-AR")



combined_species <- sapply(regions, write_spec_tally) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames("ENGLISH.NAME") %>% 
  distinct(ENGLISH.NAME)


ebd <- read_csv("eBirdTaxonomy.csv") %>% 
  magrittr::set_colnames(c("ENGLISH.NAME", "SORT.V2021")) 

combined_species <- left_join(combined_species, ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  select(-SORT.V2021)

write_csv(combined_species, "TEBC2022_Day1_SpeciesList.csv")



participation_summary <- sapply(regions, write_obs_tally) %>% 
  t() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("EBIRD.REGION", "CHECKLISTS", "SPECIES", "OBSERVERS"))

write_csv(participation_summary, "TEBC2022_Day1_Participation.csv")


