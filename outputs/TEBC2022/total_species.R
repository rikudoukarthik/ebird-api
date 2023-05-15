library(tidyverse)


total_species <- list.files(path = "outputs/TEBC2022/", pattern = "*SpeciesList.csv", full.names = T) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  distinct(ENGLISH.NAME) %>% 
  # to order species taxonomically
  left_join(ebd) %>% 
  mutate(SORT.V2021 = as.numeric(SORT.V2021)) %>% 
  arrange(SORT.V2021) %>% 
  select(-SORT.V2021)

write_csv(total_species, "outputs/TEBC2022/TEBC2022_Overall_SpeciesList.csv")
