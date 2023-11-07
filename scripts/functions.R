
# basic functions -------------------------------------------------------------------

get_dates <- function(dates_cur = lubridate::today()) {
  
  # dates_cur can also be a vector of dates (for multi-day events)
  
  if (!is.Date(dates_cur)) {
    dates_cur <- dates_cur %>% lubridate::as_date()
  }
  
  dates_prev <- dates_cur - lubridate::years(1)
  
  # return objects to environment
  list("dates_cur" = dates_cur,
       "dates_prev" = dates_prev) %>%
    list2env(envir = .GlobalEnv)
  
}


# eBird API functions ---------------------------------------------------------------

# admin units based on eBird 
get_admin_codes <- function(unit_code, hi_arch = TRUE) {
  
  if (!exists("all_units")) {
    
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
    
  }
  

  if (!unit_code %in% all_units$code) {
    return("Input admin unit code is not a valid code!")
  }
  
  # if country, we want only subnational1
  req_adm2 <- if (unit_code %in% list_countries$code) FALSE else TRUE
  
  if (!hi_arch) {
    
    return(unit_code)
    
  } else {
    
    req_units <- all_units %>% 
      filter(str_detect(code, unit_code)) %>% 
      {if (req_adm2) {
        .
      } else {
        anti_join(., list_districts, by = c("code", "name"))
      }} %>% 
      pull(code)

    return(req_units)
    
  }
  
}

get_admin_names <- function(region_input) {
  
  if (!exists("all_units")) {
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
  }
  
  region_names <- all_units %>% 
    filter(code %in% get_admin_codes(region_input, hi_arch = TRUE)) %>% 
    magrittr::set_colnames(c("REGION", "REGION.NAME"))
  
  return(region_names)
  
}



get_obs_count <- function(region, date = date_cur)
{
  date <- str_replace_all(date, "-", "/")
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/product/stats/{region}/{date}")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_obs_tally <- function(region, date = date_cur)
{  
  req <- get_obs_count(region, date)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  else
  {
    print ("Observer count returned error")
  }
  
  regionStat <- cbind(region, parsed$numContributors, parsed$numChecklists, parsed$numSpecies)
  # print(nrow(regionStat))
  # print(regionStat)
  
  return(regionStat)
}


get_spec_list <- function(region, date = date_cur)
{
  date <- str_replace_all(date, "-", "/")
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/data/obs/{region}/historic/{date}")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_spec_tally <- function(region, date = date_cur)
{  
  req <- get_spec_list(region, date)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  speciesList <- cbind(region, comName = parsed$comName)
  
  if(is.null(parsed$comName)) {
    speciesList <- cbind(speciesList, comName = NA_real_)
  }
  
  return(speciesList)
}


get_notable_spec <- function(region, back, maxResults = 5) {
  
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/data/obs/{region}/recent/notable")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
  
}

write_notable_spec <- function(region) {  
  req <- get_notable_spec(region)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  speciesList <- cbind(region, parsed$comName, parsed$subId)

  if(is.null(parsed$comName)) {
    speciesList <- cbind(speciesList, comName = NA_real_, subId = NA_real_)
  }
  
  return(speciesList)
  
}


# adapted from PJ's code in ebird-datasets/BCI-metrics
get_media_summary <- function(date_start, date_end) {
  
  h <- new_handle()
  
  region_info <- get_admin_names(cur_region) %>% filter(REGION == cur_region)
  
  address_url <- "https://search.macaulaylibrary.org/api/v1/stats/media-count?yr=YCUSTOM&mr=MCUSTOM&"
  address_part1 <- glue("bmo={month(date_start)}&by={year(date_start)}&emo={month(date_end)}&ey={year(date_end)}")
  address_part2 <- glue("&region={region_info$REGION.NAME}%20({region_info$REGION})&regionCode={region_info$REGION}")
  address_part3 <- glue("&includeUnconfirmed=T")
  address_API <- glue("{address_url}{address_part1}{address_part2}{address_part3}")
  
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  retries <- 0
  while (retries < 5) {
    req <- curl_fetch_memory(address_API, h)
    Sys.sleep(3)

    if (req$status_code == 200) {
      mediaSummary <- jsonlite::prettify(rawToChar(req$content)) %>% 
        fromJSON(flatten = FALSE) %>%
        as.data.frame()
      return (mediaSummary)
    } else {
      print("HTTP GET returned error. Retrying...")
      retries <- retries + 1
    }
    
  }
  
}

# higher-level API functions --------------------------------------------------------

# uses above functions 


# generate species list for given regions and dates

# # for subnat2
# get_admin_codes(input$region_code, hi_arch = TRUE) %>%
#   gen_spec_list(dates = dates_cur) %>%
#   filter(REGION != input$region_code) # if hi_arch == TRUE

gen_spec_list <- function(regions, dates) {
  
  parent_code <- str_sort(regions)[1]
  
  if (length(dates) == 1) {
    
    list_spec <- regions %>% 
      map(~ write_spec_tally(.x, dates)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME")) %>% 
      left_join(ebd, by = "ENGLISH.NAME") %>% 
      arrange(REGION, SORT) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>% 
      dplyr::select(REGION, REGION.NAME, ENGLISH.NAME)
    
  } else if (length(dates) > 1) {
    
    regions_dates <- expand_grid(regions, dates) %>% 
      magrittr::set_colnames(c("REGION", "DATE")) %>% 
      arrange(DATE) %>% 
      group_by(DATE) %>% 
      mutate(DAY.NO = cur_group_id()) %>% 
      ungroup()
    
    list_spec <- map2(regions_dates$REGION, regions_dates$DATE, 
                      ~ write_spec_tally(.x, .y) %>% bind_cols(tibble(DATE = .y))) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME", "DATE")) %>% 
      left_join(regions_dates %>% distinct(DATE, DAY.NO), 
                by = "DATE") %>% 
      left_join(ebd, by = "ENGLISH.NAME") %>% 
      arrange(DAY.NO, REGION, SORT) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>%
      dplyr::select(DAY.NO, REGION, REGION.NAME, ENGLISH.NAME) %>% # remove DATE for pivot
      mutate(PRESENT = 1) %>% 
      pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "PRESENT") %>% 
      mutate(across(starts_with("DAY"), ~ replace_na(., replace = 0))) %>% 
      arrange(REGION, across(starts_with("DAY"), desc))
    
    # total species reported over all days
    tot_spec_alldays <- list_spec %>% 
      group_by(REGION) %>% 
      reframe(TOT.SPEC = n_distinct(ENGLISH.NAME))
    
    list("tot_spec_alldays" = tot_spec_alldays) %>% list2env(envir = .GlobalEnv)
    
  }
  
  # list of SoIB High Priority
  
  
  # list of endemics
  
  
  return(list_spec)
  
}


# generate participation summary for given regions and dates

# # for subnat2
# get_admin_codes(input$region_code, hi_arch = TRUE) %>%
#   gen_part_summ(dates = dates_cur) %>%
#   filter(REGION != input$region_code) # if hi_arch == TRUE

gen_part_summ <- function(regions, dates) {
  
  parent_code <- str_sort(regions)[1]
  
  if (length(dates) == 1) {
    
    summary_part <- regions %>% 
      map(~ write_obs_tally(.x, dates)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
      mutate(across(c("OBSERVERS", "CHECKLISTS", "SPECIES"), ~ as.integer(.))) %>% 
      arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>% 
      relocate(REGION, REGION.NAME)
    
  } else if (length(dates) > 1) {

    regions_dates <- expand_grid(regions, dates) %>% 
      magrittr::set_colnames(c("REGION", "DATE")) %>% 
      arrange(DATE) %>% 
      group_by(DATE) %>% 
      mutate(DAY.NO = cur_group_id()) %>% 
      ungroup()
    
    summary_part <- map2(regions_dates$REGION, regions_dates$DATE, 
                         ~ write_obs_tally(.x, .y) %>% bind_cols(tibble(DATE = .y))) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES", "DATE")) %>% 
      mutate(across(c("OBSERVERS", "CHECKLISTS", "SPECIES"), ~ as.integer(.))) %>% 
      left_join(regions_dates %>% distinct(DATE, DAY.NO), 
                by = "DATE") %>% 
      arrange(DAY.NO, desc(OBSERVERS), desc(SPECIES)) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>%
      relocate(DATE, DAY.NO, REGION, REGION.NAME) %>% 
      pivot_longer(c(OBSERVERS, CHECKLISTS, SPECIES),
                   names_to = "TOTAL", values_to = "VALUE") %>% 
      dplyr::select(-DATE) %>% 
      pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "VALUE") %>% 
      arrange(REGION)
    
    # adding all-day totals
    if (exists("tot_spec_alldays")) {
      
      message(paste("Taking total species over all days from existing object in environment.",
                    "(Is the correct object loaded for current regions and dates?)"))
      
      summary_part <- summary_part %>% 
        left_join(tot_spec_alldays %>% mutate(TOTAL = "SPECIES"), 
                  by = c("TOTAL", "REGION")) %>% 
        mutate(ALL.DAYS = case_when(
          TOTAL == "OBSERVERS" ~ "NA",
          TOTAL == "CHECKLISTS" ~ rowSums(across(starts_with("DAY"))) %>% as.character(),
          TOTAL == "SPECIES" ~ TOT.SPEC %>% as.character()
        ),
        TOT.SPEC = NULL)
      
    } else {
      
      warning(paste("Total species over all days can only be calculated after generating species lists.",
                    "Returning NA."))
      
      summary_part <- summary_part %>% 
        mutate(ALL.DAYS = case_when(
          TOTAL == "OBSERVERS" ~ "NA",
          TOTAL == "CHECKLISTS" ~ rowSums(across(starts_with("DAY"))) %>% as.character(),
          TOTAL == "SPECIES" ~ "NA"
        ))
      
    }
    
  }
  
  return(summary_part)
  
}
