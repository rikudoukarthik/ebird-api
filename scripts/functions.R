
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
      filter(str_detect(code, parent_code)) %>% 
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
  print(nrow(regionStat))
  print(regionStat)
  
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
  speciesList <- cbind(region, parsed$comName)
  
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

write_notable_spec <- function(region)
{  
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
