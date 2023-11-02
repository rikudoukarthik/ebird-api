
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

# admin units based on eBird 
get_admin_codes <- function(unit_code, hi_arch = TRUE) {
  
  # list of region codes
  load("data/region_codes.RData")
  
  
  all_units <- unique(c(region_codes$COUNTRY.CODE, region_codes$STATE.CODE, region_codes$COUNTY.CODE))
  if (!unit_code %in% all_units) {
    return("Input admin unit code is not a valid code!")
  }
  
  
  if (!hi_arch) {
    
    return(unit_code)
    
  } else {
    
    countries <- region_codes %>% filter(str_detect(COUNTRY.CODE, unit_code)) %>% distinct(COUNTRY.CODE)
    states <- region_codes %>% filter(str_detect(STATE.CODE, unit_code)) %>% distinct(STATE.CODE)
    districts <- region_codes %>% filter(str_detect(COUNTY.CODE, unit_code)) %>% distinct(COUNTY.CODE)
    
    return(c(countries$COUNTRY.CODE, states$STATE.CODE, districts$COUNTY.CODE))
    
  }
  
}

# eBird API functions ---------------------------------------------------------------

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

write_obs_tally <- function(region)
{  
  print(paste(region, date_cur))
  req <- get_obs_count(region, date = date_cur)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  else
  {
    print ("Observer count returned error")
  }
  
  regionStat <- cbind(region, parsed$numChecklists, parsed$numSpecies, parsed$numContributors)
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

write_spec_tally <- function(region)
{  
  print(region)
  req <- get_spec_list(region, date = date_cur)
  
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
  print(region)
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
