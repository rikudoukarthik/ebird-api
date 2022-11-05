
get_obs_count <- function(region, day, month, year)
{
  date <- paste(year, month, day, sep = "/")
  h <- new_handle()
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(paste("https://api.ebird.org/v2/product/stats", region, date, 
                                 sep = "/"), 
                           h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_obs_tally <- function(region)
{  
  print(paste(region, day, month, year))
  req <- get_obs_count(region, day, month, year)
  
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


get_spec_list <- function(region, day, month, year)
{
  date <- paste(year, month, day, sep = "/")
  h <- new_handle()
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(paste("https://api.ebird.org/v2/data/obs", region,"historic", date,
                                 sep = "/"),
                           h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_spec_tally <- function(region)
{  
  print(region)
  req <- get_spec_list(region, day, month, year)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  speciesList <- cbind(parsed$comName)
  
  return(speciesList)
}