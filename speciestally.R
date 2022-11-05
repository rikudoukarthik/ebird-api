library (curl)
# Get an eBird token and assign it to myEBirdToken
source ("token.R")

getObserverCount <- function (region, day, month, year)
{
  date <- paste0(year,"/",month,"/",day)
  h <- new_handle()
  
  #myEBirdToken should be assigned in token.R
  handle_setheaders(h,
                    "X-eBirdApiToken" = myEBirdToken)
  req <- curl_fetch_memory(paste0("https://api.ebird.org/v2/product/stats/", region,"/",date),h)
  
  Sys.sleep(0.2)     
  return (req)
}
  
writeObserverTally <- function (region)
{  
  print (paste(region, day, month, year))
  req <- getObserverCount (region, day, month, year)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- fromJSON(response, flatten=FALSE)
  }
  else
  {
    print ("Observer Count returned error")
  }
  regionStat <- cbind (region, parsed$numChecklists, parsed$numSpecies, parsed$numContributors)
  print(nrow(regionStat))
  print(regionStat)
  return (regionStat)
}


getSpeciesList <- function (region, day, month, year)
{
  date <- paste0(year,"/",month,"/",day)
  h <- new_handle()

  #myEBirdToken should be assigned in token.R
  handle_setheaders(h,
                    "X-eBirdApiToken" = myEBirdToken)
  req <- curl_fetch_memory(paste0("https://api.ebird.org/v2/data/obs/", region,"/historic/",date),h)
  
  Sys.sleep(0.2)     
  return (req)
}

writeSpeciesTally <- function (region)
{  
  print (region)
  req <- getSpeciesList (region, day, month, year)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- fromJSON(response, flatten=FALSE)
  }
  speciesList <- cbind (parsed$comName)
  return (speciesList)
}

day = 12
month = 9
year = 2022

regions = c ("IN-KL")

#regions = c ("IN-JK", "IN-LA", "IN-HP", "IN-UL", "NP", "IN-SK", "IN-WB-DA", "IN-WB-KA", "IN-WB-JA", "IN-WB-AL", "BT", "IN-AR")

combinedSpecies <- sapply (regions, writeSpeciesTally) %>% unlist() %>% as.data.frame() %>% unique()
colnames(combinedSpecies) <- c("English.name")

ebd <- read.csv2 ("eBirdTaxonomy.csv", sep=",")
ebd$sort.v2021 <- as.numeric(as.character(ebd$sort.v2021))

combinedSpecies <- left_join(combinedSpecies, ebd) 

combinedSpecies <- combinedSpecies [order (combinedSpecies$sort.v2021), ] [1]

#write.csv2 (combinedSpecies, "HBC 2022 Species List.csv", row.names = FALSE)
write.csv2 (combinedSpecies, "OBC 2022 Species List.csv", row.names = FALSE)

participationSummary <- sapply (regions, writeObserverTally) %>% as.data.frame() %>% t()
colnames(participationSummary) <- c("eBird-Region", "Checklists", "Species", "Observers")

write.csv2(participationSummary, "OBC Participation.csv", row.names = FALSE)