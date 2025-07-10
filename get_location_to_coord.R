library(storr)
library(httr2)

###this code is linked with "urlcache folder", url=unique resource location, cache is a place where you store information to find later
# in cache, we store useuful info to look up later!!
# you have the name of your information 
## this makes the process A LOT faster than dowloading from the website because you already have this info stored!! so save a lot of time!!


get_location_to_coord <- function (locations) {
  ## For each unique URL in the combined file, ask the gov.uk website
  ## for the longitude/latitude value
  ## Store in a local cache to avoid repeatedly asking the website for
  ## the same information
  
  st <- storr::storr_rds("./urlcache")
  
  for (l in locations) {
    if (! st$exists(l)) {
      print(sprintf("No cached location for %s, retrieving", l))
      
      url <- paste("http://environment.data.gov.uk/water-quality/id/sampling-point/", l, sep="")
      req <- request(url)
      
      resp <- req_perform(req)
      js <- resp |> resp_body_json()
      
      longitude <- js$items[[1]]$long
      latitude <- js$items[[1]]$lat
      area <- js$items[[1]]$area$`@id`
      subarea <- js$items[[1]]$subArea$`@id`
      # Record the extra details in the disk cache, even if not using them for now
      # (can the disk cache be used directly instead of writing this out again?)
      coord <- c(longitude, latitude, area, subarea)
      
      st$set(l, coord)
    }
  }
  
  st
}
