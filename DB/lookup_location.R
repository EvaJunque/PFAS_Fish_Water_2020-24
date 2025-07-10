## the lat/long location of the sampling points is available as json at a http endpoint
## example of retrieving that for a fixed string, optimistically assuming everything works
## and the json format is consistent (which seems likely, the rest of this interface has been
## nicely structured)


library(httr2)

req <- request("http://environment.data.gov.uk/water-quality/id/sampling-point/AN-22M18A")

resp <- req_perform(req)

js <- resp |> resp_body_json()

coord <- c(js$items[[1]]$long, js$items[[1]]$lat)

print(coord)
